
// ================================================================================================
// -*- C++ -*-
// File: builtins.cpp
// Author: Guilherme R. Lampert
// Created on: 20/03/16
// Brief: Built-in native functions and runtime types.
// ================================================================================================

#include "vm.hpp"

// ========================================================
// Local aux macros:
// ========================================================

#define ADD_FUNC(funcTable, retType, funcName, flags, ...)                             \
    do                                                                                 \
    {                                                                                  \
        const Variant::Type argTypes[]{ __VA_ARGS__ };                                 \
        (funcTable).addFunction(#funcName, (retType), argTypes, arrayLength(argTypes), \
                                Function::TargetNative, (flags), &native::funcName);   \
    } while (0)

#define ADD_VARARGS_FUNC(funcTable, retType, funcName, flags) \
    (funcTable).addFunction(#funcName, (retType), nullptr, 0, \
                            Function::TargetNative, (flags), &native::funcName)

// Clashes with our native::assert() function.
#undef assert

// ========================================================

namespace moon
{
namespace native
{

// ========================================================
// Miscellaneous printing and error reporting functions:
// ========================================================

[[noreturn]] static void scriptError(const Int64 lineNum,
                                     const char * srcFileName,
                                     const char * callerName,
                                     const char * colorCode,
                                     const char * baseMessage,
                                     const std::string & userMessage) noexcept(false)
{
    std::string message;
    message += colorCode;
    message += baseMessage;
    message += srcFileName;
    message += "(" + toString(lineNum) + ") ";
    message += "at ";
    message += callerName;
    message += color::restore();

    if (!userMessage.empty())
    {
        message += " - ";
        message += userMessage;
    }

    // Throws ScriptException.
    moon::scriptError(message);
}

void assert(VM &, Stack::Slice args) // (varargs) -> void
{
    // First three arguments will always be the source location
    // in filename, line num and caller function order.
    const auto fileName = args.first();
    const auto lineNum  = args.next();
    const auto funcName = args.next();

    // Fourth arg is the assert condition.
    // If non-null/non-zero we pass the assertion.
    const auto cond = args.next();
    if (cond && cond->toBool())
    {
        return;
    }

    // If it is a function name we will get a Function variant.
    // If calling from the global scope, it is a string with "<global scope>"
    const char * callerName = ((funcName->type == Variant::Type::Function) ?
                                funcName->getAsFunction()->name->chars :
                                funcName->getAsString()->c_str());

    // Additional user-provided error message strings:
    std::string userMessage;
    for (auto var = args.next(); var != nullptr; var = args.next())
    {
        userMessage += toString(*var);
    }

    scriptError(lineNum->getAsInteger(), fileName->getAsString()->c_str(),
                callerName, color::yellow(), "script assert() failed: ", userMessage);
}

void panic(VM &, Stack::Slice args) // (varargs) -> void
{
    // Same layout of the above assert() function.
    const auto fileName = args.first();
    const auto lineNum  = args.next();
    const auto funcName = args.next();

    // Additional user-provided error message strings:
    std::string userMessage;
    for (auto var = args.next(); var != nullptr; var = args.next())
    {
        userMessage += toString(*var);
    }

    const char * callerName = ((funcName->type == Variant::Type::Function) ?
                                funcName->getAsFunction()->name->chars :
                                funcName->getAsString()->c_str());

    scriptError(lineNum->getAsInteger(), fileName->getAsString()->c_str(),
                callerName, color::red(), "script panic(): ", userMessage);
}

void print(VM &, Stack::Slice args) // (varargs) -> void
{
    for (auto var = args.first(); var != nullptr; var = args.next())
    {
        logStream() << toString(*var);
    }
}

void println(VM &, Stack::Slice args) // (varargs) -> void
{
    for (auto var = args.first(); var != nullptr; var = args.next())
    {
        logStream() << toString(*var);
    }
    logStream() << "\n";
}

void to_string(VM & vm, Stack::Slice args) // (varargs) -> string
{
    if (args.getSize() < 1)
    {
        MOON_RUNTIME_EXCEPTION("to_string() requires at least one argument");
    }

    // Second argument is the optional formatting flag.
    const auto var = args.first();
    const auto fmt = args.next();
    std::string outputStr;

    if (fmt != nullptr)
    {
        if (fmt->type != Variant::Type::Str || fmt->value.asString == nullptr)
        {
            MOON_RUNTIME_EXCEPTION("to_string() format flag should be a string, "
                                   "e.g.: \"b\", \"f\", \"h\", \"p\", etc");
        }

        // Print with specific format:
        const char * flag = fmt->value.asString->c_str();
        switch (*flag)
        {
        case 'b' : // boolean
            outputStr = toString(var->toBool());
            // Anything but zero/null will be true, including negative numbers.
            break;

        case 'c' : // character
            if (var->type == Variant::Type::Integer)
            {
                outputStr = strPrintF("%c", static_cast<char>(var->value.asInteger));
            }
            else
            {
                outputStr = "?";
            }
            break;

        case 'f' : // float
            if (var->type == Variant::Type::Float)
            {
                outputStr = toString(var->value.asFloat);
            }
            else if (var->type == Variant::Type::Integer)
            {
                outputStr = toString(var->value.asInteger);
            }
            else // Not a number
            {
                outputStr = "nan";
            }
            break;

        case 'l' : // long or
        case 'i' : // integer
            if (var->type == Variant::Type::Float)
            {
                // Floats will get truncated to integer.
                outputStr = toString(static_cast<Int64>(var->value.asFloat));
            }
            else if (var->type == Variant::Type::Integer)
            {
                outputStr = toString(var->value.asInteger);
            }
            else // Not a number
            {
                outputStr = "nan";
            }
            break;

        case 'h' : // hexadecimal
            outputStr = strPrintF("0x%016llX", static_cast<UInt64>(var->value.asInteger));
            break;

        case 'p' : // pointer
            if (var->type == Variant::Type::Integer || var->type == Variant::Type::Float)
            {
                // Types stored directly into the Variant can print the Variant's own address.
                outputStr = strPrintF("0x%016llX", static_cast<UInt64>(
                                      reinterpret_cast<std::uintptr_t>(var)));
            }
            else // Objects print the address of the object:
            {
                outputStr = strPrintF("0x%016llX", static_cast<UInt64>(
                                      reinterpret_cast<std::uintptr_t>(var->value.asVoidPtr)));
            }
            break;

        case 't' : // type tag
            outputStr = toString(var->type);
            break;

        case 'o' : // object (all members recursive)
            if (var->type == Variant::Type::Object && var->value.asObject != nullptr)
            {
                outputStr = var->value.asObject->getStringRepresentation();
            }
            else // Not an object. Print as if no flag given.
            {
                outputStr = toString(*var);
            }
            break;

        default :
            MOON_RUNTIME_EXCEPTION("to_string() called with unrecognized format flag '" + toString(flag) + "'");
        } // switch (*flag)
    }
    else
    {
        // Just make the input into a string using
        // default formatting for its type:
        outputStr = toString(*var);
    }

    Variant result{ Variant::Type::Str };
    result.value.asString = Str::newFromString(vm, outputStr, /* makeConst = */ false);
    vm.setReturnValue(result);
}

// ========================================================
// Miscellaneous built-ins:
// ========================================================

//TODO other nice things to have:
//
// - a small strings library (can be based on std::string)
// - a small array handling library (can be based on std::vector)
// - a small dictionary library (std::unordered_map based)
// - expose the C-maths library
// - small rand gen library
// - a library for bit manipulation within integers? We don't have the traditional bitshift ops...
// - env library to get things like cmdline and env vars
// - parse_int/parse_float functions (str=>int) conversion
// - console input function

} // namespace native {}

// ========================================================
// registerNativeBuiltInFunctions():
// ========================================================

void registerNativeBuiltInFunctions(FunctionTable & funcTable)
{
    const auto retString = Variant::Type::Str;

    ADD_VARARGS_FUNC( funcTable, nullptr,    assert,    Function::VarArgs | Function::AddCallerInfo | Function::DebugOnly );
    ADD_VARARGS_FUNC( funcTable, nullptr,    panic,     Function::VarArgs | Function::AddCallerInfo );
    ADD_VARARGS_FUNC( funcTable, nullptr,    print,     Function::VarArgs );
    ADD_VARARGS_FUNC( funcTable, nullptr,    println,   Function::VarArgs );
    ADD_VARARGS_FUNC( funcTable, &retString, to_string, Function::VarArgs );
}

#undef ADD_VARARGS_FUNC
#undef ADD_FUNC

} // namespace moon {}
