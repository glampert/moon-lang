
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

void assert(VM &, Stack::Slice args) // (varargs) -> void
{
    // First var must be the boolean result of the assert expression.
    // Expression as string should follow, then source file/line info.
    // Lastly, optional user-provided error message.
    auto var = args.first();
    if (var && var->toBool())
    {
        return;
    }

    std::string message;
    message += color::yellow();
    message += "script assert() failed: ";
    message += color::restore();

    for (var = args.next(); var; var = args.next())
    {
        message += toString(*var);
    }

    scriptError(message);
}

void panic(VM &, Stack::Slice args) // (varargs) -> void
{
    std::string message;
    message += color::red();
    message += "script panic(): ";
    message += color::restore();

    for (auto var = args.first(); var; var = args.next())
    {
        message += toString(*var);
    }

    scriptError(message);
}

void print(VM &, Stack::Slice args) // (varargs) -> void
{
    for (auto var = args.first(); var; var = args.next())
    {
        logStream() << toString(*var);
    }
}

void println(VM &, Stack::Slice args) // (varargs) -> void
{
    for (auto var = args.first(); var; var = args.next())
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
    auto var = args.first();
    auto fmt = args.next();
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

//TODO other nice things to have:
//
// - a small strings library (can be based on std::string)
// - a small array handling library (can be based on std::vector)
// - a small dictionary library (std::unordered_map based)
// - expose the C-maths library
// - small rand gen library
// - a library for bit manipulation within integers? We don't have the traditional bitshift ops...
// - env library to get things like cmdline and env vars

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
