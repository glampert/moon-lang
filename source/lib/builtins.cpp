
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
    const Variant * fileName = args.first();
    const Variant * lineNum  = args.next();
    const Variant * funcName = args.next();
    MOON_ASSERT(fileName != nullptr && lineNum != nullptr && funcName != nullptr);

    // Fourth arg is the assert condition.
    // If non-null/non-zero we pass the assertion.
    const Variant * cond = args.next();
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
    for (const Variant * var = args.next(); var != nullptr; var = args.next())
    {
        userMessage += toString(*var);
    }

    scriptError(lineNum->getAsInteger(), fileName->getAsString()->c_str(),
                callerName, color::yellow(), "script assert() failed: ", userMessage);
}

void panic(VM &, Stack::Slice args) // (varargs) -> void
{
    // Same layout of the above assert() function.
    const Variant * fileName = args.first();
    const Variant * lineNum  = args.next();
    const Variant * funcName = args.next();
    MOON_ASSERT(fileName != nullptr && lineNum != nullptr && funcName != nullptr);

    // Additional user-provided error message strings:
    std::string userMessage;
    for (const Variant * var = args.next(); var != nullptr; var = args.next())
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
    for (const Variant * var = args.first(); var != nullptr; var = args.next())
    {
        logStream() << toString(*var);
    }
}

void println(VM &, Stack::Slice args) // (varargs) -> void
{
    for (const Variant * var = args.first(); var != nullptr; var = args.next())
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
    const Variant * var = args.first();
    const Variant * fmt = args.next();
    std::string outputStr;

    MOON_ASSERT(var != nullptr);
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
            {
                outputStr = toString(var->toBool());
                // Anything but zero/null will be true, including negative numbers.
                break;
            }
        case 'c' : // character
            {
                if (var->type == Variant::Type::Integer)
                {
                    outputStr = strPrintF("%c", static_cast<char>(var->value.asInteger));
                }
                else
                {
                    outputStr = "?";
                }
                break;
            }
        case 'f' : // float
            {
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
            }
        case 'l' : // long or
        case 'i' : // integer
            {
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
            }
        case 'h' : // hexadecimal
            {
                const UInt64 value = var->value.asInteger;
                outputStr = strPrintF(MOON_X64_PRINT_FMT, value);
                break;
            }
        case 'p' : // pointer
            {
                UInt64 address;
                if (var->type == Variant::Type::Integer ||
                    var->type == Variant::Type::Float)
                {
                    // Types stored directly into the Variant can print the Variant's own address.
                    address = static_cast<UInt64>(reinterpret_cast<std::uintptr_t>(var));
                    outputStr = strPrintF(MOON_X64_PRINT_FMT, address);
                }
                else // Objects print the address of the object:
                {
                    address = static_cast<UInt64>(reinterpret_cast<std::uintptr_t>(var->value.asVoidPtr));
                    outputStr = strPrintF(MOON_X64_PRINT_FMT, address);
                }
                break;
            }
        case 't' : // type tag
            {
                outputStr = toString(var->type);
                break;
            }
        case 'o' : // object (all members recursive)
            {
                if (var->type == Variant::Type::Object &&
                    var->value.asObject != nullptr)
                {
                    outputStr = var->value.asObject->getStringRepresentation();
                }
                else // Not an object. Print as if no flag given.
                {
                    outputStr = toString(*var);
                }
                break;
            }
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

void gc_need_to_collect(VM & vm, Stack::Slice) // () -> bool
{
    Variant result{ Variant::Type::Integer };
    result.value.asInteger = vm.gc.needToCollectGarbage();
    vm.setReturnValue(result);
}

void gc_collect(VM & vm, Stack::Slice) // () -> void
{
    vm.gc.collectGarbage(vm);
}

} // namespace native {}

// ========================================================
// registerNativeBuiltInFunctions():
// ========================================================

void registerNativeBuiltInFunctions(FunctionTable & funcTable)
{
    const auto retStr = Variant::Type::Str;
    const auto retInt = Variant::Type::Integer;

    ADD_VARARGS_FUNC( funcTable, nullptr, assert,     Function::VarArgs | Function::AddCallerInfo | Function::DebugOnly );
    ADD_VARARGS_FUNC( funcTable, nullptr, panic,      Function::VarArgs | Function::AddCallerInfo );
    ADD_VARARGS_FUNC( funcTable, nullptr, print,      Function::VarArgs );
    ADD_VARARGS_FUNC( funcTable, nullptr, println,    Function::VarArgs );
    ADD_VARARGS_FUNC( funcTable, &retStr, to_string,  Function::VarArgs );
    ADD_VARARGS_FUNC( funcTable, &retInt, gc_need_to_collect, 0 );
    ADD_VARARGS_FUNC( funcTable, nullptr, gc_collect,         0 );
}

#undef ADD_VARARGS_FUNC
#undef ADD_FUNC

// ========================================================
// TODO other libraries we should have at some point:
// ========================================================

// ----------------------------------------------
// - A small strings library:
//
// str_new("text" or len + "fill-val")
// str_length(str)
// str_set_length(str, len, [fill_char])
// str_push(str, "test")
// str_pop(str, num_chars)
// str_clear(str)
// str_find_first(str, "pattern")
// str_find_last(str, "pattern")
// str_equal_no_case(str1, str2)
// new_str = str_trim_new(str)
// str_trim_in_place(str)
// str_get_char(str, index)
// str_set_char(str, index)
//
// parse_int(str, [base])
// parse_float(str)
//
// ----------------------------------------------
// - A small array handling library:
//
// array_new(store_type_id, initial_size)
// array_push(arr, val)
// array_push(arr, other_array)
// array_pop(arr, [count])
// array_clear(arr)
// array_set_capacity(arr, n)
// array_set_length(arr, n, [fill_val])
// array_capacity(arr)
// array_length(arr)
// array_sort(arr)
// array_get_index(arr, index)
// array_set_index(arr, index)
//
// ----------------------------------------------
// - Additional ranges support:
//
// range_begin(r)
// range_end(r)
// range_length(r)
//
// ----------------------------------------------
// - Other useful stuff:
//
// - A small dictionary library (std::unordered_map based).
// - Expose the C-maths library.
// - Small random number gen library.
// - A library for bit manipulation within integers? We don't have the traditional bitshift ops in the language...
// - Env library to get things like cmdline and environment vars.
// - Console input functions.

} // namespace moon {}
