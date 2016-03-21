
// ================================================================================================
// -*- C++ -*-
// File: builtins.cpp
// Author: Guilherme R. Lampert
// Created on: 20/03/16
// Brief: Built-in native functions and runtime types.
// ================================================================================================

#include "vm.hpp"
#include <cstdarg>

// ========================================================
// Local aux macros:
// ========================================================

#define ADD_FUNC(funcTable, funcName, ...)                                \
    do {                                                                  \
        const Variant::Type argTypes[]{ __VA_ARGS__ };                    \
        funcTable.addFunction(#funcName, argTypes, arrayLength(argTypes), \
                              Function::TargetNative, &native::funcName); \
    } while (0)

#define ADD_VARARGS_FUNC(funcTable, funcName) \
    funcTable.addFunction(#funcName, nullptr, 0, Function::TargetNative, &native::funcName)

// Clashes with our native::assert()
#undef assert

// ========================================================

namespace moon
{
namespace native
{

static std::string strPrintF(const char * format, ...)
{
// Suppress "format string is not a string literal" on GCC and Clang.
#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wformat-nonliteral"
#endif // __GNUC__

    va_list vaArgs;
    char buffer[1024];

    va_start(vaArgs, format);
    std::vsnprintf(buffer, sizeof(buffer), format, vaArgs);
    va_end(vaArgs);

    buffer[sizeof(buffer) - 1] = '\0';
    return buffer;

#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif // __GNUC__
}

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

    std::cerr << message << "\n";
    throw ScriptException{ message };
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

    std::cerr << message << "\n";
    throw ScriptException{ message };
}

void print(VM &, Stack::Slice args) // (varargs) -> void
{
    for (auto var = args.first(); var; var = args.next())
    {
        std::cout << toString(*var);
    }
}

void println(VM &, Stack::Slice args) // (varargs) -> void
{
    for (auto var = args.first(); var; var = args.next())
    {
        std::cout << toString(*var);
    }
    std::cout << "\n";
}

void to_string(VM & vm, Stack::Slice args) // (varargs) -> string
{
    if (args.getSize() < 1)
    {
        MOON_RUNTIME_EXCEPTION("to_string() requires at least one argument");
    }

    // Second argument is the formatting flag. It is optional.
    auto var = args.first();
    auto fmt = args.next();
    if (!fmt)
    {
        vm.stack.push(*var); // Return value
        return;
    }

    // Print with specific format:
    if (fmt->type != Variant::Type::String)
    {
        MOON_RUNTIME_EXCEPTION("to_string() format flag should be a string, e.g.: \"b\", \"h\", \"p\", etc");
    }

    std::string formatted;
    const auto flag = fmt->value.asStringPtr;
    switch (*flag)
    {
    case 'b' : // boolean
        formatted = toString(var->toBool());
        break;

    case 'f' : // float
        if (var->type == Variant::Type::Float)
        {
            formatted = trimTrailingFloatZeros(strPrintF("%f", var->value.asFloat));
        }
        else if (var->type == Variant::Type::Integer)
        {
            formatted = strPrintF("%i", var->value.asInteger);
        }
        else
        {
            formatted = "#NAN";
        }
        break;

    case 'h' : // hexadecimal
    case 'p' : // or pointer
        formatted = strPrintF("0x%016llX", static_cast<std::uint64_t>(var->value.asInteger));
        break;

    default :
        MOON_RUNTIME_EXCEPTION("to_string() called with unrecognized format flag '" + toString(flag) + "'");
    } // switch (*flag)

    //FIXME temp: str is never freed for now...
    auto newStr = new char[formatted.length() + 1];
    std::strcpy(newStr, formatted.c_str());

    Variant result{ Variant::Type::String };
    result.value.asStringPtr = newStr;
    vm.stack.push(result);
}

} // namespace native {}

// ========================================================
// registerNativeBuiltInFunctions():
// ========================================================

void registerNativeBuiltInFunctions(FunctionTable & funcTable)
{
    ADD_VARARGS_FUNC( funcTable, assert    );
    ADD_VARARGS_FUNC( funcTable, panic     );
    ADD_VARARGS_FUNC( funcTable, print     );
    ADD_VARARGS_FUNC( funcTable, println   );
    ADD_VARARGS_FUNC( funcTable, to_string );
}

} // namespace moon {}
