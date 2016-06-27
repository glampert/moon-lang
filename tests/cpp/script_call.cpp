
// ================================================================================================
// -*- C++ -*-
// File: script_call.cpp
// Author: Guilherme R. Lampert
// Created on: 27/06/16
// Brief: Sample C++ code calling script functions by name.
// ================================================================================================

#include "compiler.hpp"
#include "vm.hpp"

int main()
{
    using namespace moon;

    Compiler compiler; // Script parsing and bytecode generation.
    VM       vm;       // Executes the bytecode generation by a Compiler.

    try
    {
        compiler.parseScript(&vm, "tests/script/cpp_call.ml");
        compiler.compile(&vm);
        vm.execute();

        Variant retVal;
        Variant arg0, arg1, arg2;

        logStream() << "C++: Calling script functions...\n\n";

        // Void function that returns void:
        {
            retVal = vm.call("script_func0");
            MOON_ASSERT(retVal.isNull());
        }

        // Function taking 1 argument:
        {
            arg0.type = Variant::Type::Integer;
            arg0.value.asInteger = 1337;

            retVal = vm.call("script_func1", { arg0 });
            MOON_ASSERT(retVal.type == Variant::Type::Integer && retVal.value.asInteger == 321);
        }

        // Function taking 2 arguments:
        {
            arg0.type = Variant::Type::Integer;
            arg0.value.asInteger = 42;

            arg1.type = Variant::Type::Float;
            arg1.value.asFloat = 2.7;

            retVal = vm.call("script_func2", { arg0, arg1 });
            MOON_ASSERT(retVal.type == Variant::Type::Float && retVal.value.asFloat == -1.5);
        }

        // Function taking 3 arguments:
        {
            arg0.type = Variant::Type::Integer;
            arg0.value.asInteger = 33;

            arg1.type = Variant::Type::Float;
            arg1.value.asFloat = -0.5;

            arg2.type = Variant::Type::Str;
            const char * s = "hello from C++";
            arg2.value.asString = Str::newFromString(vm, s, std::strlen(s), true);

            retVal = vm.call("script_func3", { arg0, arg1, arg2 });
            MOON_ASSERT(retVal.type == Variant::Type::Str && std::strcmp("testing, 123", retVal.value.asString->c_str()) == 0);
        }

        // Varargs function:
        {
            arg0.type = Variant::Type::Integer;
            arg0.value.asInteger = 666;

            arg1.type = Variant::Type::Range;
            arg1.value.asRange.begin = -5;
            arg1.value.asRange.end   = 10;

            arg2.type = Variant::Type::Str;
            const char * s = "hello from C++, again";
            arg2.value.asString = Str::newFromString(vm, s, std::strlen(s), true);

            retVal = vm.call("script_func_varargs", { arg0, arg1, arg2 });
            MOON_ASSERT(retVal.isNull()); // Returns nothing.
        }

        // Call a native function by name:
        {
            arg0.type = Variant::Type::Str;
            const char * s = "native function called from C++; ";
            arg0.value.asString = Str::newFromString(vm, s, std::strlen(s), true);

            arg1.type = Variant::Type::Float;
            arg1.value.asFloat = 2.345;

            retVal = vm.call("println", { arg0, arg1 });
            MOON_ASSERT(retVal.isNull());
        }
    }
    catch (...)
    {
        #if MOON_SAVE_SCRIPT_CALLSTACK
        if (!vm.callstack.isEmpty())
        {
            vm.printStackTrace(logStream());
        }
        #endif // MOON_SAVE_SCRIPT_CALLSTACK

        logStream() << color::red() << "terminating with error(s)...\n" << color::restore();
        return EXIT_FAILURE;
    }
}
