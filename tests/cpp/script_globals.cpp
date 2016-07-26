
// ================================================================================================
// -*- C++ -*-
// File: script_globals.cpp
// Author: Guilherme R. Lampert
// Created on: 27/06/16
// Brief: Sample showing how to manipulate global script variables from C++.
// ================================================================================================

#include "compiler.hpp"
#include "vm.hpp"

#if !defined(MOON_GLOBALS_TABLE) || (MOON_GLOBALS_TABLE == 0)
    #error "This sample requires MOON_GLOBALS_TABLE to be defined!"
#endif

int main()
{
    using namespace moon;

    Compiler compiler; // Script parsing and bytecode output.
    VM       vm;       // Executes bytecode produced by a Compiler.

    try
    {
        compiler.parseScript(&vm, "tests/script/globals.ml");
        compiler.compile(&vm);

        // Init the globals:
        logStream() << "C++: First run...\n";
        vm.execute();

        // Print the current values:
        logStream() << "C++: Calling print_globals()...\n";
        vm.call("print_globals");

        // Change them:
        logStream() << "C++: Editing globals vars...\n";
        {
            Variant var;

            var.type = Variant::Type::Integer;
            var.value.asInteger = 1337;
            MOON_ASSERT(vm.globals.setGlobal("an_integer", var) == true);

            var.type = Variant::Type::Float;
            var.value.asFloat = 3.141592;
            MOON_ASSERT(vm.globals.setGlobal("a_float", var) == true);

            var.type = Variant::Type::Range;
            var.value.asRange.begin = -5;
            var.value.asRange.end   = +5;
            MOON_ASSERT(vm.globals.setGlobal("a_range", var) == true);

            var.type = Variant::Type::Str;
            const char * s = "hello from C++";
            var.value.asString = Str::newFromString(vm, s, std::strlen(s), true);
            MOON_ASSERT(vm.globals.setGlobal("a_string", var) == true);
        }

        // Print the new values:
        logStream() << "C++: Calling print_globals() again...\n";
        vm.call("print_globals");
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
