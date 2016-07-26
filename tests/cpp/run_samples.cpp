
// ================================================================================================
// -*- C++ -*-
// File: run_samples.cpp
// Author: Guilherme R. Lampert
// Created on: 27/06/16
// Brief: This test spins up a Compiler and VM for each of the sample scripts and runs them.
// ================================================================================================

#include "compiler.hpp"
#include "vm.hpp"

static void runScript(const std::string & scriptFile)
{
    using namespace moon;

    Compiler compiler; // Script parsing and bytecode output.
    VM       vm;       // Executes bytecode produced by a Compiler.

    try
    {
        compiler.parseScript(&vm, scriptFile);
        compiler.compile(&vm);
        vm.execute();
    }
    catch (...)
    {
        #if MOON_SAVE_SCRIPT_CALLSTACK
        if (!vm.callstack.isEmpty())
        {
            vm.printStackTrace(logStream());
        }
        #endif // MOON_SAVE_SCRIPT_CALLSTACK

        logStream() << color::red() << "terminating script \"" << scriptFile
                    << "\" with error(s).\n" << color::restore();
    }
}

int main()
{
    const std::string path = "tests/script/";
    const std::string scripts[]
    {
        path + "callstack.ml",
        path + "cpp_call.ml",
        path + "enums.ml",
        path + "fibonacci.ml",
        path + "functions.ml",
        path + "gc.ml",
        path + "globals.ml",
        path + "if-else.ml",
        path + "imports.ml",
        path + "loops.ml",
        path + "match.ml",
        path + "native_types.ml",
        path + "structs.ml",
        path + "test-import-1.ml",
        path + "test-import-2.ml",
        path + "unused-expr.ml",
    };

    for (const auto & s : scripts)
    {
        runScript(s);
    }
}
