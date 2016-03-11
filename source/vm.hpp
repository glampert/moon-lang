
// ================================================================================================
// -*- C++ -*-
// File: vm.hpp
// Author: Guilherme R. Lampert
// Created on: 06/07/15
// Brief: The Virtual Machine class.
// ================================================================================================

#ifndef MOON_VM_HPP
#define MOON_VM_HPP

#include "opcodes.hpp"
#include "runtime.hpp"

#include <iostream>
#include <utility>
#include <vector>

namespace moon
{

// ========================================================
// class VM:
// ========================================================

class VM final
{
public:

    static constexpr int DefaultStackSize = 8192; // Initial stack size in Variants.
    using DataVector = std::vector<Variant>;      // Global program data.
    using CodeVector = std::vector<Instruction>;  // Instructions / program code.

    VM(int stackSize = DefaultStackSize);
    VM(DataVector && data, CodeVector && code, int stackSize = DefaultStackSize);

    // Not copyable.
    VM(const VM &) = delete;
    VM & operator = (const VM &) = delete;

    void resetProgram(DataVector && data, CodeVector && code);
    void executeProgram();

private:

    void setProgCounter(int target);
    void executeSingleInstruction(OpCode op, std::uint32_t operandIndex);

    // The "Program Counter" (index of the next
    // instruction to be executed from progCode).
    int pc;

    // Program context:
    Stack         progStack;
    DataVector    progData;
    CodeVector    progCode;
    FunctionTable progFunctions;
};

// Debug printing:
void printDataVector(const VM::DataVector & progData, std::ostream & os = std::cout);
void printCodeVector(const VM::CodeVector & progCode, std::ostream & os = std::cout);

} // namespace moon {}

#endif // MOON_VM_HPP
