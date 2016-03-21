
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

    // Default program stack size in Variants.
    static constexpr int DefaultStackSize = 8192;

    // Helper types:
    using DataVector = std::vector<Variant>;
    using CodeVector = std::vector<Instruction>;

    // Program context:
    Stack         stack;
    DataVector    data;
    CodeVector    code;
    FunctionTable functions;

    //
    // VM interface:
    //

    // loadBuiltIns: Load built-in native functions and helper modules.
    // stackSize:    Size in Variants of the program stack. Size is fixed.
    VM(bool loadBuiltIns = true, int stackSize = DefaultStackSize);

    // Not copyable.
    VM(const VM &) = delete;
    VM & operator = (const VM &) = delete;

    void setProgramCounter(int target) noexcept;
    int  getProgramCounter() const noexcept;

    void execute();
    void executeSingleInstruction(OpCode op, std::uint32_t operandIndex);

    // Prints the current Program counter, data vector and stack.
    void print(std::ostream & os = std::cout) const;

private:

    // The "Program Counter" (index of the next
    // instruction to be executed from the code vector).
    int pc;
};

// ========================================================
// Debug printing helpers:
// ========================================================

void printDataVector(const VM::DataVector & data, std::ostream & os = std::cout);
void printCodeVector(const VM::CodeVector & code, std::ostream & os = std::cout);

inline std::ostream & operator << (std::ostream & os, const VM::DataVector & data)
{
    printDataVector(data, os);
    return os;
}
inline std::ostream & operator << (std::ostream & os, const VM::CodeVector & code)
{
    printCodeVector(code, os);
    return os;
}
inline std::ostream & operator << (std::ostream & os, const VM & vm)
{
    vm.print(os);
    return os;
}

} // namespace moon {}

#endif // MOON_VM_HPP
