
// ================================================================================================
// -*- C++ -*-
// File: opcodes.hpp
// Author: Guilherme R. Lampert
// Created on: 10/03/16
// Brief: Virtual Machine op-codes/instructions.
// ================================================================================================

#ifndef MOON_OPCODES_HPP
#define MOON_OPCODES_HPP

#include "common.hpp"

namespace moon
{

// ========================================================
// OpCode constants:
// ========================================================

enum class OpCode : std::uint8_t
{
    NoOp = 0,
    ModuleStart,

    // Unconditional jump to target instruction.
    // Doesn't touch the VM stack.
    Jmp,

    // Jumps to target if the previous comparison yielded non-zero/true.
    // Pops 1 value from the VM stack.
    JmpIfTrue,

    // Jumps to target if the previous comparison yielded zero/false.
    // Pops 1 value from the VM stack.
    JmpIfFalse,

    // Calls a function. The callee pops arguments from the VM stack.
    Call,

    // Call external C function.
    CallNative,

    //FIXME
    // might not need multiple instruction types after all.
    // the VM always works in terms of Variants, so we probably won't need the typing at this level...
    //
    // Integer instructions and arithmetics:
    IntNew,             // let int a = ... / let a = <int expr> ...
    IntLoad,            // move from memory to VM stack (pushes 1 value)
    IntStore,           // a =  b write back from stack to memory (pops 1 value)

    IntCmpNotEq,        // a != b (pops 2, pushes the result)
    IntCmpEq,           // a == b (pops 2, pushes the result)
    IntCmpGreaterEqual, // a >= b (pops 2, pushes the result)
    IntCmpGreater,      // a >  b (pops 2, pushes the result)
    IntCmpLessEqual,    // a <= b (pops 2, pushes the result)
    IntCmpLess,         // a <  b (pops 2, pushes the result)

    IntSub,             // a -  b (pops 2, pushes the result)
    IntAdd,             // a +  b (pops 2, pushes the result)
    IntMod,             // a %  b (pops 2, pushes the result)
    IntDiv,             // a /  b (pops 2, pushes the result)
    IntMul,             // a *  b (pops 2, pushes the result)

    // Number of op-codes. Internal use.
    Count
};

// Opcodes are packed inside an integer for the VM representation, with
// only 8 bits allocated for the opcode part, so the max instruction number
// must also fit in a byte. Hence the name "bytecode" sometimes used.
static_assert(unsigned(OpCode::Count) <= 255, "Too many opcodes! Value must fit in a byte!");

inline bool isJumpInstruction(const OpCode op) noexcept
{
    return op == OpCode::Jmp || op == OpCode::JmpIfTrue || op == OpCode::JmpIfFalse;
}
inline bool isCallInstruction(const OpCode op) noexcept
{
    return op == OpCode::Call || op == OpCode::CallNative;
}

// Return a printable string for debug dumping or disassembly.
std::string toString(OpCode op);

// ========================================================
// VM instruction representation:
// ========================================================

// Each instruction is a 32-bits unsigned integer.
//
// The top byte is the opcode. The next 24-bits are the index
// to its operand in the `progData` or the index to its jump
// target in `progCode` if it is a jump or call instruction.
//
// For an instruction that takes no operands, such as
// a compare instruction, then the index will be zero.
//
// A store instruction will write the top of the `progStack`
// to its operand index.
//
// VM instruction word:
//  +-------------+--------------------------+
//  | op-code (8) |     operand-index (24)   |
//  +-------------+--------------------------+
// MSB                                      LSB

// 8 bits opcode, 24 bits data/jump index.
using Instruction = std::uint32_t;

// Pack unpack into integer:
inline Instruction packInstruction(const OpCode op, const std::uint32_t operandIndex) noexcept
{
    return (static_cast<std::uint32_t>(op) << 24) | (operandIndex & 0x00FFFFFF);
}
inline void unpackInstruction(const Instruction instr, OpCode & op, std::uint32_t & operandIndex) noexcept
{
    op = static_cast<OpCode>((instr & 0xFF000000) >> 24); // 8
    operandIndex = (instr & 0x00FFFFFF);                  // 24
}

} // namespace moon {}

#endif // MOON_OPCODES_HPP
