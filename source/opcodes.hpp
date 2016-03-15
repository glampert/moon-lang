
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

    // Nothing right now. In the future we might use it for global initialization.
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

    // Jumps to the end of a function taking with it the return value, if any.
    JmpReturn,

    // Call a script function.
    Call,

    // allocate & call constructor, pushes result into the stack
    NewObj,

    // creates a new typed Variant, pushes into the stack
    // assumes the previous load is the argument count, e.g.:
    // 0=uninitialized var; 1=pops one value for the initializer
    NewVar,

    // FuncStart has the name of the function as its operand
    FuncStart,
    FuncEnd,

    // operand is the name of the iterator
    ForLoopPrep,
    ForLoopTest,
    ForLoopStep,

    ArraySubscript, // pops the array ref and subscript from the stack[array_ref, sub]
                    // pushes the resulting value into the stack

    Load,  // push operand into stack
    Store, // store stack top into operand and pop

    CmpNotEqual,
    CmpEqual,
    CmpGreaterEqual,
    CmpGreater,
    CmpLessEqual,
    CmpLess,

    LogicOr,
    LogicAnd,
    LogicNot,

    Sub,
    Add,
    Mod,
    Div,
    Mul,

    SubAssign,
    AddAssign,
    ModAssign,
    DivAssign,
    MulAssign,

    Negate,
    Plus,

    // Number of op-codes. Internal use.
    Count
};

// Opcodes are packed inside an integer for the VM representation, with
// only 8 bits allocated for the opcode part, so the max instruction number
// must also fit in a byte. Hence the name "bytecode" sometimes used.
static_assert(unsigned(OpCode::Count) <= 255, "Too many opcodes! Value must fit in a byte!");

// Return a printable string for debug dumping or disassembly.
std::string toString(OpCode op);

// Shorthand helper:
inline bool isJumpInstruction(const OpCode op) noexcept
{
    return (op == OpCode::Jmp        ||
            op == OpCode::JmpIfFalse ||
            op == OpCode::JmpIfTrue  ||
            op == OpCode::JmpReturn);
}

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
