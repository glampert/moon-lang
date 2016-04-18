
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

//TODO provide more descriptive comments on each op
enum class OpCode : UInt8
{
    NoOp = 0,

    // Nothing right now. In the future we might
    // use them for global initialization/shutdown.
    ProgStart,
    ProgEnd,

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

    Typeof, // pops 1, pushes a tid
    Typecast, // pops 2, pushes the result

    // Call a script function.
    Call,

    // creates a new typed Variant, pushes into the stack
    // assumes the previous load is the argument count, e.g.:
    // 0=uninitialized var; 1=pops one value for the initializer
    NewVar,

    // allocate new range object from the previous 2 values in the stack.
    // Pushes the result.
    NewRange,

    // Allocates a new array from previous stack values.
    // Value immediately before is the number of elements in the array.
    // Pushes the result.
    NewArray,

    // allocate & call constructor, pushes result into the stack
    NewObj,

    // FuncStart has the name of the function as its operand
    FuncStart,
    FuncEnd,

    // operand is the name of the iterator
    ForLoopPrep,
    ForLoopTest,
    ForLoopStep,

    MatchPrep, // pops one value
    MatchTest, // pops one value, pushes the result of the test

    ArraySubscript, // pops the array ref and subscript from the stack[array_ref, sub]
                    // pushes the resulting value into the stack

    MemberRef, // pops 2 from the stack, [obj, member_idx] and pushes the resulting member var

    // Return Value Register
    LoadRVR,  // push RVR into stack
    StoreRVR, // store stack top into RVR

    // global variable/constant
    LoadGlob,  // push operand into stack
    StoreGlob, // store stack top into operand and pop
    MemberStoreGlob, // pops 2 from the stack, [obj, member_idx] and writes to member variable

    // local variable (function scope)
    LoadLocal,
    StoreLocal,
    MemberStoreLocal,

    CmpNotEqual,
    CmpEqual,
    CmpGreaterEqual,
    CmpGreater,
    CmpLessEqual,
    CmpLess,

    LogicOr,
    LogicAnd,

    Sub,
    Add,
    Mod,
    Div,
    Mul,

    // Unary ops:
    LogicNot,
    Negate,
    Plus,

    // Number of op-codes. Internal use.
    Count
};

// Opcodes are packed inside an integer for the VM representation, with
// only 8 bits allocated for the opcode part, so the max instruction number
// must also fit in a byte. Hence the name "bytecode" sometimes used.
static_assert(UInt32(OpCode::Count) <= 255, "Too many opcodes! Value must fit in a byte!");

// OpCode to printable string. No color coding added.
std::string toString(OpCode op);

inline bool isJumpOpCode(const OpCode op) noexcept
{
    return op == OpCode::Jmp        ||
           op == OpCode::JmpIfFalse ||
           op == OpCode::JmpIfTrue  ||
           op == OpCode::JmpReturn;
}

inline bool referencesStackData(const OpCode op) noexcept
{
    return op == OpCode::LoadLocal  ||
           op == OpCode::StoreLocal ||
           op == OpCode::MemberStoreLocal;
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
// For an instruction that takes no operands, such as a compare
// instruction, then the operand index will be zero.
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
using Instruction = UInt32;

// Pack/unpack into integer:
inline Instruction packInstruction(const OpCode op, const UInt32 operandIndex) noexcept
{
    return (static_cast<UInt32>(op) << 24) | (operandIndex & 0x00FFFFFF);
}

inline void unpackInstruction(const Instruction instr, OpCode & op, UInt32 & operandIndex) noexcept
{
    op = static_cast<OpCode>((instr & 0xFF000000) >> 24); // 8
    operandIndex = (instr & 0x00FFFFFF);                  // 24
}

} // namespace moon {}

#endif // MOON_OPCODES_HPP
