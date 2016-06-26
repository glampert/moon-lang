
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

enum class OpCode : UInt8
{
    // Do nothing for a cycle.
    NoOp = 0,

    // Nothing right now. In the future we might
    // use them for global initialization/shutdown.
    ProgStart,

    // Sets the PC to one instruction past the end, terminating a program.
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

    // Jumps to the end of a function (currently no difference from a regular jump).
    JmpReturn,

    Typeof,   // Pops 1 value, pushes a TypeId.
    Typecast, // Pops 2 value, target TypeId and value. Pushes the result of the cast.

    // Call a script function.
    // Operand is the global Function object.
    // Pops the function arguments from the VM stack upon completion.
    Call,

    // Indirect call (via function pointer) from local function parameter or var.
    // Operand pointers to the local stack.
    CallLocal,

    // Creates a new typed Variant, pushes the result into the stack.
    // Assumes the top of the stack is the argument count, e.g.:
    // 0=uninitialized var; 1=pops one more value for the initializer.
    NewVar,

    // Allocate new range object from the previous 2 values in the stack.
    // Pushes the result.
    NewRange,

    // Allocates a new array from previous stack values.
    // Value immediately before is the number of elements in the array.
    // Pushes the result.
    NewArray,

    // Allocate & call constructor, pushes the resulting object into the stack.
    NewObj,

    // Expects the stack top to be the enum type id.
    // Operand is the enum instance to be initialized.
    EnumDynInit,

    // FuncStart/FuncEnd has the name of the function as its operand.
    // The return address is pushed/pop from the VM stack.
    FuncStart,
    FuncEnd,

    // Pops 3 values (parameter, iterator, index) and pushes the
    // updated iterator and index back (2 values).
    ForLoopPrep,

    // Pops the loop condition and current index, pushes
    // true if the loop is NOT done, false if finished.
    ForLoopTest,

    // Pops 3 values (parameter, iterator, index) and pushes the
    // updated iterator and index back (2 values).
    ForLoopStep,

    // Pops the test value, performs the test and puts it back into
    // the stack along with the result of the test.
    MatchTest,

    // Pops the test value from the VM stack.
    MatchEnd,

    // Pops the array_ptr and subscript from the stack [array, sub]
    // and pushes the resulting value into the stack.
    ArraySubscript,

    // Pops 2 values from the stack, [obj, member_idx]
    // and pushes the resulting member var.
    MemberRef,

    // Return Value Register:
    LoadRVR,  // Push the RVR into the stack.
    StoreRVR, // Store the stack top into the RVR.

    // Global variables/constants:
    LoadGlobal,  // Push operand into stack.
    StoreGlobal, // Store stack top into operand and pop.

    // Resolves a chain of member references. Operand is in the prog data.
    MemberStoreGlobal,

    // Local variables (function scope):
    // Both will access the VM::locals and the VM::stack.
    // The operand index points to the locals stack.
    LoadLocal,
    StoreLocal,

    // Resolves a chain of member references. Operand is in the locals stack.
    MemberStoreLocal,

    // Pops the value and subscript, write to the array operand.
    StoreArraySubLocal,
    StoreArraySubGlobal,

    // Pops 3 values from stack [array_ptr, value, subscript]
    // and writes to the array. Pushes nothing back.
    StoreArraySubStk,

    // Same as StoreLocal/StoreGlobal, but overrides the type of
    // the operand instead of assigning with conversion/checking.
    StoreSetTypeLocal,
    StoreSetTypeGlobal,

    // Binary operators:
    // Two values popped from the stack and result pushed.
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

    // Unary operators:
    // Pop one value from the stack and push the result.
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

// ========================================================
// VM instruction representation:
// ========================================================

// Each instruction is a 32-bits unsigned integer.
//
// The top byte is the opcode. The next 24-bits are the index
// to its operand in the 'prog data' or the index to its jump
// target in the 'prog code' if it is a jump or call instruction.
//
// For an instruction that takes no operands, such as a compare
// instruction, then the operand index will be zero.
//
// A store instruction will write the top of the VM stack
// to its operand index. Jump instructions will set the PC
// from its operand.
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
