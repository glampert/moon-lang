
// ================================================================================================
// -*- C++ -*-
// File: opcodes.cpp
// Author: Guilherme R. Lampert
// Created on: 10/03/16
// Brief: Virtual Machine op-codes/instructions.
// ================================================================================================

#include "opcodes.hpp"

namespace moon
{

// OpCode to printable string (no colors added)
std::string toString(const OpCode op)
{
    static const std::string opCodeNames[]
    {
        "NOOP",
        "PROG_START",
        "PROG_END",
        "JMP",
        "JMP_IF_TRUE",
        "JMP_IF_FALSE",
        "JMP_RETURN",
        "CALL",
        "NEW_VAR",
        "NEW_RANGE",
        "NEW_ARRAY",
        "NEW_OBJ",
        "FUNC_START",
        "FUNC_END",
        "FOR_LOOP_PREP",
        "FOR_LOOP_TEST",
        "FOR_LOOP_STEP",
        "MATCH_PREP",
        "MATCH_TEST",
        "ARRAY_SUBSCRIPT",
        "MEMBER_REF",
        "LOAD_RVR",
        "STORE_RVR",
        "LOAD_GLOB",
        "STORE_GLOB",
        "MEMBER_STORE_GLOB",
        "LOAD_LOCAL",
        "STORE_LOCAL",
        "MEMBER_STORE_LOCAL",
        "CMP_NOT_EQUAL",
        "CMP_EQUAL",
        "CMP_GREATER_EQUAL",
        "CMP_GREATER",
        "CMP_LESS_EQUAL",
        "CMP_LESS",
        "LOGIC_OR",
        "LOGIC_AND",
        "SUB",
        "ADD",
        "MOD",
        "DIV",
        "MUL",
        "LOGIC_NOT",
        "NEGATE",
        "PLUS"
    };
    static_assert(arrayLength(opCodeNames) == UInt32(OpCode::Count),
                  "Keep this array in sync with the enum declaration!");

    return opCodeNames[UInt32(op)];
}

} // namespace moon {}
