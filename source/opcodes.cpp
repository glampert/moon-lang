
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

// OpCode to printable string:
std::string toString(const OpCode op)
{
    static const std::string opCodeNames[]
    {
        "NOOP",
        "MODULE_START",
        "JMP",
        "JMP_IF_TRUE",
        "JMP_IF_FALSE",
        "CALL",
//        "CALL_NATIVE",

        "LOAD",
        "STORE",

        "CMP_NOT_EQUAL",
        "CMP_EQUAL",
        "CMP_GREATER_EQUAL",
        "CMP_GREATER",
        "CMP_LESS_EQUAL",
        "CMP_LESS",

        "LOGIC_OR",
        "LOGIC_AND",
        "LOGIC_NOT",

        "SUB",
        "ADD",
        "MOD",
        "DIV",
        "MUL",

        "SUB_ASSIGN",
        "ADD_ASSIGN",
        "MOD_ASSIGN",
        "DIV_ASSIGN",
        "MUL_ASSIGN",

        "NEGATE",
        "PLUS"

        /*FIXME tidy
        "INT_NEW",
        "INT_LOAD",
        "INT_STORE",
        "INT_CMP_NOT_EQ",
        "INT_CMP_EQ",
        "INT_CMP_GREATER_EQUAL",
        "INT_CMP_GREATER",
        "INT_CMP_LESS_EQUAL",
        "INT_CMP_LESS",
        "INT_SUB",
        "INT_ADD",
        "INT_MOD",
        "INT_DIV",
        "INT_MUL"
        */
    };
    static_assert(arrayLength(opCodeNames) == unsigned(OpCode::Count),
                  "Keep this array in sync with the enum declaration!");

    return opCodeNames[unsigned(op)];
}

} // namespace moon {}
