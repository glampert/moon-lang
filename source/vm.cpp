
// ================================================================================================
// -*- C++ -*-
// File: vm.cpp
// Author: Guilherme R. Lampert
// Created on: 06/07/15
// Brief: The Virtual Machine class.
// ================================================================================================

#include "vm.hpp"

#include <functional> // std::plus, std::minus, std::divides, etc...
#include <iomanip>    // For std::setw & friends

namespace moon
{

// ========================================================
// FIXME test/temp stuff

template<template<typename> class OP>
void binaryOp(Stack & stack)
{
    OP<LangLong> op;

    auto operandB = stack.pop();
    MOON_ASSERT(operandB.type == Variant::Type::Integer);

    auto operandA = stack.pop();
    MOON_ASSERT(operandA.type == Variant::Type::Integer);

    Variant result;
    result.type = Variant::Type::Integer;
    result.value.asInteger = op(operandA.value.asInteger, operandB.value.asInteger);

    stack.push(result);
}

void Native_Print(VM & vm, Stack::Slice args)
{
    (void)vm;

    std::string str;
    for (auto var = args.first(); var != nullptr; var = args.next())
    {
        str += " ";
        str += toString(*var);
    }
    std::cout << "Native_Print: " << str << std::endl;
}

// ========================================================
// VM class methods:
// ========================================================

VM::VM(const int stackSize)
    : stack { stackSize }
    , pc    { 0 }
{
    //FIXME TEST
    functions.addFunction("print", nullptr, 0, Function::TargetNative, &Native_Print);
}

void VM::setProgramCounter(const int target) noexcept
{
    MOON_ASSERT(target < code.size());
    pc = target - 1;
    //
    // The -1 is necessary because the 'for' loop
    // in () will still increment the pc after
    // executeSingleInstruction() returns.
    //
    // This is arguably a hack. Perhaps it should
    // be implemented in a more clear way...
    //
}

int VM::getProgramCounter() const noexcept
{
    return pc;
}

void VM::execute()
{
    const int instructionCount = static_cast<int>(code.size());
    for (pc = 0; pc < instructionCount; ++pc)
    {
        OpCode op;
        std::uint32_t operandIndex;
        unpackInstruction(code[pc], op, operandIndex);
        executeSingleInstruction(op, operandIndex);
    }

    // If the stack is left dirty at the end of a program, there's probably a bug somewhere.
    MOON_ASSERT(stack.isEmpty() && "Stack should be empty at the end of a program!");
}

void VM::executeSingleInstruction(const OpCode op, const std::uint32_t operandIndex)
{
    //TODO
    (void)op;
    (void)operandIndex;
#if 0
    switch (op)
    {
    //
    // Misc instructions / program control:
    //
    case OpCode::NoOp :
    case OpCode::ModuleStart :
        break;//TODO

    //
    // Jump instructions:
    //
    case OpCode::Jmp :
        setProgramCounter(operandIndex);
        break;

    case OpCode::JmpIfTrue :
        if (stack.pop().value.asInteger != 0)
        {
            setProgramCounter(operandIndex);
        }
        break;

    case OpCode::JmpIfFalse :
        if (stack.pop().value.asInteger == 0)
        {
            setProgramCounter(operandIndex);
        }
        break;

    //
    // Function call:
    //
    case OpCode::Call :
    case OpCode::CallNative :
        {
            const auto funcName = data[operandIndex];
            if (funcName.type != Variant::Type::String)
            {
                MOON_RUNTIME_EXCEPTION("function name not a string!");
            }

            auto func = functions.findFunction(funcName.value.asStringPtr);
            if (func == nullptr)
            {
                MOON_RUNTIME_EXCEPTION("attempting to call undefined function: '" + toString(funcName) + "()'");
            }

            const auto argCount = stack.pop();
            if (argCount.type != Variant::Type::Integer)
            {
                MOON_RUNTIME_EXCEPTION("function arg count sentry should be an integer!");
            }

            const auto n = argCount.value.asInteger;
            (*func)(*this, stack.slice(stack.getCurrSize() - n, n));
            stack.popN(n);
        }
        break;

    //
    // Integer instructions:
    //
    case OpCode::IntNew :
        //TODO
        break;

    case OpCode::IntLoad :
        // Push integer operand to the VM stack.
        stack.push(data[operandIndex]);
        break;

    case OpCode::IntStore :
        // Stores the current stack top to the data index of
        // this instruction, then pop the VM stack.
        data[operandIndex] = stack.pop();
        break;

    //
    // Integer comparisons:
    //
    case OpCode::IntCmpNotEq        : intBinaryOp< std::not_equal_to  >(stack); break;
    case OpCode::IntCmpEq           : intBinaryOp< std::equal_to      >(stack); break;
    case OpCode::IntCmpGreaterEqual : intBinaryOp< std::greater_equal >(stack); break;
    case OpCode::IntCmpGreater      : intBinaryOp< std::greater       >(stack); break;
    case OpCode::IntCmpLessEqual    : intBinaryOp< std::less_equal    >(stack); break;
    case OpCode::IntCmpLess         : intBinaryOp< std::less          >(stack); break;

    //
    // Integer arithmetics:
    //
    case OpCode::IntSub : intBinaryOp< std::minus      >(stack); break;
    case OpCode::IntAdd : intBinaryOp< std::plus       >(stack); break;
    case OpCode::IntMod : intBinaryOp< std::modulus    >(stack); break;
    case OpCode::IntDiv : intBinaryOp< std::divides    >(stack); break; // TODO check for divide by zero! (maybe use an overloaded template!)
    case OpCode::IntMul : intBinaryOp< std::multiplies >(stack); break;

    default :
        //TODO
        break;
    } // switch (op)
#endif //0
}

// ========================================================
// Debug printing helpers:
// ========================================================

static void dumpVariant(const Variant var, const int index, std::ostream & os)
{
    os << color::cyan() << "[ " << std::setw(3) << std::setfill(' ') << index << " ] "
       << color::yellow() << "0x" << std::hex << std::setw(16) << std::setfill('0')
       << reinterpret_cast<std::uintptr_t>(var.value.asVoidPtr) << color::restore() << " ("
       << color::red() << std::dec << toString(var) << color::restore() << ") => "
       << toString(var.type) << "\n";
}

void printCodeVector(const VM::CodeVector & code, std::ostream & os)
{
    OpCode op = OpCode::NoOp;
    std::uint32_t operandIndex = 0;
    std::uint32_t instrIndex   = 0;

    os << color::white() << "[[ begin code vector dump ]]" << color::restore() << "\n";

    for (auto instr : code)
    {
        unpackInstruction(instr, op, operandIndex);

        os << color::cyan() << "[ " << std::setw(3) << instrIndex << " ] "
           << color::red() << toString(op) << color::restore() << " (operand=" << operandIndex << ")\n";

        ++instrIndex;
    }

    os << color::white() << "[[ listed " << code.size() << " instructions ]]" << color::restore() << "\n";
}

void printDataVector(const VM::DataVector & data, std::ostream & os)
{
    os << color::white() << "[[ begin data vector dump ]]" << color::restore() << "\n";

    int index = 0;
    for (auto var : data)
    {
        dumpVariant(var, index++, os);
    }

    os << color::white() << "[[ printed " << data.size() << " variants ]]" << color::restore() << "\n";
}

void VM::print(std::ostream & os) const
{
    os << color::white() << "[[ ---- VM state dump ---- ]]" << color::restore() << "\n";
    os << color::red() << "PC = " << color::restore() << pc << "\n";

    os << "\n";
    printDataVector(data, os);
    os << "\n";

    os << color::white() << "[[ VM stack ]]" << color::restore() << "\n";
    if (!stack.isEmpty())
    {
        int index = 0;
        auto s = stack.slice(0, stack.getCurrSize());
        for (auto var = s.first(); var != nullptr; var = s.next())
        {
            dumpVariant(*var, index++, os);
        }
    }
    os << color::white() << "[[ printed " << stack.getCurrSize() << " variants ]]" << color::restore() << "\n";
    os << color::white() << "\n[[ ----------------------- ]]" << color::restore() << "\n";
}

} // namespace moon {}
