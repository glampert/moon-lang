
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

template<template<typename> class OP>
void intBinaryOp(Stack & progStack)
{
    OP<LangLong> op;

    auto operandB = progStack.pop();
    MOON_ASSERT(operandB.type == Variant::Type::Integer);

    auto operandA = progStack.pop();
    MOON_ASSERT(operandA.type == Variant::Type::Integer);

    Variant result;
    result.type = Variant::Type::Integer;
    result.value.asInteger = op(operandA.value.asInteger, operandB.value.asInteger);

    progStack.push(result);
}

//TEST
void Native_Print(VM & vm, Stack::Slice args)
{
    (void)vm;

    std::string str;
    for (auto v = args.first(); v != nullptr; v = args.next())
    {
        str += " ";
        str += toString(*v);
    }
    std::cout << "Native_Print: " << str << std::endl;
}

// ========================================================
// VM class methods:
// ========================================================

VM::VM(const int stackSize)
    : pc        { 0 }
    , progStack { stackSize }
{ }

VM::VM(DataVector && data, CodeVector && code, const int stackSize)
    : pc        { 0 }
    , progStack { stackSize }
    , progData  { std::forward<DataVector>(data) }
    , progCode  { std::forward<CodeVector>(code) }
{
    //TEST
    {
        progFunctions.addFunction("print", nullptr, 0, Function::TargetNative, &Native_Print);
    }
}

void VM::setProgCounter(const int target)
{
    MOON_ASSERT(target < progCode.size());
    pc = target - 1;
    //
    // The -1 is necessary because the 'for' loop
    // in executeProgram() will still increment the
    // pc after executeSingleInstruction() returns.
    // This is arguably a hack. Perhaps it should
    // be implemented in a more clear way...
    //
}

void VM::resetProgram(DataVector && data, CodeVector && code)
{
    pc       = 0;
    progData = std::forward<DataVector>(data);
    progCode = std::forward<CodeVector>(code);
    progStack.clear();
    //TODO what about progFunctions?
}

void VM::executeProgram()
{
    const int instructionCount = static_cast<int>(progCode.size());

    for (pc = 0; pc < instructionCount; ++pc)
    {
        OpCode op;
        std::uint32_t operandIndex;
        unpackInstruction(progCode[pc], op, operandIndex);
        executeSingleInstruction(op, operandIndex);
    }

    /////FIXME temp
    printDataVector(progData);

    MOON_ASSERT(progStack.isEmpty());
}

void VM::executeSingleInstruction(const OpCode op, const std::uint32_t operandIndex)
{
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
        setProgCounter(operandIndex);
        break;

    case OpCode::JmpIfTrue :
        if (progStack.pop().value.asInteger != 0)
        {
            setProgCounter(operandIndex);
        }
        break;

    case OpCode::JmpIfFalse :
        if (progStack.pop().value.asInteger == 0)
        {
            setProgCounter(operandIndex);
        }
        break;

    //
    // Function call:
    //
    case OpCode::Call :
    case OpCode::CallNative :
        {
            const auto funcName = progData[operandIndex];
            if (funcName.type != Variant::Type::String)
            {
                MOON_RUNTIME_EXCEPTION("function name not a string!");
            }

            auto func = progFunctions.findFunction(funcName.value.asStringPtr);
            if (func == nullptr)
            {
                MOON_RUNTIME_EXCEPTION("attempting to call undefined function: '" + toString(funcName) + "()'");
            }

            const auto argCount = progStack.pop();
            if (argCount.type != Variant::Type::Integer)
            {
                MOON_RUNTIME_EXCEPTION("function arg count sentry should be an integer!");
            }

            const auto n = argCount.value.asInteger;
            (*func)(*this, progStack.slice(progStack.getCurrSize() - n, n));
            progStack.popN(n);
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
        progStack.push(progData[operandIndex]);
        break;

    case OpCode::IntStore :
        // Stores the current stack top to the data index of
        // this instruction, then pop the VM stack.
        progData[operandIndex] = progStack.pop();
        break;

    //
    // Integer comparisons:
    //
    case OpCode::IntCmpNotEq        : intBinaryOp< std::not_equal_to  >(progStack); break;
    case OpCode::IntCmpEq           : intBinaryOp< std::equal_to      >(progStack); break;
    case OpCode::IntCmpGreaterEqual : intBinaryOp< std::greater_equal >(progStack); break;
    case OpCode::IntCmpGreater      : intBinaryOp< std::greater       >(progStack); break;
    case OpCode::IntCmpLessEqual    : intBinaryOp< std::less_equal    >(progStack); break;
    case OpCode::IntCmpLess         : intBinaryOp< std::less          >(progStack); break;

    //
    // Integer arithmetics:
    //
    case OpCode::IntSub : intBinaryOp< std::minus      >(progStack); break;
    case OpCode::IntAdd : intBinaryOp< std::plus       >(progStack); break;
    case OpCode::IntMod : intBinaryOp< std::modulus    >(progStack); break;
    case OpCode::IntDiv : intBinaryOp< std::divides    >(progStack); break; // TODO check for divide by zero! (maybe use an overloaded template!)
    case OpCode::IntMul : intBinaryOp< std::multiplies >(progStack); break;

    default :
        //TODO
        break;
    } // switch (op)
}

// ========================================================
// Debug printing helpers:
// ========================================================

void printCodeVector(const VM::CodeVector & progCode, std::ostream & os)
{
    OpCode op = OpCode::NoOp;
    std::uint32_t operandIndex = 0;
    std::uint32_t instrIndex   = 0;

    os << color::white() << "[[ code vector dump ]]" << color::restore() << "\n";

    for (auto instr : progCode)
    {
        unpackInstruction(instr, op, operandIndex);

        os << color::cyan() << "[ " << std::setw(3) << instrIndex << " ] "
           << color::red() << toString(op) << color::restore() << " (operand=" << operandIndex << ")\n";

        ++instrIndex;
    }

    os << color::white() << "[[ listed " << progCode.size() << " instructions ]]" << color::restore() << "\n";
}

void printDataVector(const VM::DataVector & progData, std::ostream & os)
{
    os << color::white() << "[[ data vector dump ]]" << color::restore() << "\n";

    std::uint32_t instrIndex = 0;
    for (auto var : progData)
    {
        os << color::cyan() << "[ " << std::setw(3) << std::setfill(' ') << instrIndex << " ] "
           << color::yellow() << "0x" << std::hex << std::setw(16) << std::setfill('0')
           << reinterpret_cast<std::uintptr_t>(var.value.asVoidPtr) << color::restore() << " ("
           << color::red() << std::dec << toString(var) << color::restore() << ") => "
           << toString(var.type) << "\n";

        ++instrIndex;
    }

    os << color::white() << "[[ printed " << progData.size() << " variants ]]" << color::restore() << "\n";
}

} // namespace moon {}
