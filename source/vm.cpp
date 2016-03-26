
// ================================================================================================
// -*- C++ -*-
// File: vm.cpp
// Author: Guilherme R. Lampert
// Created on: 06/07/15
// Brief: The Virtual Machine class.
// ================================================================================================

#include "vm.hpp"
#include <iomanip> // For std::setw & friends

namespace moon
{

// ========================================================
// Opcode handlers:
// ========================================================

namespace
{

// Function signature of an opcode handler. See the array of handlers below.
using OpCodeHandlerCB = void (*)(VM & vm, std::uint32_t operandIndex);

void opNOOP(VM &, std::uint32_t)
{
    // Surprisingly, a no-op does nothing :P
}

void opProgEnd(VM & vm, std::uint32_t)
{
    // Set the program count to the end of the code vector to terminate
    // execution, even if there was more code after this instruction.
    vm.setProgramCounter(static_cast<int>(vm.code.size()));
}

void opJump(VM & vm, const std::uint32_t operandIndex)
{
    vm.setProgramCounter(operandIndex);
}

void opJumpIfTrue(VM & vm, const std::uint32_t operandIndex)
{
    if (!vm.stack.pop().isZero())
    {
        vm.setProgramCounter(operandIndex);
    }
}

void opJumpIfFalse(VM & vm, const std::uint32_t operandIndex)
{
    if (vm.stack.pop().isZero())
    {
        vm.setProgramCounter(operandIndex);
    }
}

void opLoadRVR(VM & vm, std::uint32_t)
{
    vm.stack.push(vm.getReturnValue());
}

void opStoreRVR(VM & vm, std::uint32_t)
{
    vm.setReturnValue(vm.stack.pop());
}

void opLoadGlob(VM & vm, const std::uint32_t operandIndex)
{
    // Push integer operand to the VM stack.
    vm.stack.push(vm.data[operandIndex]);
}

void opStoreGlob(VM & vm, const std::uint32_t operandIndex)
{
    // Stores the current stack top to the data index of
    // this instruction, then pop the VM stack.
    vm.data[operandIndex] = vm.stack.pop();
}

void opLoadLocal(VM & vm, const std::uint32_t operandIndex)
{
    const auto argc = vm.locals.getTopVar().value.asInteger;
    const auto args = vm.locals.slice(vm.locals.getCurrSize() - argc - 1, argc);
    vm.stack.push(args[operandIndex]);
}

void opStoreLocal(VM & vm, const std::uint32_t operandIndex)
{
    const auto argc = vm.locals.getTopVar().value.asInteger;
    auto args = vm.locals.slice(vm.locals.getCurrSize() - argc - 1, argc);
    args[operandIndex] = vm.stack.pop();
}

void opCall(VM & vm, const std::uint32_t operandIndex)
{
    const Variant argcVar = vm.stack.getTopVar();
    vm.funcArgv = vm.stack.slice(vm.stack.getCurrSize() - argcVar.value.asInteger - 1, argcVar.value.asInteger + 1);

    const Variant funcVar = vm.data[operandIndex];
    vm.stack.pop(); // the argCount

    if (funcVar.type != Variant::Type::Function)
    {
        MOON_RUNTIME_EXCEPTION("expected a function object!");
    }
    if (funcVar.value.asFunctionPtr == nullptr)
    {
        MOON_RUNTIME_EXCEPTION("attempting to call a null function object!");
    }
    if (argcVar.type != Variant::Type::Integer)
    {
        MOON_RUNTIME_EXCEPTION("function arg count sentry should be an integer!");
    }

    const auto argc = argcVar.value.asInteger;
    const auto argv = vm.stack.slice(vm.stack.getCurrSize() - argc, argc);
    vm.stack.popN(argc);

    funcVar.value.asFunctionPtr->invoke(vm, argv);
}

void opFuncStart(VM & vm, std::uint32_t)
{
    // Copy from the main work stack to the function-local stack (including the argCount):
    for (auto var = vm.funcArgv.first(); var; var = vm.funcArgv.next())
    {
        vm.locals.push(*var);
    }

    // Save so we can return to the caller.
    Variant retAddr{ Variant::Type::Integer };
    retAddr.value.asInteger = vm.getReturnAddress();
    vm.stack.push(retAddr);
}

void opFuncEnd(VM & vm, const std::uint32_t operandIndex)
{
    const auto argc = vm.locals.getTopVar().value.asInteger;
    vm.locals.popN(argc + 1); // +1 to also pop the argCount

    // Pop the return address pushed by FuncStart and jump to it.
    Variant retAddr = vm.stack.pop();
    MOON_ASSERT(retAddr.type == Variant::Type::Integer);
    vm.setProgramCounter(retAddr.value.asInteger);

    // We can double check this here, but the compiler should have
    // already validated the return type against the return statements.
    const Variant funcVar = vm.data[operandIndex];
    funcVar.value.asFunctionPtr->validateReturnValue(vm.getReturnValue());
}

void opNewVar(VM & vm, const std::uint32_t operandIndex)
{
    const Variant tid = vm.data[operandIndex];
    Variant initVal{ Variant::fromTypeId(tid.value.asStringPtr) };

    const Variant hasInitializer = vm.stack.pop();
    if (hasInitializer.toBool())
    {
        const Variant rhs = vm.stack.pop();
        performAssignmentWithConversion(initVal, rhs);
    }
    vm.stack.push(initVal);

    // Expand the function scope stack if we're newing from a function:
    if (!vm.locals.isEmpty())
    {
        auto argc = vm.locals.pop();
        argc.value.asInteger += 1;
        vm.locals.push(initVal);
        vm.locals.push(argc);
    }
}

template<OpCode OP>
void opUnary(VM & vm, std::uint32_t)
{
    vm.stack.push(performUnaryOp(OP, vm.stack.pop()));
}

template<OpCode OP>
void opBinary(VM & vm, std::uint32_t)
{
    const Variant operandB = vm.stack.pop();
    const Variant operandA = vm.stack.pop();
    vm.stack.push(performBinaryOp(OP, operandA, operandB));
}

template<OpCode OP>
void opBinaryStore(VM & vm, const std::uint32_t operandIndex)
{
    // Compound op+store operation.
    const Variant operandB = vm.stack.pop();
    const Variant operandA = vm.stack.pop();
    vm.data[operandIndex] = performBinaryOp(OP, operandA, operandB);
}

// ----------------------------------------------------------------------------
// opHandlerCallbacks[]:
//
// The handlers for each instruction opcode.
// These are called by the VM for each instruction
// in the program by VM::executeSingleInstruction().
// ----------------------------------------------------------------------------
static const OpCodeHandlerCB opHandlerCallbacks[]
{
    &opNOOP,                            // NoOp
    &opNOOP,                            // ProgStart
    &opProgEnd,                         // ProgEnd
    &opJump,                            // Jmp
    &opJumpIfTrue,                      // JmpIfTrue
    &opJumpIfFalse,                     // JmpIfFalse
    &opJump,                            // JmpReturn
    &opCall,                            // Call
    &opNewVar,                          // NewVar
    &opNOOP,                            // NewRange
    &opNOOP,                            // NewArray
    &opNOOP,                            // NewObj
    &opFuncStart,                       // FuncStart
    &opFuncEnd,                         // FuncEnd
    &opNOOP,                            // ForLoopPrep
    &opNOOP,                            // ForLoopTest
    &opNOOP,                            // ForLoopStep
    &opNOOP,                            // MatchPrep
    &opNOOP,                            // MatchTest
    &opNOOP,                            // ArraySubscript
    &opLoadRVR,                         // LoadRVR
    &opStoreRVR,                        // StoreRVR
    &opLoadGlob,                        // LoadGlob
    &opStoreGlob,                       // StoreGlob
    &opLoadLocal,                       // LoadLocal
    &opStoreLocal,                      // StoreLocal
    &opBinary<OpCode::CmpNotEqual>,     // CmpNotEqual
    &opBinary<OpCode::CmpEqual>,        // CmpEqual
    &opBinary<OpCode::CmpGreaterEqual>, // CmpGreaterEqual
    &opBinary<OpCode::CmpGreater>,      // CmpGreater
    &opBinary<OpCode::CmpLessEqual>,    // CmpLessEqual
    &opBinary<OpCode::CmpLess>,         // CmpLess
    &opBinary<OpCode::LogicOr>,         // LogicOr
    &opBinary<OpCode::LogicAnd>,        // LogicAnd
    &opBinary<OpCode::Sub>,             // Sub
    &opBinary<OpCode::Add>,             // Add
    &opBinary<OpCode::Mod>,             // Mod
    &opBinary<OpCode::Div>,             // Div
    &opBinary<OpCode::Mul>,             // Mul
    &opBinaryStore<OpCode::Sub>,        // SubStore
    &opBinaryStore<OpCode::Add>,        // AddStore
    &opBinaryStore<OpCode::Mod>,        // ModStore
    &opBinaryStore<OpCode::Div>,        // DivStore
    &opBinaryStore<OpCode::Mul>,        // MulStore
    &opUnary<OpCode::LogicNot>,         // LogicNot
    &opUnary<OpCode::Negate>,           // Negate
    &opUnary<OpCode::Plus>              // Plus
};
static_assert(arrayLength(opHandlerCallbacks) == unsigned(OpCode::Count),
              "Keep this array in sync with the enum declaration!");

} // namespace {}

// ========================================================
// VM class methods:
// ========================================================

VM::VM(const bool loadBuiltIns, const int stackSize)
    : stack   { stackSize }
    , locals  { stackSize }
    , pc      { 0 }
    , retAddr { 0 }
{
    if (loadBuiltIns)
    {
        registerNativeBuiltInFunctions(functions);
    }
}

void VM::setProgramCounter(const int target)
{
    if (target < 0 || target > static_cast<int>(code.size()))
    {
        MOON_RUNTIME_EXCEPTION("invalid instruction index: " + toString(target));
    }

    // The -1 is necessary because the 'for' loop
    // in execute() will still increment the pc after
    // executeSingleInstruction() returns.
    // This is arguably a hack. Perhaps it should
    // be implemented in a more clear way...
    pc = target - 1;
}

void VM::setReturnAddress(const int target)
{
    if (target < 0 || target > static_cast<int>(code.size()))
    {
        MOON_RUNTIME_EXCEPTION("invalid instruction index: " + toString(target));
    }

    retAddr = target;
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

    // If a stack is left dirty at the end of a program, there's probably a bug somewhere.
    MOON_ASSERT(stack.isEmpty()  && "VM work stack should be empty at the end of a program!");
    MOON_ASSERT(locals.isEmpty() && "Function stack should be empty at the end of a program!");
}

void VM::executeSingleInstruction(const OpCode op, const std::uint32_t operandIndex)
{
    const auto handlerIndex  = static_cast<unsigned>(op);
    MOON_ASSERT(handlerIndex < static_cast<unsigned>(OpCode::Count));
    return opHandlerCallbacks[handlerIndex](*this, operandIndex);
}

// ========================================================
// Debug printing helpers:
// ========================================================

static void dumpVariant(const Variant var, const int index, std::ostream & os)
{
    auto valStr = toString(var);
    if (var.type == Variant::Type::String)
    {
        valStr = unescapeString(valStr.c_str());
    }

    os << color::cyan() << "[ " << std::setw(3) << std::setfill(' ') << index << " ] "
       << color::yellow() << "0x" << std::hex << std::setw(16) << std::setfill('0')
       << static_cast<std::uint64_t>(var.value.asInteger) << color::restore() << " ("
       << color::red() << std::dec << valStr << color::restore() << ") => "
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
    os << color::red() << "PC       = " << color::restore() << getProgramCounter() << "\n";
    os << color::red() << "Ret Addr = " << color::restore() << getReturnAddress()  << "\n";

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
