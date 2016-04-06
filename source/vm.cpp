
// ================================================================================================
// -*- C++ -*-
// File: vm.cpp
// Author: Guilherme R. Lampert
// Created on: 06/07/15
// Brief: The Virtual Machine and execution model.
// ================================================================================================

#include "vm.hpp"
#include <iomanip> // For std::setw & friends

namespace moon
{

// ========================================================
// Opcode handlers:
// ========================================================

// Function signature of an opcode handler. See the array of handlers below.
using OpCodeHandlerCB = void (*)(VM & vm, UInt32 operandIndex);

static void opNOOP(VM &, UInt32)
{
    // Surprisingly, a no-op does nothing :P
}

static void opProgEnd(VM & vm, UInt32)
{
    // Set the program count to the end of the code vector to terminate
    // execution, even if there was more code after this instruction.
    vm.setProgramCounter(static_cast<int>(vm.code.size()));
}

static void opJump(VM & vm, const UInt32 operandIndex)
{
    vm.setProgramCounter(operandIndex);
}

static void opJumpIfTrue(VM & vm, const UInt32 operandIndex)
{
    if (!vm.stack.pop().isZero())
    {
        vm.setProgramCounter(operandIndex);
    }
}

static void opJumpIfFalse(VM & vm, const UInt32 operandIndex)
{
    if (vm.stack.pop().isZero())
    {
        vm.setProgramCounter(operandIndex);
    }
}

static void opLoadRVR(VM & vm, UInt32)
{
    vm.stack.push(vm.getReturnValue());
}

static void opStoreRVR(VM & vm, UInt32)
{
    vm.setReturnValue(vm.stack.pop());
}

static void opLoadGlob(VM & vm, const UInt32 operandIndex)
{
    // Push integer operand to the VM stack.
    vm.stack.push(vm.data[operandIndex]);
}

static void opStoreGlob(VM & vm, const UInt32 operandIndex)
{
    // Stores the current stack top to the data index of
    // this instruction, then pop the VM stack.
    performAssignmentWithConversion(vm.data[operandIndex], vm.stack.pop());
}

static void opLoadLocal(VM & vm, const UInt32 operandIndex)
{
    const auto argc = vm.locals.getTopVar().getAsInteger();
    const auto args = vm.locals.slice(vm.locals.getCurrSize() - argc - 1, argc);
    vm.stack.push(args[operandIndex]);
}

static void opStoreLocal(VM & vm, const UInt32 operandIndex)
{
    const auto argc = vm.locals.getTopVar().getAsInteger();
    auto args = vm.locals.slice(vm.locals.getCurrSize() - argc - 1, argc);
    performAssignmentWithConversion(args[operandIndex], vm.stack.pop());
}

static void opMemberStoreCommon(VM & vm, Variant objRef)
{
    const auto memberCount = vm.stack.pop().getAsInteger();
    const auto offsets = vm.stack.slice(vm.stack.getCurrSize() - memberCount, memberCount);
    vm.stack.popN(memberCount);

    const Variant storeValue = vm.stack.pop();
    Object::Member * memberRef = nullptr;

    // We have a list of member offsets in the stack, so
    // we start from the head object and keep going down
    // until the last member reference that is the target
    // of this store instruction.
    for (int o = 0; o < offsets.getSize(); ++o)
    {
        memberRef = &objRef.getAsObject()->getMemberAt(offsets[o].getAsInteger());
        objRef = memberRef->data;
    }

    MOON_ASSERT(memberRef != nullptr);
    performAssignmentWithConversion(memberRef->data, storeValue);
}

static void opMemberStoreGlob(VM & vm, const UInt32 operandIndex)
{
    // Source the target object from the global program data.
    opMemberStoreCommon(vm, vm.data[operandIndex]);
}

static void opMemberStoreLocal(VM & vm, const UInt32 operandIndex)
{
    // Source the target object from the function-local stack.
    const auto argc = vm.locals.getTopVar().getAsInteger();
    auto args = vm.locals.slice(vm.locals.getCurrSize() - argc - 1, argc);
    opMemberStoreCommon(vm, args[operandIndex]);
}

static void opCall(VM & vm, const UInt32 operandIndex)
{
    const Variant argcVar = vm.stack.getTopVar();
    vm.funcArgs = vm.stack.slice(vm.stack.getCurrSize() - argcVar.getAsInteger() - 1, argcVar.getAsInteger() + 1);

    const Variant funcVar = vm.data[operandIndex];
    vm.stack.pop(); // the argCount

    if (funcVar.type != Variant::Type::Function)
    {
        MOON_RUNTIME_EXCEPTION("opCall: expected a function object!");
    }
    if (funcVar.value.asFunction == nullptr)
    {
        MOON_RUNTIME_EXCEPTION("attempting to call a null function object!");
    }
    if (argcVar.type != Variant::Type::Integer)
    {
        MOON_RUNTIME_EXCEPTION("function arg count sentry should be an integer!");
    }

    const auto argc = argcVar.value.asInteger;
    const auto args = vm.stack.slice(vm.stack.getCurrSize() - argc, argc);
    vm.stack.popN(argc);

    funcVar.value.asFunction->invoke(vm, args);
}

static void opFuncStart(VM & vm, UInt32)
{
    // Copy from the main work stack to the function-local stack (including the argCount):
    for (int i = 0; i < vm.funcArgs.getSize(); ++i)
    {
        vm.locals.push(vm.funcArgs[i]);
    }

    // Save so we can return to the caller.
    Variant retAddr{ Variant::Type::Integer };
    retAddr.value.asInteger = vm.getReturnAddress();
    vm.stack.push(retAddr);
}

static void opFuncEnd(VM & vm, const UInt32 operandIndex)
{
    const auto argc = vm.locals.getTopVar().getAsInteger();
    vm.locals.popN(argc + 1); // +1 to also pop the argCount

    // Pop the return address pushed by FuncStart and jump to it.
    const Variant retAddr = vm.stack.pop();
    vm.setProgramCounter(retAddr.getAsInteger());

    // We can double check this here, but the compiler should have
    // already validated the return type against the return statements.
    const Variant funcVar = vm.data[operandIndex];
    funcVar.value.asFunction->validateReturnValue(vm.getReturnValue());
}

static void opNewVar(VM & vm, const UInt32 operandIndex)
{
    const Variant tid = vm.data[operandIndex];
    Variant initVal{ variantTypeFromTypeId(tid.getAsTypeId()) };

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

static void opNewObj(VM & vm, const UInt32 operandIndex)
{
    const Variant tid = vm.data[operandIndex];
    if (tid.type != Variant::Type::Tid || tid.value.asTypeId == nullptr)
    {
        MOON_RUNTIME_EXCEPTION("opNewObj: expected a type id!");
    }

    const Variant constructorArgCount = vm.stack.pop();
    const auto argc = constructorArgCount.getAsInteger();
    const auto args = vm.stack.slice(vm.stack.getCurrSize() - argc, argc);

    Variant newObj{ Variant::Type::Object };
    newObj.value.asObject = newRuntimeObject(vm, tid.value.asTypeId, args);

    vm.stack.popN(argc);
    vm.stack.push(newObj);
}

static void opMemberRef(VM & vm, UInt32)
{
    // Pop the object and its member index from
    // the stack and push the member var back:
    const Variant memberIdx = vm.stack.pop();
    const Variant objRef    = vm.stack.pop();

    const auto member = objRef.getAsObject()->getMemberAt(memberIdx.getAsInteger());
    vm.stack.push(member.data);
}

template<OpCode OP>
static void opUnary(VM & vm, UInt32)
{
    vm.stack.push(performUnaryOp(OP, vm.stack.pop()));
}

template<OpCode OP>
static void opBinary(VM & vm, UInt32)
{
    const Variant operandB = vm.stack.pop();
    const Variant operandA = vm.stack.pop();
    vm.stack.push(performBinaryOp(OP, operandA, operandB));
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
    &opNewObj,                          // NewObj
    &opFuncStart,                       // FuncStart
    &opFuncEnd,                         // FuncEnd
    &opNOOP,                            // ForLoopPrep
    &opNOOP,                            // ForLoopTest
    &opNOOP,                            // ForLoopStep
    &opNOOP,                            // MatchPrep
    &opNOOP,                            // MatchTest
    &opNOOP,                            // ArraySubscript
    &opMemberRef,                       // MemberRef
    &opLoadRVR,                         // LoadRVR
    &opStoreRVR,                        // StoreRVR
    &opLoadGlob,                        // LoadGlob
    &opStoreGlob,                       // StoreGlob
    &opMemberStoreGlob,                 // MemberStoreGlob
    &opLoadLocal,                       // LoadLocal
    &opStoreLocal,                      // StoreLocal
    &opMemberStoreLocal,                // MemberStoreLocal
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
    &opUnary<OpCode::LogicNot>,         // LogicNot
    &opUnary<OpCode::Negate>,           // Negate
    &opUnary<OpCode::Plus>              // Plus
};
static_assert(arrayLength(opHandlerCallbacks) == UInt32(OpCode::Count),
              "Keep this array in sync with the enum declaration!");

// ========================================================
// VM class methods:
// ========================================================

VM::VM(const bool loadBuiltIns, const int stackSize)
    : stack   { stackSize }
    , locals  { stackSize }
    , types   { *this     }
    , pc      { 0         }
    , retAddr { 0         }
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
        UInt32 operandIndex;
        unpackInstruction(code[pc], op, operandIndex);
        executeSingleInstruction(op, operandIndex);
    }

    // If a stack is left dirty at the end of a program, there's probably a bug somewhere.
    MOON_ASSERT(stack.isEmpty()  && "VM work stack should be empty at the end of a program!");
    MOON_ASSERT(locals.isEmpty() && "Function stack should be empty at the end of a program!");
}

void VM::executeSingleInstruction(const OpCode op, const UInt32 operandIndex)
{
    const auto handlerIndex  = static_cast<UInt32>(op);
    MOON_ASSERT(handlerIndex < static_cast<UInt32>(OpCode::Count));
    return opHandlerCallbacks[handlerIndex](*this, operandIndex);
}

// ========================================================
// Debug printing helpers:
// ========================================================

static void dumpVariant(const Variant var, const int index, std::ostream & os)
{
    auto valStr  = toString(var);
    auto typeStr = toString(var.type);

    if (var.type == Variant::Type::Str)
    {
        valStr = unescapeString(valStr.c_str());
    }

    os << strPrintF("%s[ %3i ]%s %s0x%016llX%s (%s%s%s) => %s\n",
                    color::cyan(), index, color::restore(), color::yellow(),
                    static_cast<UInt64>(var.value.asInteger), color::restore(),
                    color::red(), valStr.c_str(), color::restore(), typeStr.c_str());
}

void printCodeVector(const VM::CodeVector & code, std::ostream & os)
{
    OpCode op = OpCode::NoOp;
    UInt32 operandIndex = 0;
    UInt32 instrIndex   = 0;

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

std::string toString(const OpCode op)
{
    // OpCode to printable string (no colors added)
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
