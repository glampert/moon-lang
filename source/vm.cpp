
// ================================================================================================
// -*- C++ -*-
// File: vm.cpp
// Author: Guilherme R. Lampert
// Created on: 06/07/15
// Brief: The Virtual Machine and execution model.
// ================================================================================================

#include "vm.hpp"
#include "semantic_check.hpp" // For some type conversion helpers
#include <iomanip>            // For std::setw & friends

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

static void opLoadGlobal(VM & vm, const UInt32 operandIndex)
{
    // Push integer operand to the VM stack.
    vm.stack.push(vm.data[operandIndex]);
}

static void opStoreGlobal(VM & vm, const UInt32 operandIndex)
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

    // Expand the local stack if needed:
    if (operandIndex >= argc)
    {
        Variant argcVar = vm.locals.pop();
        argcVar.value.asInteger += 1;

        vm.locals.push(vm.stack.pop());
        vm.locals.push(argcVar);
    }
    else // Assign to already allocated stack variable (function local or parameter):
    {
        auto args = vm.locals.slice(vm.locals.getCurrSize() - argc - 1, argc);
        performAssignmentWithConversion(args[operandIndex], vm.stack.pop());
    }
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

static void opMemberStoreGlobal(VM & vm, const UInt32 operandIndex)
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

static void opStoreArraySubLocal(VM & vm, const UInt32 operandIndex)
{
    const Variant subscriptVar   = vm.stack.pop();
    const Variant assignedValVar = vm.stack.pop();

    const auto argc = vm.locals.getTopVar().getAsInteger();
    auto args = vm.locals.slice(vm.locals.getCurrSize() - argc - 1, argc);

    Array * destArray = args[operandIndex].getAsArray();

    const int index = static_cast<int>(subscriptVar.getAsInteger());
    destArray->setIndex(index, assignedValVar);
}

static void opStoreArraySubGlobal(VM & vm, const UInt32 operandIndex)
{
    const Variant subscriptVar   = vm.stack.pop();
    const Variant assignedValVar = vm.stack.pop();

    Array * destArray = vm.data[operandIndex].getAsArray();

    const int index = static_cast<int>(subscriptVar.getAsInteger());
    destArray->setIndex(index, assignedValVar);
}

static void opStoreSetTypeLocal(VM & vm, const UInt32 operandIndex)
{
    const auto argc = vm.locals.getTopVar().getAsInteger();

    // Expand the local stack if needed:
    if (operandIndex >= argc)
    {
        Variant argcVar = vm.locals.pop();
        argcVar.value.asInteger += 1;

        vm.locals.push(vm.stack.pop());
        vm.locals.push(argcVar);
    }
    else // Assign to already allocated stack variable (function local or parameter):
    {
        auto args = vm.locals.slice(vm.locals.getCurrSize() - argc - 1, argc);
        args[operandIndex] = vm.stack.pop(); // Override the type instead of assigning with conversion.
    }
}

static void opStoreSetTypeGlobal(VM & vm, const UInt32 operandIndex)
{
    // Override the type instead of assigning with conversion.
    vm.data[operandIndex] = vm.stack.pop();
}

static void opCallCommon(VM & vm, const Variant funcVar)
{
    if (funcVar.type != Variant::Type::Function)
    {
        MOON_RUNTIME_EXCEPTION("opCall: expected a function object!");
    }
    if (funcVar.value.asFunction == nullptr)
    {
        MOON_RUNTIME_EXCEPTION("opCall: attempting to call a null function object!");
    }

    const Variant argcVar = vm.stack.getTopVar();
    const auto argc = argcVar.value.asInteger;

    if (argcVar.type != Variant::Type::Integer)
    {
        MOON_RUNTIME_EXCEPTION("opCall: function arg count sentry should be an integer!");
    }

    vm.funcArgs = vm.stack.slice(vm.stack.getCurrSize() - argc - 1, argc + 1);
    vm.stack.pop(); // the argCount

    const auto callArgs = vm.stack.slice(vm.stack.getCurrSize() - argc, argc);
    vm.stack.popN(argc);

    funcVar.value.asFunction->invoke(vm, callArgs);
}

static void opCall(VM & vm, const UInt32 operandIndex)
{
    // Source the function pointer from the global program data.
    opCallCommon(vm, vm.data[operandIndex]);
}

static void opCallLocal(VM & vm, const UInt32 operandIndex)
{
    // Source the function pointer from the local function stack.
    const auto argc = vm.locals.getTopVar().getAsInteger();
    const auto args = vm.locals.slice(vm.locals.getCurrSize() - argc - 1, argc);
    opCallCommon(vm, args[operandIndex]);
}

static void opFuncStart(VM & vm, const UInt32 operandIndex)
{
    const Variant funcVar = vm.data[operandIndex];

    // Turn the stack slice into an array for the varargs:
    if (funcVar.getAsFunction()->isVarArgs())
    {
        Stack::Slice arrayInitializers = vm.funcArgs;
        arrayInitializers.pop();

        // We don't known the types of each argument, so the array tag is 'Any'.
        Variant result{ Variant::Type::Array };
        result.value.asArray = Array::newFromArgs(vm, vm.types.anyTypeId, arrayInitializers);
        vm.locals.push(result);

        // Also push the argCount (1) so we can rollback at FuncEnd:
        Variant argc{ Variant::Type::Integer };
        argc.value.asInteger = 1;
        vm.locals.push(argc);
    }
    else // Copy from the main work stack to the function-local stack (including the argCount):
    {
        for (int i = 0; i < vm.funcArgs.getSize(); ++i)
        {
            vm.locals.push(vm.funcArgs[i]);
        }
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
    funcVar.getAsFunction()->validateReturnValue(vm.getReturnValue());
}

static void opNewVar(VM & vm, const UInt32 operandIndex)
{
    const Variant tidVar = vm.data[operandIndex];
    Variant initVal;

    if (tidVar.type == Variant::Type::Null)
    {
        initVal.type = Variant::Type::Null;
    }
    else
    {
        initVal.type = typeId2VarType(tidVar.getAsTypeId());
    }

    const Variant hasInitializer = vm.stack.pop();
    if (hasInitializer.toBool())
    {
        const Variant rhs = vm.stack.pop();
        performAssignmentWithConversion(initVal, rhs);
    }
    vm.stack.push(initVal);
}

static void opNewObj(VM & vm, const UInt32 operandIndex)
{
    const Variant tidVar = vm.data[operandIndex];
    if (tidVar.type != Variant::Type::Tid || tidVar.value.asTypeId == nullptr)
    {
        MOON_RUNTIME_EXCEPTION("opNewObj: expected a type id!");
    }

    const Variant constructorArgCount = vm.stack.pop();
    const auto argc = constructorArgCount.getAsInteger();
    const auto args = vm.stack.slice(vm.stack.getCurrSize() - argc, argc);

    Variant newObj{ Variant::Type::Object };
    newObj.value.asObject = newRuntimeObject(vm, tidVar.value.asTypeId, args);

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

static void opNewArray(VM & vm, UInt32)
{
    const auto elementCount = vm.stack.pop().getAsInteger();
    const auto arrayInitializers = vm.stack.slice(vm.stack.getCurrSize() - elementCount, elementCount);
    vm.stack.popN(elementCount);

    const Variant::Type underlayingType = arrayInitializers[0].type; // Type of first item dictates the storage type
    const TypeId * dataTypeId = varType2TypeId(vm.types, underlayingType);

    auto newArrayObj = Array::newFromArgs(vm, dataTypeId, arrayInitializers);

    Variant result{ Variant::Type::Array };
    result.value.asArray = newArrayObj;
    vm.stack.push(result);
}

static void opArraySubscript(VM & vm, UInt32)
{
    const int index = vm.stack.pop().getAsInteger();
    const Array * array = vm.stack.pop().getAsArray();
    vm.stack.push(array->getIndex(index));
}

static void opNewRange(VM & vm, UInt32)
{
    Range newRange;
    newRange.end   = vm.stack.pop().getAsInteger();
    newRange.begin = vm.stack.pop().getAsInteger();

    Variant result{ Variant::Type::Range };
    result.value.asRange = newRange;
    vm.stack.push(result);
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
    vm.stack.push(performBinaryOp(OP, operandA, operandB, &vm));
}

static void opTypeof(VM & vm, UInt32)
{
    // type_of() simply converts its operand Variant to a TypeId.
    // Note that for the 'Any' type we actually want to get the underlaying type, not "any".
    const Variant operand = vm.stack.pop();
    const Variant::Type varType = (operand.isAny() ? operand.anyType : operand.type);

    Variant tidVar{ Variant::Type::Tid };
    if (varType == Variant::Type::Object &&
        operand.value.asObject != nullptr)
    {
        // For objects we can get the precise type
        tidVar.value.asTypeId = operand.value.asObject->getTypeId();
    }
    else // Other types might get a generic tag
    {
        tidVar.value.asTypeId = varType2TypeId(vm.types, varType);
    }

    vm.stack.push(tidVar);
}

static void opTypecast(VM & vm, UInt32)
{
    const Variant targetTid = vm.stack.pop();
    const Variant operand   = vm.stack.pop();
    const Variant::Type targetVarType = typeId2VarType(targetTid.getAsTypeId());

    Variant destVar{ targetVarType };
    performAssignmentWithConversion(destVar, operand);
    vm.stack.push(destVar);
}

static void opForLoopPrep(VM & vm, UInt32)
{
    Variant forParam = vm.stack.pop();
    Variant forIter  = vm.stack.pop();
    Variant forIdx   = vm.stack.pop();

    if (forParam.type == Variant::Type::Range)
    {
        forIdx.value.asInteger  = forParam.value.asRange.begin;
        forIter.value.asInteger = forParam.value.asRange.begin;
    }
    else if (forParam.type == Variant::Type::Array)
    {
        forIdx.value.asInteger = 0;
        performAssignmentWithConversion(forIter, forParam.value.asArray->getIndex(0));
    }
    else
    {
        MOON_RUNTIME_EXCEPTION("opForLoopPrep: for loop param is not a range or array!");
    }

    vm.stack.push(forIter);
    vm.stack.push(forIdx);
    // Gets written back to the 'i' and '__for_idx__' vars
}

static void opForLoopTest(VM & vm, UInt32)
{
    Variant forParam = vm.stack.pop();
    Variant forIdx   = vm.stack.pop();
    Variant continueLooping{ Variant::Type::Integer };

    if (forParam.type == Variant::Type::Range)
    {
        continueLooping.value.asInteger = (forIdx.value.asInteger != forParam.value.asRange.end);
    }
    else if (forParam.type == Variant::Type::Array)
    {
        continueLooping.value.asInteger = (forIdx.value.asInteger != forParam.value.asArray->getArrayLength());
    }
    else
    {
        MOON_RUNTIME_EXCEPTION("opForLoopTest: for loop param is not a range or array!");
    }

    vm.stack.push(continueLooping);
}

static void opForLoopStep(VM & vm, UInt32)
{
    Variant forParam = vm.stack.pop();
    Variant forIter  = vm.stack.pop();
    Variant forIdx   = vm.stack.pop();

    forIdx.value.asInteger += 1;
    const int idx = forIdx.value.asInteger;

    if (forParam.type == Variant::Type::Range)
    {
        forIter.value.asInteger = idx;
    }
    else if (forParam.type == Variant::Type::Array)
    {
        if (idx < forParam.value.asArray->getArrayLength())
        {
            performAssignmentWithConversion(forIter, forParam.value.asArray->getIndex(idx));
        }
    }
    else
    {
        MOON_RUNTIME_EXCEPTION("opForLoopStep: for loop param is not a range or array!");
    }

    vm.stack.push(forIter);
    vm.stack.push(forIdx);
    // Gets written back to the 'i' and '__for_idx__' vars
}

static void opMatchTest(VM & vm, UInt32)
{
    const Variant caseVar = vm.stack.pop();
    const Variant testVar = vm.stack.pop();
    Variant result;

    if (caseVar.type == Variant::Type::Range && testVar.type != Variant::Type::Range)
    {
        Int32 test;
        const Range range = caseVar.getAsRange();

        if (testVar.type == Variant::Type::Integer)
        {
            test = static_cast<Int32>(testVar.getAsInteger());
        }
        else if (testVar.type == Variant::Type::Str)
        {
            if (testVar.value.asString->getStringLength() != 1)
            {
                MOON_RUNTIME_EXCEPTION("expected single character literal in match statement");
            }

            // Get the ASCII value of the character:
            test = *(testVar.value.asString->c_str());
        }
        else
        {
            MOON_RUNTIME_EXCEPTION("cannot compare range with " + toString(testVar.type));
        }

        result.type = Variant::Type::Integer;
        result.value.asInteger = (test >= range.begin && test < range.end);
    }
    else if (caseVar.type == Variant::Type::Array && testVar.type != Variant::Type::Array)
    {
        // Test each array element against the testVar.
        const Array * array = caseVar.getAsArray();
        const int count = array->getArrayLength();

        for (int i = 0; i < count; ++i)
        {
            const Variant arrayElement = array->getIndex(i);
            result = performBinaryOp(OpCode::CmpEqual, testVar, arrayElement, &vm);
            if (!result.isZero())
            {
                break;
            }
        }
    }
    else
    {
        result = performBinaryOp(OpCode::CmpEqual, testVar, caseVar, &vm);
    }

    // The test value is put back into the stack for the next case.
    // MatchEnd removes it when reached.
    vm.stack.push(testVar);
    vm.stack.push(result);
}

static void opMatchEnd(VM & vm, UInt32)
{
    // Pop the match test value.
    vm.stack.pop();
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
    &opNOOP,                            // ProgStart TODO: allow user hooking up some callback?
    &opProgEnd,                         // ProgEnd
    &opJump,                            // Jmp
    &opJumpIfTrue,                      // JmpIfTrue
    &opJumpIfFalse,                     // JmpIfFalse
    &opJump,                            // JmpReturn
    &opTypeof,                          // Typeof
    &opTypecast,                        // Typecast
    &opCall,                            // Call
    &opCallLocal,                       // CallLocal
    &opNewVar,                          // NewVar
    &opNewRange,                        // NewRange
    &opNewArray,                        // NewArray
    &opNewObj,                          // NewObj
    &opFuncStart,                       // FuncStart
    &opFuncEnd,                         // FuncEnd
    &opForLoopPrep,                     // ForLoopPrep
    &opForLoopTest,                     // ForLoopTest
    &opForLoopStep,                     // ForLoopStep
    &opMatchTest,                       // MatchTest
    &opMatchEnd,                        // MatchEnd
    &opArraySubscript,                  // ArraySubscript
    &opMemberRef,                       // MemberRef
    &opLoadRVR,                         // LoadRVR
    &opStoreRVR,                        // StoreRVR
    &opLoadGlobal,                      // LoadGlobal
    &opStoreGlobal,                     // StoreGlobal
    &opMemberStoreGlobal,               // MemberStoreGlobal
    &opLoadLocal,                       // LoadLocal
    &opStoreLocal,                      // StoreLocal
    &opMemberStoreLocal,                // MemberStoreLocal
    &opStoreArraySubLocal,              // StoreArraySubLocal
    &opStoreArraySubGlobal,             // StoreArraySubGlobal
    &opStoreSetTypeLocal,               // StoreSetTypeLocal
    &opStoreSetTypeGlobal,              // StoreSetTypeGlobal
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
    std::string valStr  = toString(var);
    std::string typeStr = toString(var.type);

    if (var.type == Variant::Type::Str)
    {
        valStr = unescapeString(valStr.c_str());
    }

    os << strPrintF("%s[ %3i ]%s %s" MOON_X64_PRINT_FMT "%s (%s%s%s) => %s\n",
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
        "TYPEOF",
        "TYPECAST",
        "CALL",
        "CALL_LOCAL",
        "NEW_VAR",
        "NEW_RANGE",
        "NEW_ARRAY",
        "NEW_OBJ",
        "FUNC_START",
        "FUNC_END",
        "FOR_LOOP_PREP",
        "FOR_LOOP_TEST",
        "FOR_LOOP_STEP",
        "MATCH_TEST",
        "MATCH_END",
        "ARRAY_SUBSCRIPT",
        "MEMBER_REF",
        "LOAD_RVR",
        "STORE_RVR",
        "LOAD_GLOBAL",
        "STORE_GLOBAL",
        "MEMBER_STORE_GLOBAL",
        "LOAD_LOCAL",
        "STORE_LOCAL",
        "MEMBER_STORE_LOCAL",
        "STORE_ARRAYSUB_LOCAL",
        "STORE_ARRAYSUB_GLOBAL",
        "STORE_SETTYPE_LOCAL",
        "STORE_SETTYPE_GLOBAL",
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
