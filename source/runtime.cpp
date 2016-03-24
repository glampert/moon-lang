
// ================================================================================================
// -*- C++ -*-
// File: runtime.cpp
// Author: Guilherme R. Lampert
// Created on: 10/03/16
// Brief: Runtime support structures and code.
// ================================================================================================

#include "vm.hpp"
#include "runtime.hpp"
#include "symbol_table.hpp"

// For std::is_pod<T>
#include <type_traits>

namespace moon
{

// ========================================================
// Variant:
// ========================================================

Variant Variant::fromSymbol(const Symbol & sym)
{
    Variant var;

    switch (sym.type)
    {
    case Symbol::Type::IntLiteral :
        var.value.asInteger = sym.value.asInteger;
        var.type = Variant::Type::Integer;
        break;

    case Symbol::Type::FloatLiteral :
        var.value.asFloat = sym.value.asFloat;
        var.type = Variant::Type::Float;
        break;

    case Symbol::Type::BoolLiteral :
        var.value.asInteger = sym.value.asBoolean;
        var.type = Variant::Type::Integer;
        break;

    case Symbol::Type::Identifier :
    case Symbol::Type::StrLiteral :
        //FIXME this is temporary. We must copy the string because the symbol might be gone! (or use RcString)
        var.value.asStringPtr = sym.value.asStringPtr;
        var.type = Variant::Type::String;
        break;

    default :
        MOON_RUNTIME_EXCEPTION("symbol doesn't map directly to a variant!");
    } // switch (sym.type)

    return var;
}

std::string toString(const Variant var)
{
    switch (var.type)
    {
    case Variant::Type::Integer  : return toString(var.value.asInteger);
    case Variant::Type::Float    : return toString(var.value.asFloat);
    case Variant::Type::String   : return toString(var.value.asStringPtr);
    case Variant::Type::Function : return toString(var.value.asFunctionPtr ? var.value.asFunctionPtr->name : "null");
    case Variant::Type::Null     : return "null";
    default                      : return "???";
    } // switch (var.type)
}

std::string toString(const Variant::Type type)
{
    static const std::string typeNames[]
    {
        color::red()    + std::string("null")   + color::restore(),
        color::blue()   + std::string("int")    + color::restore(),
        color::yellow() + std::string("float")  + color::restore(),
        color::white()  + std::string("string") + color::restore(),
        color::white()  + std::string("func")   + color::restore()
    };
    static_assert(arrayLength(typeNames) == unsigned(Variant::Type::Count),
                  "Keep this array in sync with the enum declaration!");

    return typeNames[unsigned(type)];
}

// ========================================================
// Function:
// ========================================================

void Function::invoke(VM & vm, Stack::Slice args) const
{
    validateArguments(args);

    if (nativeCallback != nullptr)
    {
        nativeCallback(vm, args);
    }
    else if (jumpTarget != TargetNative)
    {
        vm.setProgramCounter(jumpTarget);
    }
    else
    {
        MOON_RUNTIME_EXCEPTION("function has no native callback or jump target!");
    }
}

void Function::validateArguments(Stack::Slice args) const
{
    // No args or varargs validated by the function itself.
    if (argumentTypes == nullptr || argumentCount == 0)
    {
        return;
    }

    const std::uint32_t argsIn = args.getSize();
    if (argsIn != argumentCount)
    {
        MOON_RUNTIME_EXCEPTION("function '" + toString(name) + "' expected " +
                               toString(argumentCount) + " argument(s) but " +
                               toString(argsIn) + " where provided.");
    }

    for (std::uint32_t a = 0; a < argsIn; ++a)
    {
        const Variant::Type typeIn = args[a].type;
        const Variant::Type typeExpected = argumentTypes[a];

        if (typeIn != typeExpected)
        {
            MOON_RUNTIME_EXCEPTION("function '" + toString(name) + "' expected " +
                                   toString(typeExpected) + " for argument " + toString(a) +
                                   " but " + toString(typeIn) + " was provided.");
        }
    }
}

void Function::print(std::ostream & os) const
{
    const std::string retTypeStr   = hasReturnVal() ? toString(*returnType) : "void";
    const std::string argCountStr  = !isVarArgs() ? toString(argumentCount) : "varargs";
    const std::string jmpTargetStr = (jumpTarget != TargetNative) ? toString(jumpTarget) : "native";

    std::string flagsStr;
    if (isNative())       { flagsStr += "N "; }
    if (isVarArgs())      { flagsStr += "V "; }
    if (isDebugOnly())    { flagsStr += "D "; }
    if (hasCallerInfo())  { flagsStr += "I "; }
    if (hasReturnVal())   { flagsStr += "R "; }
    if (flagsStr.empty()) { flagsStr = "- - - - -"; }

    char formatStr[512] = {'\0'};
    std::snprintf(formatStr, arrayLength(formatStr), "| %-30s | %-9s | %-9s | %-6s | %s\n",
                  name, argCountStr.c_str(), flagsStr.c_str(), jmpTargetStr.c_str(), retTypeStr.c_str());

    os << formatStr;
}

// ========================================================
// FunctionTable:
// ========================================================

FunctionTable::~FunctionTable()
{
    static_assert(std::is_pod<Function>::value, "Function struct should be a POD type!");

    // The Function instances are allocated as raw byte blocks
    // and may be followed by argument lists and return type.
    // As long as we keep the Function type a POD, this is safe.
    for (auto && entry : table)
    {
        auto funcObj = const_cast<Function *>(entry.second);
        ::operator delete(reinterpret_cast<void *>(funcObj)); // Raw delete. Doesn't call the destructor.
    }
    table.clear();
}

const Function * FunctionTable::findFunction(const char * name) const
{
    MOON_ASSERT(name != nullptr);
    const auto it = table.find(name);
    if (it != std::end(table))
    {
        return it->second;
    }
    return nullptr; // Not found.
}

const Function * FunctionTable::addFunction(const char * funcName, const Variant::Type * returnType,
                                            const Variant::Type * argTypes, const std::uint32_t argCount,
                                            const std::uint32_t jumpTarget, const std::uint32_t extraFlags,
                                            Function::NativeCB nativeCallback)
{
    MOON_ASSERT(funcName != nullptr);
    MOON_ASSERT(!findFunction(funcName) && "Function already registered!");
    MOON_ASSERT(argCount <= Function::MaxArguments && "Max arguments per function exceeded!");

    const std::size_t extraTypes = (returnType != nullptr) ? 1 : 0;
    const std::size_t totalBytes = sizeof(Function) + ((argCount + extraTypes) * sizeof(Variant::Type));

    // We allocate the Function instance plus eventual argument type list and return
    // type as a single memory block. Function will be first, followed by N arguments
    // then the return type at the end.
    std::uint8_t * memPtr = static_cast<std::uint8_t *>(::operator new(totalBytes));

    Function * funcObj = reinterpret_cast<Function *>(memPtr);
    memPtr += sizeof(Function);

    Variant::Type * funcArgs;
    if (argTypes != nullptr && argCount != 0)
    {
        funcArgs = reinterpret_cast<Variant::Type *>(memPtr);
        memPtr += argCount * sizeof(Variant::Type);
        std::memcpy(funcArgs, argTypes, argCount * sizeof(Variant::Type));
    }
    else
    {
        funcArgs = nullptr;
    }

    Variant::Type * funcRet;
    if (returnType != nullptr)
    {
        funcRet = reinterpret_cast<Variant::Type *>(memPtr);
        memPtr += sizeof(Variant::Type);
        *funcRet = *returnType;
    }
    else
    {
        funcRet = nullptr;
    }

    const auto funcId = cloneCString(funcName);
    table[funcId] = construct(funcObj, { funcId, funcRet, funcArgs, argCount, jumpTarget, extraFlags, nativeCallback });
    return funcObj;
}

void FunctionTable::setJumpTargetFor(const char * funcName, const std::uint32_t newTarget)
{
    MOON_ASSERT(funcName != nullptr);

    auto it = table.find(funcName);
    MOON_ASSERT(it != std::end(table));

    // This is the only time we need to unconst the object, so
    // it is still worth storing it as const Function* in the table.
    auto func = const_cast<Function *>(it->second);
    func->jumpTarget = newTarget;
}

const char * FunctionTable::cloneCString(const char * sourcePtr)
{
    auto sourceLen = std::strlen(sourcePtr);
    if (sourceLen == 0)
    {
        return getEmptyCString();
    }

    //FIXME probably replace with ConstRcString!
    auto newString = new char[sourceLen + 1];
    std::memcpy(newString, sourcePtr, sourceLen);
    newString[sourceLen] = '\0';
    return newString;
}

bool FunctionTable::isEmpty() const noexcept
{
    return table.empty();
}

std::size_t FunctionTable::getSize() const noexcept
{
    return table.size();
}

void FunctionTable::print(std::ostream & os) const
{
    os << color::white() << "[[ begin function table dump ]]" << color::restore() << "\n";
    if (!table.empty())
    {
        os << "+--------------------------------+-----------+-----------+--------+----------+\n";
        os << "| name                           | arg-count | flags     | jump   | ret-type |\n";
        os << "+--------------------------------+-----------+-----------+--------+----------+\n";
        for (auto && entry : table)
        {
            entry.second->print(os);
        }
    }
    else
    {
        os << "(empty)\n";
    }
    os << color::white() << "[[ listed " << table.size() << " functions ]]" << color::restore() << "\n";
}

std::ostream & operator << (std::ostream & os, const FunctionTable & funcTable)
{
    funcTable.print(os);
    return os;
}

} // namespace moon {}
