
// ================================================================================================
// -*- C++ -*-
// File: runtime.cpp
// Author: Guilherme R. Lampert
// Created on: 10/03/16
// Brief: Runtime support structures and code.
// ================================================================================================

#include "runtime.hpp"
#include "symbol_table.hpp"

namespace moon
{

// ========================================================
// Variant:
// ========================================================

Variant Variant::fromSymbol(const Symbol & sym)
{
    Variant v;
    switch (sym.type)
    {
    case Symbol::Type::IntLiteral :
        v.value.asInteger = sym.value.asInteger;
        v.type = Variant::Type::Integer;
        break;

    case Symbol::Type::FloatLiteral :
        v.value.asFloat = sym.value.asFloat;
        v.type = Variant::Type::Float;
        break;

    case Symbol::Type::BoolLiteral :
        v.value.asInteger = sym.value.asBoolean;
        v.type = Variant::Type::Integer;
        break;

    case Symbol::Type::Identifier :
    case Symbol::Type::StrLiteral :
        //FIXME this is temporary. We must copy the string because the symbol might be gone!
        v.value.asStringPtr = (char *)sym.value.asStringPtr;
        v.type = Variant::Type::String;
        break;

    default : // Will return a null variant.
        break;
    }
    return v;
}

std::string toString(const Variant v)
{
    switch (v.type)
    {
    case Variant::Type::Integer : return toString(v.value.asInteger);
    case Variant::Type::Float   : return toString(v.value.asFloat);
    case Variant::Type::String  : return toString(v.value.asStringPtr);
    case Variant::Type::Null    : return "null";
    default                     : return "???";
    } // switch (v.type)
}

std::string toString(const Variant::Type type)
{
    static const std::string typeNames[]
    {
        color::red()    + std::string("null")   + color::restore(),
        color::blue()   + std::string("int")    + color::restore(),
        color::yellow() + std::string("float")  + color::restore(),
        color::white()  + std::string("string") + color::restore()
    };
    static_assert(arrayLength(typeNames) == unsigned(Variant::Type::Count),
                  "Keep this array in sync with the enum declaration!");

    return typeNames[unsigned(type)];
}

// ========================================================
// Function:
// ========================================================

Function::Function() noexcept
    : name           { "???"   }
    , argumentTypes  { nullptr }
    , argumentCount  { 0       }
    , jumpTarget     { 0       }
    , nativeCallback { nullptr }
{ }

Function::Function(const char * funcName, const Variant::Type * args, const std::uint32_t argCount,
                   const std::uint32_t target, const NativeCB callback) noexcept
    : name           { funcName }
    , argumentTypes  { args     }
    , argumentCount  { argCount }
    , jumpTarget     { target   }
    , nativeCallback { callback }
{ }

void Function::operator()(VM & vm, Stack::Slice args) const
{
    validateArguments(args);
    if (nativeCallback != nullptr)
    {
        nativeCallback(vm, args);
    }
    else
    {
        //TODO jumpTarget? error?
        //TODO return type?
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
        MOON_RUNTIME_EXCEPTION("function '" + toString(name) + "()' expected " +
                               toString(argumentCount) + " argument(s) but " +
                               toString(argsIn) + " where provided.");
    }

    for (std::uint32_t a = 0; a < argsIn; ++a)
    {
        const Variant::Type typeIn = args[a].type;
        const Variant::Type typeExpected = argumentTypes[a];

        if (typeIn != typeExpected)
        {
            MOON_RUNTIME_EXCEPTION("function '" + toString(name) + "()' expected " +
                                   toString(typeExpected) + " for argument " + toString(a) +
                                   " but " + toString(typeIn) + " was provided.");
        }
    }
}

// ========================================================
// FunctionTable:
// ========================================================

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

const Function * FunctionTable::addFunction(const char * funcName, const Variant::Type * argTypes,
                                            const std::uint32_t argCount, const std::uint32_t jumpTarget,
                                            const Function::NativeCB nativeCallback)
{
    MOON_ASSERT(funcName != nullptr);
    MOON_ASSERT(!findFunction(funcName) && "Function already registered!");
    MOON_ASSERT(argCount <= Function::MaxArguments && "Max arguments per function exceeded!");

    const auto funcId   = cloneCString(funcName);
    const auto funcArgs = cloneArgTypes(argTypes, argCount);

    //TODO memory pool of Function?
    auto func = new Function{ funcId, funcArgs, argCount, jumpTarget, nativeCallback };
    table[funcId] = func;
    return func;
}

const char * FunctionTable::cloneCString(const char * sourcePtr)
{
    auto sourceLen = std::strlen(sourcePtr);
    if (sourceLen == 0)
    {
        return getEmptyCString();
    }

    auto newString = new char[sourceLen + 1];
    std::memcpy(newString, sourcePtr, sourceLen);
    newString[sourceLen] = '\0';
    return newString;
}

Variant::Type * FunctionTable::cloneArgTypes(const Variant::Type * argTypes, const std::uint32_t argCount)
{
    if (argTypes == nullptr || argCount == 0)
    {
        return nullptr;
    }

    //TODO special allocator? another pool?
    // nevertheless, they must be deallocated. FunctionTable should own the data.
    auto newTypes = new Variant::Type[argCount];
    std::memcpy(newTypes, argTypes, argCount * sizeof(Variant::Type));
    return newTypes;
}

bool FunctionTable::isEmpty() const noexcept
{
    return table.empty();
}

std::size_t FunctionTable::getSize() const noexcept
{
    return table.size();
}

} // namespace moon {}
