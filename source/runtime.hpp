
// ================================================================================================
// -*- C++ -*-
// File: runtime.hpp
// Author: Guilherme R. Lampert
// Created on: 10/03/16
// Brief: Runtime support structures and code.
// ================================================================================================

#ifndef MOON_RUNTIME_HPP
#define MOON_RUNTIME_HPP

#include "common.hpp"
#include "pool.hpp"

namespace moon
{

// ========================================================
// Running Variant (the common data type):
// ========================================================

struct Variant final
{
    enum class Type : std::uint8_t
    {
        Null,
        Integer,
        Float,
        String,

        // Number of types. Internal use.
        Count
    };

    union Value
    {
        LangLong    asInteger;
        LangFloat   asFloat;
        char      * asStringPtr;
        void      * asVoidPtr;
    };

    Value value;
    Type  type;

    // Construct a null variant:
    Variant() noexcept : type{ Type::Null }
    {
        value.asInteger = 0;
    }

    static Variant fromSymbol(const Symbol & sym);
};

std::string toString(Variant v); // prints just the value
std::string toString(Variant::Type type);

// ========================================================
// The program stack:
// ========================================================

class Stack final
{
public:

    //
    // Common stack interface:
    //

    // Not copyable.
    Stack(const Stack &) = delete;
    Stack & operator = (const Stack &) = delete;

    explicit Stack(const int maxSize)
        : data { new Variant[maxSize] }
        , size { maxSize }
        , top  { 0 }
    { }

    ~Stack() { delete[] data; }
    void clear() noexcept { top = 0; }

    bool isEmpty() const noexcept { return top == 0;    }
    bool isFull()  const noexcept { return top == size; }

    int getMaxSize()  const noexcept { return size; }
    int getCurrSize() const noexcept { return top;  }

    void push(const Variant & v)
    {
        if (isFull()) { MOON_RUNTIME_EXCEPTION("stack overflow!"); }
        data[top++] = v;
    }
    Variant pop()
    {
        if (isEmpty()) { MOON_RUNTIME_EXCEPTION("stack underflow!"); }
        return data[--top];
    }
    void popN(const int n)
    {
        MOON_ASSERT(n >= 0);
        if (top - n < 0) { MOON_RUNTIME_EXCEPTION("stack underflow!"); }
        top -= n;
    }
    Variant getTopVar() const
    {
        if (isEmpty()) { MOON_RUNTIME_EXCEPTION("stack is empty!"); }
        return data[top - 1];
    }

    //
    // Stack slices:
    //

    class Slice final
    {
    public:
        Slice() noexcept // Null slice
            : data { nullptr }
            , size { 0 }
        { }
        Slice(Variant * d, const int s) noexcept
            : data { d }
            , size { s }
        {
            MOON_ASSERT(data != nullptr);
            MOON_ASSERT(size > 0);
        }

        const Variant & operator[](const int index) const
        {
            if (index < 0 || index >= size) { MOON_RUNTIME_EXCEPTION("stack slice index out-of-bounds!"); }
            return data[index];
        }
        Variant & operator[](const int index)
        {
            if (index < 0 || index >= size) { MOON_RUNTIME_EXCEPTION("stack slice index out-of-bounds!"); }
            return data[index];
        }

        Variant * first()
        {
            if (size == 0) { return nullptr; }
            --size;
            return data;
        }
        Variant * next()
        {
            if (size == 0) { return nullptr; }
            --size;
            return (++data);
        }

        int  getSize() const noexcept { return size;      }
        bool isEmpty() const noexcept { return size == 0; }

    private:
        Variant * data;
        int size;
    };

    Slice slice(const int first, const int count) const
    {
        if (first < 0)
        {
            MOON_RUNTIME_EXCEPTION("bad stack slice index!");
        }
        if (count == 0)
        {
            return {}; // Empty/null slice.
        }

        auto slicePtr = data + first;
        auto endPtr   = data + top;
        if (slicePtr > endPtr)
        {
            MOON_RUNTIME_EXCEPTION("invalid stack slice range!");
        }
        if ((slicePtr + count) > endPtr)
        {
            MOON_RUNTIME_EXCEPTION("invalid stack slice size!");
        }
        return { slicePtr, count };
    }

private:

    Variant * data;
    const int size;
    int top;
};

// ========================================================
// Runtime Function (native or script):
// ========================================================

struct Function final
{
    // Pointer to a native C/C++ callback:
    using NativeCB = void (*)(VM & vm, Stack::Slice args);

    static constexpr std::uint32_t TargetNative = 0;
    static constexpr std::uint32_t MaxArguments = 64;

    const char * const    name;           // Full function name for debug printing.
    const Variant::Type * argumentTypes;  // May be null for a function taking 0 arguments or to disable validation for varargs.
    const std::uint32_t   argumentCount;  // ditto.
    const std::uint32_t   jumpTarget;     // If nonzero nativeCallback is null and this is a script function.
    const NativeCB        nativeCallback; // Not null if an external native function. jumpTarget must be zero.

    // Construct a null/invalid function.
    Function() noexcept;

    Function(const char * funcName,
             const Variant::Type * args,
             std::uint32_t argCount,
             std::uint32_t target,
             NativeCB callback) noexcept;

    void operator()(VM & vm, Stack::Slice args) const;
    void validateArguments(Stack::Slice args)   const;
    bool isNative() const noexcept { return nativeCallback != nullptr; }
};

// ========================================================
// Runtime Function Table/Registry:
// ========================================================

class FunctionTable final
{
public:

    using FuncTable = HashTable<const Function *>;
    FunctionTable() = default;

    // Not copyable.
    FunctionTable(const FunctionTable &) = delete;
    FunctionTable & operator = (const FunctionTable &) = delete;

    // Find a function by its fully qualified name (including module) or returns null.
    const Function * findFunction(const char * name) const;

    // Add unique function to the table. Asserts with a function with the same name already exists.
    const Function * addFunction(const char * funcName, const Variant::Type * argTypes,
                                 const std::uint32_t argCount, const std::uint32_t jumpTarget,
                                 const Function::NativeCB nativeCallback);

    // Miscellaneous queries:
    bool isEmpty() const noexcept;
    std::size_t getSize() const noexcept;

private:

    const char * cloneCString(const char * sourcePtr);
    Variant::Type * cloneArgTypes(const Variant::Type * argTypes, const std::uint32_t argCount);

    // Hash table indexed by the full function name.
    FuncTable table;
};

} // namespace moon {}

#endif // MOON_RUNTIME_HPP
