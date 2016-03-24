
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
#include "opcodes.hpp"

namespace moon
{

// Forward declarations for the Variant union below.
struct Function;

// ========================================================
// Runtime Variant (the common data type):
// ========================================================

struct Variant final
{
    enum class Type : std::uint8_t
    {
        Null = 0,
        Integer,
        Float,
        String,
        Function,

        // Number of types. Internal use.
        Count
    };

    union Value
    {
        LangLong         asInteger;
        LangFloat        asFloat;
        const Function * asFunctionPtr;
        const char     * asStringPtr;
        void           * asVoidPtr;
    };

    Value value;
    Type  type;

    // Construct a null variant:
    Variant() noexcept : type{ Type::Null }
    {
        value.asInteger = 0;
    }
    explicit Variant(const Type t) noexcept : type{ t }
    {
        value.asInteger = 0;
    }

    //
    // Miscellaneous helpers:
    //
    bool isZero() const noexcept { return value.asInteger == 0; }
    bool toBool() const noexcept { return !!value.asInteger;    }
    static Variant fromSymbol(const Symbol & sym);
};

// Debug printing helpers:
std::string toString(Variant v);          // Prints just the value
std::string toString(Variant::Type type); // Prints the variant type tag
std::string binaryOpToString(OpCode op);  // Prints the symbol if available, e.g.: + instead of ADD
std::string unaryOpToString(OpCode op);   // Prints the symbol or falls back to toString(OpCode)

// Binary operator on Variant:
bool isBinaryOpValid(OpCode op, Variant::Type typeA, Variant::Type typeB) noexcept;
Variant performBinaryOp(OpCode op, Variant varA, Variant varB);

// Unary operator on Variant (not, -, +):
bool isUnaryOpValid(OpCode op, Variant::Type type) noexcept;
Variant performUnaryOp(OpCode op, Variant var);

// ========================================================
// The Program Stack:
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
            MOON_RUNTIME_EXCEPTION("bad stack slice index " + toString(first) +
                                   " for stack size " + toString(top));
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
    // Pointer to a native C++ callback:
    using NativeCB = void (*)(VM & vm, Stack::Slice args);

    // Flags that can be ORed for the 'flags' member field.
    enum BitFlags
    {
        VarArgs       = 1 << 0, // Function takes a varying number of arguments, like std::printf.
        DebugOnly     = 1 << 1, // Calls to this function get stripped out if not compiling in debug mode.
        AddCallerInfo = 1 << 2  // Ask the compiler to add source filename and line num to every call.
    };

    static constexpr std::uint32_t TargetNative = 0;  // Use this for jumpTarget when registering a native function.
    static constexpr std::uint32_t MaxArguments = 64; // So we can have an allocation hint. This value is enforced upon registration.

    const char * const    name;           // Full function name for debug printing.
    const Variant::Type * returnType;     // Null if the function returns nothing (void), 1 return type otherwise.
    const Variant::Type * argumentTypes;  // May be null for a function taking 0 arguments or to disable validation for varargs.
    const std::uint32_t   argumentCount;  // ditto.
    std::uint32_t         jumpTarget;     // If nonzero nativeCallback is null and this is a script function.
    std::uint32_t         flags;          // Miscellaneous additional flags. See the above bit-field enum. May be zero.
    NativeCB              nativeCallback; // Not null if an external native function. jumpTarget must be zero.

    // invoke() calls the native handler or jumps to the first native instruction.
    // Will also validateArguments() before invoking the handler.
    void invoke(VM & vm, Stack::Slice args) const;

    // Validation of the stack slice Variants according to the
    // expected number and type of arguments for the function.
    void validateArguments(Stack::Slice args) const;

    // Miscellaneous queries:
    bool isNative()      const noexcept { return nativeCallback != nullptr; }
    bool isVarArgs()     const noexcept { return flags & VarArgs;           }
    bool isDebugOnly()   const noexcept { return flags & DebugOnly;         }
    bool hasCallerInfo() const noexcept { return flags & AddCallerInfo;     }
    bool hasReturnVal()  const noexcept { return returnType != nullptr;     }

    // Prints the function record as a table row for use by FunctionTable::print().
    void print(std::ostream & os = std::cout) const;
};

// ========================================================
// Runtime Function Table/Registry:
// ========================================================

class FunctionTable final
{
public:

    using FuncTable = HashTableCStr<const Function *>;

     FunctionTable() = default;
    ~FunctionTable();

    // Not copyable.
    FunctionTable(const FunctionTable &) = delete;
    FunctionTable & operator = (const FunctionTable &) = delete;

    // Find a function by its fully qualified name (including module) or returns null.
    const Function * findFunction(const char * name) const;

    // Add unique function to the table. Asserts if a function with the same name already exists.
    const Function * addFunction(const char * funcName, const Variant::Type * returnType,
                                 const Variant::Type * argTypes, std::uint32_t argCount,
                                 std::uint32_t jumpTarget, std::uint32_t flags,
                                 Function::NativeCB nativeCallback);

    // Updates the jumpTarget for a script function.
    // The compiler needs to call this to update the functions defined during parsing.
    void setJumpTargetFor(const char * funcName, std::uint32_t newTarget);

    // Miscellaneous queries:
    bool isEmpty() const noexcept;
    std::size_t getSize() const noexcept;

    // Print the whole table in table format (no pun intended).
    void print(std::ostream & os = std::cout) const;

private:

    const char * cloneCString(const char * sourcePtr);

    // Hash table indexed by the full function name.
    FuncTable table;
};

// Adds all the built-in native function entries to a FuncTable.
void registerNativeBuiltInFunctions(FunctionTable & funcTable);

// Calls FunctionTable::print().
std::ostream & operator << (std::ostream & os, const FunctionTable & funcTable);

} // namespace moon {}

#endif // MOON_RUNTIME_HPP
