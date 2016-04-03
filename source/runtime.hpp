
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
#include "registry.hpp"
#include "pool.hpp"
#include <vector>

#ifndef MOON_TYPEID_POOL_GRANULARITY
    #define MOON_TYPEID_POOL_GRANULARITY 256
#endif // MOON_TYPEID_POOL_GRANULARITY

namespace moon
{

// Forward declarations for the Variant union below.
struct Function;
struct TypeId;
class  Object;

// ========================================================
// Runtime Variant (the common data type):
// ========================================================

struct Variant final
{
    enum class Type : UInt8
    {
        Null = 0,
        Integer,
        Float,
        String,
        Function,

        //TODO these don't yet have operators for them!
        Tid, // Type id
        Object,

        // Number of types. Internal use.
        Count
    };

    union Value
    {
        Int64            asInteger;
        Float64          asFloat;
        const char     * asString; //TODO  replace with Str* at some point
        const Function * asFunction;
        const TypeId   * asTypeId;
        Object         * asObject;
        void           * asVoidPtr;

        Value() noexcept { asInteger = 0; }
    };

    Value value;
    Type  type;

    // Construct a null variant:
    Variant() noexcept
        : type{ Type::Null }
    { }

    // Construct a typed variant but with null/zero contents:
    explicit Variant(const Type t) noexcept
        : type{ t }
    { }

    //
    // Miscellaneous helpers:
    //

    Int64 getAsInteger() const
    {
        MOON_ASSERT(type == Type::Integer);
        return value.asInteger;
    }
    Float64 getAsFloat() const
    {
        MOON_ASSERT(type == Type::Float);
        return value.asFloat;
    }
    const char * getAsCString() const
    {
        MOON_ASSERT(type == Type::String);
        return value.asString;
    }
    const Function * getAsFunction() const
    {
        MOON_ASSERT(type == Type::Function);
        return value.asFunction;
    }
    const TypeId * getAsTypeId() const
    {
        MOON_ASSERT(type == Type::Tid);
        return value.asTypeId;
    }
    Object * getAsObject() const
    {
        MOON_ASSERT(type == Type::Object);
        return value.asObject;
    }
    void * getAsVoidPointer() const
    {
        // No specific type assumed.
        return value.asVoidPtr;
    }

    bool isZero() const noexcept { return value.asInteger == 0; }
    bool isNull() const noexcept { return type == Type::Null;   }
    bool toBool() const noexcept { return !!value.asInteger;    }
};

// ========================================================
// Variant helpers:
// ========================================================

// Debug printing helpers:
std::string toString(Variant v);          // Prints just the value.
std::string toString(Variant::Type type); // Prints the Variant's type tag.
std::string binaryOpToString(OpCode op);  // Prints the symbol if available, e.g.: '+' instead of 'ADD'.
std::string unaryOpToString(OpCode op);   // Prints the symbol or falls back to toString(OpCode).

// Conversion helpers:
Variant variantFromSymbol(const Symbol & sym);
Variant::Type variantTypeFromTypeId(const TypeId * tid);

// Binary operator on Variant (*, %, ==, !=, etc):
bool isBinaryOpValid(OpCode op, Variant::Type typeA, Variant::Type typeB) noexcept;
Variant performBinaryOp(OpCode op, Variant varA, Variant varB);

// Unary operator on Variant (not, -, +):
bool isUnaryOpValid(OpCode op, Variant::Type type) noexcept;
Variant performUnaryOp(OpCode op, Variant var);

// Assigns with integer<=>float implicit conversions.
bool isAssignmentValid(Variant::Type destType, Variant::Type srcType) noexcept;
void performAssignmentWithConversion(Variant & dest, Variant source);

// Will print just the Variant's value according to type tag.
inline std::ostream & operator << (std::ostream & os, const Variant var)
{
    os << toString(var);
    return os;
}

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
    int getMemoryBytesUsed() const noexcept { return getMaxSize() * sizeof(Variant); }

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
        Slice() noexcept // Null/empty slice
            : data { nullptr }
            , size { 0 }
        { }
        Slice(Variant * d, const int s)
            : data { d }
            , size { s }
        {
            MOON_ASSERT(data != nullptr);
            MOON_ASSERT(size > 0);
        }

        const Variant & operator[](const int index) const
        {
            if (index < 0 || index >= size)
            {
                MOON_RUNTIME_EXCEPTION("stack slice index out-of-bounds!");
            }
            return data[index];
        }
        Variant & operator[](const int index)
        {
            if (index < 0 || index >= size)
            {
                MOON_RUNTIME_EXCEPTION("stack slice index out-of-bounds!");
            }
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
            MOON_RUNTIME_EXCEPTION("bad stack slice index=" + toString(first) +
                                   " for stack size=" + toString(top));
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
            MOON_RUNTIME_EXCEPTION("invalid stack slice count=" + toString(count) +
                                   " for stack size=" + toString(top));
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

    static constexpr UInt32 TargetNative = 0;  // Use this for jumpTarget when registering a native function.
    static constexpr UInt32 MaxArguments = 64; // So we can have an allocation hint. This value is enforced upon registration.

    ConstRcString       * name;           // Full function name for debug printing.
    const Variant::Type * returnType;     // Null if the function returns nothing (void), 1 return type otherwise.
    const Variant::Type * argumentTypes;  // May be null for a function taking 0 arguments or to disable validation for varargs.
    const UInt32          argumentCount;  // ditto.
    UInt32                jumpTarget;     // If nonzero nativeCallback is null and this is a script function.
    UInt32                flags;          // Miscellaneous additional flags. See the above bit-field enum. May be zero.
    NativeCB              nativeCallback; // Not null if an external native function. jumpTarget must be zero.

    // invoke() calls the native handler or jumps to the first native instruction.
    // Will also validateArguments() before invoking the handler.
    void invoke(VM & vm, Stack::Slice args) const;

    // Validation of the stack slice Variants according to the
    // expected number and type of arguments for the function.
    void validateArguments(Stack::Slice args) const;

    // Check that the type of the passed variant matches the expected
    // for returnType. If not, it throws a runtime exception.
    void validateReturnValue(Variant retVal) const;

    // Miscellaneous queries:
    bool isScript()      const noexcept { return jumpTarget != TargetNative; }
    bool isNative()      const noexcept { return nativeCallback != nullptr;  }
    bool isVarArgs()     const noexcept { return flags & VarArgs;            }
    bool isDebugOnly()   const noexcept { return flags & DebugOnly;          }
    bool hasCallerInfo() const noexcept { return flags & AddCallerInfo;      }
    bool hasReturnVal()  const noexcept { return returnType != nullptr;      }

    // Prints the function record as a table row for use by FunctionTable::print().
    void print(std::ostream & os) const;
};

// ========================================================
// Runtime Function Table/Registry:
// ========================================================

class FunctionTable final
    : public Registry<const Function *>
{
public:

    FunctionTable() = default;
    ~FunctionTable();

    // Find a function by its fully qualified name (including module) or returns null.
    const Function * findFunction(ConstRcString * const name) const { return findInternal(name); }
    const Function * findFunction(const char * name) const          { return findInternal(name); }

    // Add unique function to the table. Asserts if a function with the same name already exists.
    const Function * addFunction(ConstRcString * funcName, const Variant::Type * returnType,
                                 const Variant::Type * argTypes, UInt32 argCount, UInt32 jumpTarget,
                                 UInt32 flags, Function::NativeCB nativeCallback);

    // Register from char* name string. Same as above, but allocates a new RcString.
    const Function * addFunction(const char * funcName, const Variant::Type * returnType,
                                 const Variant::Type * argTypes, UInt32 argCount, UInt32 jumpTarget,
                                 UInt32 flags, Function::NativeCB nativeCallback);

    // Updates the jumpTarget for a script-defined function.
    // The compiler needs to call this to update the functions defined during parsing.
    void setJumpTargetFor(ConstRcString * const funcName, UInt32 newTarget);
    void setJumpTargetFor(const char * funcName, UInt32 newTarget);

    // Print the whole table in table format (no pun intended).
    void print(std::ostream & os) const;
};

inline std::ostream & operator << (std::ostream & os, const FunctionTable & funcTable)
{
    funcTable.print(os);
    return os;
}

// Adds all the built-in native function entries to a FuncTable.
void registerNativeBuiltInFunctions(FunctionTable & funcTable);

// ========================================================
// struct BuiltInTypeDesc:
// ========================================================

using ObjectFactoryCB = Object * (*)(const TypeId *, Object **);

struct BuiltInTypeDesc final
{
    ConstRcStrUPtr name;
    ObjectFactoryCB instCb;
    const bool internalType; // Tells if this entry goes into the TypeTable.
};

// Return an array with a null entry terminating it.
const BuiltInTypeDesc * getBuiltInTypeNames();

// ========================================================
// struct TypeId:
// ========================================================

struct TypeId final
{
    ConstRcString * name;           // Unique type name within a program.
    ObjectFactoryCB createInstance; // Factory callback. May be null. Some built-ins don't have it.
    const Object  * templateObject; // For a list of members. We just use the types and names.
    bool            isBuiltIn;      // int, float, string, array, etc.
    bool            isTypeAlias;    // Also true if this type was aliased by some other.
};

// ========================================================
// Runtime Type Table/Registry:
// ========================================================

class TypeTable final
    : public Registry<const TypeId *>
{
public:

    explicit TypeTable(Object ** gcListHead);

    // Cached ids for the common runtime classes:
    const TypeId * stringTypeId;
    const TypeId * arrayTypeId;

    // Finds an already registered TypeId by name or returns null if nothing is found.
    const TypeId * findTypeId(ConstRcString * const name) const { return findInternal(name); }
    const TypeId * findTypeId(const char * name) const          { return findInternal(name); }

    // Add unique TypeId to the table. Asserts if an entry with the same name already exists.
    const TypeId * addTypeId(ConstRcString * name, ObjectFactoryCB instCb, const Object * templateObj, bool isBuiltIn);
    const TypeId * addTypeId(const char * name, ObjectFactoryCB instCb, const Object * templateObj, bool isBuiltIn);

    // A type alias just re-registers a TypeId with a new name key.
    const TypeId * addTypeAlias(const TypeId * existingType, ConstRcString * aliasName);
    const TypeId * addTypeAlias(const TypeId * existingType, const char * aliasName);

    // Print the whole table in table format (no pun intended).
    void print(std::ostream & os) const;

private:

    // TypeIds are allocated from here, the table holds pointers into this pool.
    Pool<TypeId, MOON_TYPEID_POOL_GRANULARITY> typeIdPool;
};

inline std::ostream & operator << (std::ostream & os, const TypeTable & typeTable)
{
    typeTable.print(os);
    return os;
}

// ========================================================
// Runtime Base Object:
// ========================================================

class Object
{
public:

    //
    // Common Object data:
    //

    // Runtime type identifier and template for composite objects.
    const TypeId * typeId;

    // Link in the list of dynamically allocated GC objects.
    Object * next;

    // List of members. Since objects are generally small, a plain
    // array+linear-search should be faster and more memory efficient
    // than a full-blown hash-table on the average case.
    struct Member final
    {
        ConstRcString * name;
        Variant         data;
    };
    std::vector<Member> members;

    /*
    struct BitFlags
    {
        bool isAlive      : 1;
        bool isPersistent : 1;
        bool isTemplate   : 1;
    };
    BitFlags flags;
    */

    //
    // Object interface:
    //

    Object(const TypeId * tid, Object ** gcListHead);
    virtual ~Object();

    // Not copyable.
    Object(const Object &) = delete;
    Object & operator = (const Object &) = delete;

    // Overridable initialization:
    virtual void initialize(Stack::Slice constructorArgs);
    virtual void setUpMembersTable();

    // Pretty printers:
    virtual void print(std::ostream & os) const;
    virtual std::string getStringRepresentation() const;
    virtual std::string getTypeName() const;

    // Add new member to the object. The C-string overload
    // of this method will allocate a new ref counted string.
    void addMember(ConstRcString * name, const Variant data);
    void addMember(const char * name, const Variant data);

    // Find member variable or returns a Null Variant if not found.
    Variant findMemberVar(ConstRcString * const name) const;
    Variant findMemberVar(const char * name) const;

    // -1 if not found, index between 0 and members.size-1 otherwise.
    int findMemberIndex(ConstRcString * const name) const;
    int findMemberIndex(const char * name) const;

    // Test the member's existence in this object.
    bool hasMember(ConstRcString * const name) const
    {
        return findMemberIndex(name) >= 0;
    }
    bool hasMember(const char * name) const
    {
        return findMemberIndex(name) >= 0;
    }

    // Access using the return value of findMemberIndex():
    const Member & getMemberAt(const int index) const
    {
        MOON_ASSERT(index >= 0 && index < getMemberCount());
        return members[index];
    }
    Member & getMemberAt(const int index)
    {
        MOON_ASSERT(index >= 0 && index < getMemberCount());
        return members[index];
    }
    int getMemberCount() const noexcept
    {
        return static_cast<int>(members.size());
    }
};

// Allocator used by the NEW_OBJ instruction.
Object * newRuntimeObject(const TypeId * tid, Object ** gcListHead, Stack::Slice constructorArgs);

// ================================================================================================
// Runtime classes:
// ================================================================================================

//
//NOTE some of these object types will be allocated quite often.
// Should we use a couple memory pools to reduce fragmentation?
// Actually, could use a single memory pool sized for the largest type?
// union {
//   Struct,
//   Str,
//   Array,
//   Enum
// }
// ???
//

// ========================================================
// Script Struct:
// ========================================================

class Struct final
    : public Object
{
public:

    using Object::Object;
    static Object * createInstance(const TypeId * tid, Object ** gcListHead);
};

// ========================================================
// Script Str:
// ========================================================

class Str final
    : public Object
{
public:

    using Object::Object;
    static Object * createInstance(const TypeId * tid, Object ** gcListHead);

    static Str * newFromString(VM & vm, const std::string & str, bool makeConst);
    static Str * newFromString(VM & vm, const char * cstr, UInt32 length, bool makeConst);
    static Str * newFromStrings(VM & vm, const Str & strA, const Str & strB, bool makeConst);

    static Variant unaryOp(OpCode op, const Str & str);
    static Variant binaryOp(OpCode op, const Str & strA, const Str & strB);

    int  compare(const Str & other)  const noexcept;
    bool cmpEqual(const Str & other) const noexcept;

    int  getStringLength() const noexcept;
    bool isEmptyString()   const noexcept;
    bool isConstString()   const noexcept;
    const char * c_str()   const noexcept;

    ~Str();

private:

    // When the const ref string is null we are
    // using the C++ string, and vice versa.
    ConstRcString * constString = nullptr;
    std::string     mutableString;
};

// ========================================================
// Script Array:
// ========================================================

class Array final
    : public Object
{
public:

    using Object::Object;
    static Object * createInstance(const TypeId * tid, Object ** gcListHead);

    // small array optimization?             - probably
    // array concatenation with operator +?  - probably
};

// ========================================================
// Script Enum:
// ========================================================

class Enum final
    : public Object
{
public:

    using Object::Object;
    static Object * createInstance(const TypeId * tid, Object ** gcListHead);
};

} // namespace moon {}

#endif // MOON_RUNTIME_HPP
