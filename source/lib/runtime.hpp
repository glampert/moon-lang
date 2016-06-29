
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

namespace moon
{

// Forward declarations for the Variant union below.
struct Function;
struct TypeId;
class  TypeTable;
class  Object;
class  Array;
class  Str;

// ========================================================
// Runtime Variant (the common data type):
// ========================================================

// Every variable in a script is backed by one of this,
// including member variables. This is a traditional
// "tagged union" approach.
struct Variant final
{
    enum class Type : UInt8
    {
        Null = 0,
        Integer,
        Float,
        Function,
        Tid,
        Str,
        Object,
        Array,
        Range,
        Any,

        // Number of types. Internal use.
        Count
    };

    union Value
    {
        const void     * asVoidPtr;
        Int64            asInteger;
        Float64          asFloat;
        const Function * asFunction;
        const TypeId   * asTypeId;
        Str            * asString;
        Object         * asObject;
        Array          * asArray;
        Range            asRange;

        Value() noexcept { asInteger = 0; }
    };

    Value value;   // Value must be interpreted according to the following tags.
    Type  type;    // Type of the underlaying data or 'Any'.
    Type  anyType; // If type == Any, then this holds the type of the underlaying data.

    // Construct a null variant:
    Variant() noexcept
        : type    { Type::Null }
        , anyType { Type::Null }
    { }

    // Construct a typed variant but with null/zero contents:
    explicit Variant(const Type varType) noexcept
        : type    { varType    }
        , anyType { Type::Null }
    { }

    // Make a new Variant from the any type.
    Variant unwrapAny() const
    {
        MOON_ASSERT(type    == Variant::Type::Any);
        MOON_ASSERT(anyType != Variant::Type::Any);
        Variant var{ anyType };
        var.value = value;
        return var;
    }

    //
    // Accessors:
    //

    const void * getAsVoidPointer() const
    {
        // No specific type assumed.
        return value.asVoidPtr;
    }
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
    Str * getAsString() const
    {
        MOON_ASSERT(type == Type::Str);
        return value.asString;
    }
    Object * getAsObject() const
    {
        MOON_ASSERT(type == Type::Object);
        return value.asObject;
    }
    Array * getAsArray() const
    {
        MOON_ASSERT(type == Type::Array);
        return value.asArray;
    }
    Range getAsRange() const
    {
        MOON_ASSERT(type == Type::Range);
        return value.asRange;
    }

    bool toBool() const noexcept { return !!value.asInteger;    }
    bool isZero() const noexcept { return value.asInteger == 0; }
    bool isNull() const noexcept { return type == Type::Null;   }
    bool isAny()  const noexcept { return type == Type::Any;    }
};

// ========================================================
// Variant helpers:
// ========================================================

// Debug printing helpers:
std::string toString(Variant v);          // Prints just the value.
std::string toString(Variant::Type type); // Prints the Variant's type tag.
std::string binaryOpToString(OpCode op);  // Prints the symbol if available, e.g.: '+' instead of 'ADD'.
std::string unaryOpToString(OpCode op);   // Prints the symbol or falls back to toString(OpCode).

// Binary operator on Variant (*, %, ==, !=, etc):
bool isBinaryOpValid(OpCode op, Variant::Type typeA, Variant::Type typeB);
Variant performBinaryOp(OpCode op, Variant varA, Variant varB, VM * pVM); // VM instance only required for string concat (op +)

// Unary operator on Variant (not, -, +):
bool isUnaryOpValid(OpCode op, Variant::Type type);
Variant performUnaryOp(OpCode op, Variant var);

// Assigns with integer<=>float implicit conversions.
bool isAssignmentValid(Variant::Type destType, Variant::Type srcType);
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
        Slice(Variant * d, const int s) noexcept
            : data { d }
            , size { s }
        { }

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

        Variant * first() noexcept
        {
            if (size == 0) { return nullptr; }
            --size;
            return data;
        }
        Variant * next() noexcept
        {
            if (size == 0) { return nullptr; }
            --size;
            return (++data);
        }
        Variant pop() // Removes the last element and returns it
        {
            if (isEmpty()) { MOON_RUNTIME_EXCEPTION("stack slice underflow!"); }
            return data[--size];
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
    bool validateArguments(Stack::Slice args, std::string & errorMessageOut) const;

    // Check that the type of the passed variant matches the expected
    // for returnType. If not, it throws a runtime exception.
    void validateReturnValue(Variant retVal) const;
    bool validateReturnValue(Variant::Type retValType, std::string & errorMessageOut) const;

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

    // Find a function by its name or returns null.
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

using ObjectFactoryCB = Object * (*)(VM &, const TypeId *);

struct BuiltInTypeDesc final
{
    ConstRcStrUPtr  name;
    ObjectFactoryCB newInstance;
    const bool      internalType; // Tells if this entry goes into the TypeTable.
};

// Return an array with a null entry terminating it.
const BuiltInTypeDesc * getBuiltInTypeNames();

// ========================================================
// struct TypeId:
// ========================================================

struct TypeId final
{
    ConstRcString * name;           // Unique type name within a program.
    ObjectFactoryCB newInstance;    // Factory callback. May be null. Some built-ins don't have it.
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

    explicit TypeTable(VM & vm);

    // Cached ids for the common runtime types:
    const TypeId * nullTypeId;
    const TypeId * intTypeId;
    const TypeId * longTypeId;
    const TypeId * floatTypeId;
    const TypeId * doubleTypeId;
    const TypeId * boolTypeId;
    const TypeId * rangeTypeId;
    const TypeId * anyTypeId;
    const TypeId * objectTypeId;
    const TypeId * functionTypeId;
    const TypeId * strTypeId;
    const TypeId * arrayTypeId;
    const TypeId * tidTypeId;

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

    friend class GC;

    // Miscellaneous object flags:
    bool isAlive;
    bool isPersistent;
    bool isTemplateObj;
    bool isBuiltInType;
    bool isStructType;
    bool isEnumType;
    bool isReachable; // GC reachability flag
    bool isSmall;     // GC pool hint ('small' or 'large' object)

    // Member variable record (name ref is held by the object):
    struct Member
    {
        ConstRcString * name;
        Variant         data;
    };

    //
    // Object interface:
    //

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
    virtual std::string getAddressString() const;

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

    // Query the dynamic type of the object (Array, Str, Struct, user-defined, etc).
    const TypeId * getTypeId() const noexcept
    {
        return typeId;
    }
    void setTypeId(const TypeId * tid) noexcept
    {
        typeId = tid;
    }

    void markTypeTemplate() noexcept
    {
        isPersistent  = true;
        isTemplateObj = true;
    }

    const Object * getGCLink() const noexcept { return gcNext; }

protected:

    // No direct construction. Use either Type::newInstance() or specialized allocator.
    explicit Object(const TypeId * tid) noexcept;

private:

    // Runtime type identifier and template for composite objects.
    const TypeId * typeId;

    // Link in the list of dynamically allocated GC objects.
    Object * gcNext;

    // List of members. Since objects are generally small, a plain
    // array+linear-search should be faster and more memory efficient
    // than a full-blown hash-table on the average case.
    std::vector<Member> members;
};

// ================================================================================================
// Runtime script classes:
// ================================================================================================

// Allocator used by the NEW_OBJ instruction (forwards to the GC'd newInstance callback).
Object * newRuntimeObject(VM & vm, const TypeId * tid, Stack::Slice constructorArgs);
void freeRuntimeObject(VM & vm, Object * obj);

// ========================================================
// Script Struct:
// ========================================================

class Struct final
    : public Object
{
public:

    friend class GC;
    static Object * newInstance(VM & vm, const TypeId * tid);

private:
    // Import the parent's constructor.
    using Object::Object;
};

// ========================================================
// Script Enum:
// ========================================================

class Enum final
    : public Object
{
public:

    friend class GC;
    static Object * newInstance(VM & vm, const TypeId * tid);

private:
    // Import the parent's constructor.
    using Object::Object;
};

// ========================================================
// Script String:
// ========================================================

class Str final
    : public Object
{
public:

    friend class GC;
    static Object * newInstance(VM & vm, const TypeId * tid);
    static Str * newFromString(VM & vm, ConstRcString * rstr);
    static Str * newFromString(VM & vm, const std::string & str, bool makeConst);
    static Str * newFromString(VM & vm, const char * cstr, UInt32 length, bool makeConst);
    static Str * newFromStrings(VM & vm, const Str & strA, const Str & strB, bool makeConst);
    static Str * newNullString(VM & vm);

    static Variant unaryOp(OpCode op, const Str & str);
    static Variant binaryOp(OpCode op, const Str & strA, const Str & strB);

    std::string getStringRepresentation() const override;
    void clear();

    int  compare(const Str & other)  const;
    bool cmpEqual(const Str & other) const;

    int compareNoCase(const Str & other) const
    {
        return compareCStringsNoCase(c_str(), other.c_str());
    }
    bool cmpEqualNoCase(const Str & other) const
    {
        return compareCStringsNoCase(c_str(), other.c_str()) == 0;
    }

    // Miscellaneous accessors:
    int getStringLength() const noexcept
    {
        if (isConstString())
        {
            return static_cast<int>(constString->length);
        }
        return static_cast<int>(mutableString.length());
    }
    bool isEmptyString() const noexcept
    {
        if (isConstString())
        {
            return constString->length == 0;
        }
        return mutableString.empty();
    }
    bool isConstString() const noexcept
    {
        return constString != nullptr;
    }
    const char * c_str() const noexcept
    {
        return (constString != nullptr) ? constString->chars : mutableString.c_str();
    }

    ~Str();

private:
    // Import the parent's constructor.
    using Object::Object;

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

    friend class GC;
    static Object * newInstance(VM & vm, const TypeId * tid);
    static Array * newEmpty(VM & vm, const TypeId * dataType, int capacityHint);
    static Array * newFromArgs(VM & vm, const TypeId * dataType, Stack::Slice args);
    static Array * newFromRawData(VM & vm, const TypeId * dataType, const void * data,
                                  int lengthInItems, int sizeBytesPerItem, Variant::Type varType);

    std::string getStringRepresentation() const override;
    std::string getDataTypeString() const { return toString(varType); }
    Variant::Type getDataType() const noexcept { return varType; }

    void push(Variant var);
    void push(const Array & other);
    void push(const void * data, int lengthInItems,
              int sizeBytesPerItem, Variant::Type dataType);

    Variant getIndex(int index) const;
    void setIndex(int index, Variant var);

    void setItemTypeSize(const TypeId * tid);
    void reserveCapacity(int capacityHint);
    int  getArrayCapacity() const noexcept; // In items

    void pop(const int count) noexcept
    {
        arrayLen -= count;
        if (arrayLen < 0)
        {
            arrayLen = 0;
        }
    }
    void clear() noexcept
    {
        arrayLen = 0;
    }

    bool isEmptyArray()   const noexcept { return arrayLen == 0; }
    bool isSmallArray()   const noexcept { return !isVector; }
    bool isDynamicArray() const noexcept { return isVector;  }
    int  getArrayLength() const noexcept { return arrayLen;  } // In items
    int  getItemSize()    const noexcept { return itemSize;  } // In bytes

    ~Array();

private:
    // Import the parent's constructor.
    using Object::Object;

    template<typename T>
    T * castTo() noexcept
    {
        return reinterpret_cast<T *>(&backingStore);
    }
    template<typename T>
    const T * castTo() const noexcept
    {
        return reinterpret_cast<const T *>(&backingStore);
    }
    UInt8 * getDataPtr() noexcept
    {
        return isVector ? castTo<VecType>()->data() : castTo<UInt8>();
    }
    const UInt8 * getDataPtr() const noexcept
    {
        return isVector ? castTo<VecType>()->data() : castTo<UInt8>();
    }

    void appendRawData(const void * data, int lengthInItems,
                       int sizeBytesPerItem, Variant::Type dataType);

    using VecType = std::vector<UInt8>;
    static constexpr int SmallBufSize = 56;

    // On a 64-bits arch, sizeof(Array) is 128
    union Backing
    {
        // We only resort to the dynamic vector once the SmallBufSize
        // small array capacity is depleted. This prevents allocations
        // for very small arrays like vec3s, vec4s and the like.
        alignas(VecType) UInt8 vectorBytes[sizeof(VecType)];
        alignas(Variant) UInt8 smallBufBytes[SmallBufSize];
    };

    Backing       backingStore{};
    Int32         arrayLen = 0;
    Int32         itemSize = 0;
    bool          isVector = false;
    Variant::Type varType  = Variant::Type::Null;
};

} // namespace moon {}

#endif // MOON_RUNTIME_HPP
