
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
#include <type_traits> // For std::is_pod<T>

namespace moon
{

// ========================================================
// Variant:
// ========================================================

Variant variantFromSymbol(const Symbol & sym)
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

//    case Symbol::Type::Identifier :
    case Symbol::Type::StrLiteral :
        //FIXME this is temporary. We must copy the string because the symbol might be gone! (or use RcString)
        var.value.asString = sym.value.asString->chars;
        var.type = Variant::Type::String;
        break;

    default :
        MOON_RUNTIME_EXCEPTION("symbol doesn't map directly to a variant!");
    } // switch (sym.type)

    return var;
}

Variant::Type variantTypeFromTypeId(const TypeId * tid)
{
    MOON_ASSERT(tid != nullptr && isRcStringValid(tid->name));

    // Cached for subsequent lookups:
    static constexpr auto hashInt   = ct::hashCString( "int"      );
    static constexpr auto hashLong  = ct::hashCString( "long"     );
    static constexpr auto hashFloat = ct::hashCString( "float"    );
    static constexpr auto hashBool  = ct::hashCString( "bool"     );
    //static constexpr auto hashRange = ct::hashCString( "range"    );
    //static constexpr auto hashAny   = ct::hashCString( "any"      );
    static constexpr auto hashObj   = ct::hashCString( "object"   );
    static constexpr auto hashFunc  = ct::hashCString( "function" );
    static constexpr auto hashStr   = ct::hashCString( "str"      );
    //static constexpr auto hashArray = ct::hashCString( "array"    );

    //TODO add the commented types at some point!
    if (hashInt   == tid->name->hashVal) { return Variant::Type::Integer;  }
    if (hashLong  == tid->name->hashVal) { return Variant::Type::Integer;  }
    if (hashFloat == tid->name->hashVal) { return Variant::Type::Float;    }
    if (hashBool  == tid->name->hashVal) { return Variant::Type::Integer;  }
    //if (hashRange == tid->name->hashVal) { return Variant::Type::Range;    }
    //if (hashAny   == tid->name->hashVal) { return Variant::Type::Any;      }
    if (hashObj   == tid->name->hashVal) { return Variant::Type::Object;   }
    if (hashFunc  == tid->name->hashVal) { return Variant::Type::Function; }
    if (hashStr   == tid->name->hashVal) { return Variant::Type::String;   }
    //if (hashArray == tid->name->hashVal) { return Variant::Type::Array;    }

    MOON_RUNTIME_EXCEPTION("bad variable type id '" + toString(tid->name) +
                           "' (" + strPrintF("0x%08X", tid->name->hashVal) + ")");
}

std::string toString(const Variant var)
{
    switch (var.type)
    {
    case Variant::Type::Null :
        return "null";

    case Variant::Type::Integer :
        return toString(var.value.asInteger);

    case Variant::Type::Float :
        return toString(var.value.asFloat);

    case Variant::Type::String :
        return toString(var.value.asString);

    case Variant::Type::Function :
        return toString(var.value.asFunction ? var.value.asFunction->name->chars : "null");

    case Variant::Type::Tid :
        return toString(var.value.asTypeId ? var.value.asTypeId->name->chars : "null");

    case Variant::Type::Object :
        {
            auto addr = strPrintF(" @%016llX", static_cast<UInt64>(
                                  reinterpret_cast<std::uintptr_t>(var.value.asVoidPtr)));
            return (var.value.asObject ? (var.value.asObject->getTypeName() + addr) : "null");
        }
    default :
        return "???";
    } // switch (var.type)
}

std::string toString(const Variant::Type type)
{
    static const std::string typeNames[]
    {
        color::red()     + std::string("null")     + color::restore(),
        color::blue()    + std::string("int")      + color::restore(),
        color::yellow()  + std::string("float")    + color::restore(),
        color::white()   + std::string("str")      + color::restore(),
        color::green()   + std::string("function") + color::restore(),
        color::cyan()    + std::string("tid")      + color::restore(),
        color::magenta() + std::string("object")   + color::restore()
    };
    static_assert(arrayLength(typeNames) == UInt32(Variant::Type::Count),
                  "Keep this array in sync with the enum declaration!");

    return typeNames[UInt32(type)];
}

// ========================================================
// Function:
// ========================================================

void Function::invoke(VM & vm, Stack::Slice args) const
{
    // Extra validation for safety, but the compiler should
    // have already checked that the provided argCount
    // matches the expected # and expected types.
    validateArguments(args);

    // Return to whatever is after the CALL instruction
    // (the current one being executed). This is only
    // relevant for the script functions, since the
    // native callback will return immediately, but
    // we set for both just to be consistent.
    vm.setReturnAddress(vm.getProgramCounter() + 1);

    // Dispatch to the appropriate handler:
    if (isNative())
    {
        nativeCallback(vm, args);
        // Validate the return value here because we will not hit
        // a FuncEnd instruction when returning from a native call.
        validateReturnValue(vm.getReturnValue());
    }
    else if (isScript())
    {
        vm.setProgramCounter(jumpTarget);
        // Now the next instruction executed will be this function's FuncStart.
        // Next FuncEnd instruction will validated the return value, if any.
    }
    else
    {
        MOON_RUNTIME_EXCEPTION("function '" + toString(name) +
                               "' has no native callback or script jump target!");
    }
}

void Function::validateArguments(Stack::Slice args) const
{
    // No args or varargs validated by the function itself.
    if (argumentTypes == nullptr || argumentCount == 0)
    {
        return;
    }

    const UInt32 argsIn = args.getSize();
    if (argsIn != argumentCount)
    {
        MOON_RUNTIME_EXCEPTION("function '" + toString(name) + "' expected " +
                               toString(argumentCount) + " argument(s) but " +
                               toString(argsIn) + " where provided.");
    }

    for (UInt32 a = 0; a < argsIn; ++a)
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

void Function::validateReturnValue(const Variant retVal) const
{
    if (hasReturnVal() && (*returnType != retVal.type))
    {
        MOON_RUNTIME_EXCEPTION("function '" + toString(name) + "' was expected to return " +
                               toString(*returnType) + " but instead returned " + toString(retVal.type));
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

    os << strPrintF("| %-30s | %-9s | %-9s | %-6s | %s\n",
                    name->chars, argCountStr.c_str(), flagsStr.c_str(),
                    jmpTargetStr.c_str(), retTypeStr.c_str());
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
        ::operator delete(static_cast<void *>(funcObj)); // Raw delete. Doesn't call the destructor.
    }
}

const Function * FunctionTable::addFunction(ConstRcString * funcName, const Variant::Type * returnType,
                                            const Variant::Type * argTypes, const UInt32 argCount,
                                            const UInt32 jumpTarget, const UInt32 extraFlags,
                                            Function::NativeCB nativeCallback)
{
    MOON_ASSERT(isRcStringValid(funcName));
    MOON_ASSERT(!findFunction(funcName) && "Function already registered!");
    MOON_ASSERT(argCount <= Function::MaxArguments && "Max arguments per function exceeded!");

    const std::size_t extraTypes = (returnType != nullptr) ? 1 : 0;
    const std::size_t totalBytes = sizeof(Function) + ((argCount + extraTypes) * sizeof(Variant::Type));

    // We allocate the Function instance plus eventual argument type list and return
    // type as a single memory block. Function will be first, followed by N arguments
    // then the return type at the end.
    auto memPtr  = static_cast<UInt8 *>(::operator new(totalBytes));
    auto funcObj = reinterpret_cast<Function *>(memPtr);
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

    // The name pointer held by Function doesn't have to be refed.
    // The table already hold a ref and Functions belong to the table.
    construct(funcObj, { funcName, funcRet, funcArgs, argCount, jumpTarget, extraFlags, nativeCallback });
    return addInternal(funcName, funcObj);
}

const Function * FunctionTable::addFunction(const char * funcName, const Variant::Type * returnType,
                                            const Variant::Type * argTypes, const UInt32 argCount,
                                            const UInt32 jumpTarget, const UInt32 flags,
                                            Function::NativeCB nativeCallback)
{
    ConstRcStrUPtr key{ newConstRcString(funcName) };
    return addFunction(key.get(), returnType, argTypes, argCount, jumpTarget, flags, nativeCallback);
}

void FunctionTable::setJumpTargetFor(ConstRcString * const funcName, const UInt32 newTarget)
{
    // This is the only time besides when cleaning up when we need to unconst the
    // pointer, so it is still worth storing it as 'const Function*' in the table.
    auto func = const_cast<Function *>(findFunction(funcName));
    MOON_ASSERT(func != nullptr);
    func->jumpTarget = newTarget;
}

void FunctionTable::setJumpTargetFor(const char * funcName, const UInt32 newTarget)
{
    auto func = const_cast<Function *>(findFunction(funcName));
    MOON_ASSERT(func != nullptr);
    func->jumpTarget = newTarget;
}

void FunctionTable::print(std::ostream & os) const
{
    os << color::white() << "[[ begin function table dump ]]" << color::restore() << "\n";
    if (!isEmpty())
    {
        os << "+--------------------------------+-----------+-----------+--------+----------+\n";
        os << "| name                           | arg-count | flags     | jump   | ret-type |\n";
        os << "+--------------------------------+-----------+-----------+--------+----------+\n";
        for (auto && entry : *this)
        {
            entry.second->print(os);
        }
    }
    else
    {
        os << "(empty)\n";
    }
    os << color::white() << "[[ listed " << getSize() << " functions ]]" << color::restore() << "\n";
}

// ========================================================
// TypeTable:
// ========================================================

TypeTable::TypeTable(Object ** gcListHead)
{
    MOON_ASSERT(gcListHead != nullptr);

    // Register the built-ins:
    auto builtIns = getBuiltInTypeNames();
    for (int i = 0; builtIns[i].name != nullptr; ++i)
    {
        if (!builtIns[i].internalType)
        {
            Object * templateObj = nullptr;
            auto thisTid = addTypeId(builtIns[i].name.get(), builtIns[i].instCb, templateObj, true);

            // Once we added the new entry, we can now update the template
            // object instance passing to the factory callback this TypeId.
            if (builtIns[i].instCb)
            {
                templateObj = builtIns[i].instCb(thisTid, gcListHead);
                const_cast<TypeId *>(thisTid)->templateObject = templateObj;
                templateObj->setUpMembersTable();
            }
        }
    }

    stringTypeId = findTypeId("str");
    MOON_ASSERT(stringTypeId != nullptr);

    arrayTypeId = findTypeId("array");
    MOON_ASSERT(arrayTypeId != nullptr);
}

const TypeId * TypeTable::addTypeId(ConstRcString * name, ObjectFactoryCB instCb,
                                    const Object * templateObj, const bool isBuiltIn)
{
    MOON_ASSERT(!findTypeId(name) && "Type already registered!");
    // No need to add ref for the name instance held by the type.
    // The Registry already refs the string, so it is guaranteed
    // to live for as long as the stored TypeId.
    auto newTypeId = typeIdPool.allocate();
    return addInternal(name, construct(newTypeId, { name, instCb, templateObj, isBuiltIn, false }));
}

const TypeId * TypeTable::addTypeId(const char * name, ObjectFactoryCB instCb,
                                    const Object * templateObj, const bool isBuiltIn)
{
    ConstRcStrUPtr key{ newConstRcString(name) };
    return addTypeId(key.get(), instCb, templateObj, isBuiltIn);
}

const TypeId * TypeTable::addTypeAlias(const TypeId * existingType, ConstRcString * aliasName)
{
    MOON_ASSERT(existingType != nullptr);
    auto newEntry = const_cast<TypeId *>(addInternal(aliasName, existingType));
    newEntry->isTypeAlias = true;
    return newEntry;
}

const TypeId * TypeTable::addTypeAlias(const TypeId * existingType, const char * aliasName)
{
    ConstRcStrUPtr key{ newConstRcString(aliasName) };
    return addTypeAlias(existingType, key.get());
}

void TypeTable::print(std::ostream & os) const
{
    os << color::white() << "[[ begin type table dump ]]" << color::restore() << "\n";
    if (!isEmpty())
    {
        os << "+--------------------------------+----------+----------+----------+-------+\n";
        os << "| name                           | built-in | alloc-cb | template | alias |\n";
        os << "+--------------------------------+----------+----------+----------+-------+\n";
        for (auto && entry : *this)
        {
            const char * builtIn = (entry.second->isBuiltIn      ? "yes" : "no");
            const char * instCb  = (entry.second->createInstance ? "yes" : "no");
            const char * tplObj  = (entry.second->templateObject ? "yes" : "no");
            const char * alias   = (entry.second->isTypeAlias    ? entry.second->name->chars : "-----");

            os << strPrintF("| %-30s | %-8s | %-8s | %-8s | %s\n",
                            toString(entry.first).c_str(), builtIn, instCb, tplObj, alias);
        }
    }
    else
    {
        os << "(empty)\n";
    }
    os << color::white() << "[[ listed " << getSize() << " types ]]" << color::restore() << "\n";
}

// ========================================================
// Object:
// ========================================================

Object::Object(const TypeId * tid, Object ** gcListHead)
    : typeId{ tid }
{
    MOON_ASSERT(gcListHead != nullptr);

    next = (*gcListHead);
    (*gcListHead) = this;
}

Object::~Object()
{
    // We share ownership of the member name
    // strings, so they must be unrefed.
    const int memberCount = getMemberCount();
    for (int m = 0; m < memberCount; ++m)
    {
        releaseRcString(members[m].name);
    }
}

void Object::initialize(const Stack::Slice constructorArgs)
{
    setUpMembersTable();
    if (constructorArgs.getSize() != getMemberCount())
    {
        MOON_RUNTIME_EXCEPTION("'" + getTypeName() + "' constructor requires " +
                               toString(getMemberCount()) + " arguments, but " +
                               toString(constructorArgs.getSize()) + " where provided.");
    }

    const int memberCount = getMemberCount();
    for (int m = 0; m < memberCount; ++m)
    {
        performAssignmentWithConversion(members[m].data, constructorArgs[m]);
    }
}

void Object::setUpMembersTable()
{
    MOON_ASSERT(typeId != nullptr);
    if (typeId->templateObject == nullptr)
    {
        return;
    }

    const auto templateObj = typeId->templateObject;
    const int memberCount  = templateObj->getMemberCount();
    for (int m = 0; m < memberCount; ++m)
    {
        addMember(templateObj->members[m].name,
                  templateObj->members[m].data);
    }
}

void Object::print(std::ostream & os) const
{
    os << getStringRepresentation();
}

std::string Object::getTypeName() const
{
    return (typeId != nullptr) ? typeId->name->chars : "object";
}

std::string Object::getStringRepresentation() const
{
    std::string nameStr, typeStr, valStr, finalStr;
    const int memberCount = getMemberCount();

    // Add the object's own address:
    finalStr += strPrintF("[@%016llX]\n", static_cast<UInt64>(
                          reinterpret_cast<std::uintptr_t>(this)));

    finalStr += getTypeName();
    finalStr += "{";

    if (getMemberCount() > 0)
    {
        finalStr += "\n";
    }

    for (int m = 0; m < memberCount; ++m)
    {
        nameStr = toString(members[m].name);
        valStr  = toString(members[m].data);
        typeStr = toString(members[m].data.type);

        finalStr += "  ";
        finalStr += nameStr;
        finalStr += ": ";
        finalStr += typeStr;
        finalStr += " => ";
        finalStr += (members[m].data.type == Variant::Type::String) ?
                             unescapeString(valStr.c_str()) : valStr;
        finalStr += ",\n";
    }

    finalStr += "}\n";
    return finalStr;
}

void Object::addMember(ConstRcString * name, const Variant data)
{
    MOON_ASSERT(!hasMember(name) && "Object already has a member with the same name!");

    // Takes shared ownership of the name.
    addRcStringRef(name);
    members.push_back({ name, data });
}

void Object::addMember(const char * name, const Variant data)
{
    // This one allocates a new ref string from the C-string.
    ConstRcStrUPtr rstr{ newConstRcString(name) };
    return addMember(rstr.get(), data);
}

Variant Object::findMemberVar(ConstRcString * const name) const
{
    const int index = findMemberIndex(name);
    return (index >= 0) ? members[index].data : Variant{};
}

Variant Object::findMemberVar(const char * name) const
{
    const int index = findMemberIndex(name);
    return (index >= 0) ? members[index].data : Variant{};
}

int Object::findMemberIndex(ConstRcString * const name) const
{
    MOON_ASSERT(isRcStringValid(name));
    const int memberCount = getMemberCount();

    for (int m = 0; m < memberCount; ++m)
    {
        if (cmpRcStringsEqual(members[m].name, name))
        {
            return m;
        }
    }
    return -1;
}

int Object::findMemberIndex(const char * name) const
{
    const auto nameHash   = hashCString(name);
    const int memberCount = getMemberCount();

    for (int m = 0; m < memberCount; ++m)
    {
        if (members[m].name->hashVal == nameHash)
        {
            return m;
        }
    }
    return -1;
}

// ========================================================
// Runtime Object allocator:
// ========================================================

Object * newRuntimeObject(const TypeId * tid, Object ** gcListHead, Stack::Slice constructorArgs)
{
    MOON_ASSERT(tid != nullptr);
    MOON_ASSERT(gcListHead != nullptr);

    if (tid->createInstance == nullptr)
    {
        MOON_RUNTIME_EXCEPTION("can't dynamically allocate instance of type '" + toString(tid->name) + "'");
    }

    Object * inst = tid->createInstance(tid, gcListHead);
    inst->initialize(constructorArgs);
    return inst;
}

// ========================================================
// Script Struct:
// ========================================================

Object * Struct::createInstance(const TypeId * tid, Object ** gcListHead)
{
    return new Struct{ tid, gcListHead };
}

// ========================================================
// Script Str:
// ========================================================

Object * Str::createInstance(const TypeId * tid, Object ** gcListHead)
{
    return new Str{ tid, gcListHead };
}

Str * Str::newFromString(VM & vm, const std::string & str, const bool makeConst)
{
    return newFromString(vm, str.c_str(), str.length(), makeConst);
}

Str * Str::newFromString(VM & vm, const char * cstr, const UInt32 length, const bool makeConst)
{
    MOON_ASSERT(cstr != nullptr);
    auto newStr = static_cast<Str *>(Str::createInstance(vm.runtimeTypes.stringTypeId, &vm.gcListHead));

    // The size of the small internal buffer of a std::string should be more of less the
    // whole size of the type minus a length and pointer, likely sizeof(size_t) bytes each.
    constexpr auto minConstStrLen = sizeof(std::string) - (sizeof(std::size_t) * 2);

    // If the string is short enough to fit in the small string buffer of
    // std::string we ignore the user hint and avoid the extra allocation.
    if (makeConst && length >= minConstStrLen)
    {
        newStr->constString = newConstRcString(cstr, length);
    }
    else
    {
        if (length != 0)
        {
            newStr->mutableString.assign(cstr, length);
        }
    }

    return newStr;
}

Str * Str::newFromStrings(VM & vm, const Str & strA, const Str & strB, const bool makeConst)
{
    // Used for concatenating strings with operator +
    char temp[2048];
    constexpr int available = arrayLength(temp);
    const int length = std::snprintf(temp, available, "%s%s", strA.c_str(), strB.c_str());

    if (length < 0 || length >= available)
    {
        MOON_RUNTIME_EXCEPTION("buffer overflow concatenating strings!");
    }

    return newFromString(vm, temp, length, makeConst);
}

Variant Str::unaryOp(const OpCode op, const Str & str)
{
    if (op != OpCode::LogicNot)
    {
        MOON_RUNTIME_EXCEPTION("cannot apply unary op " + unaryOpToString(op) + " on string");
    }
    //
    // let foo = ""; // empty string
    // let foo: str; // null string
    //
    // if not foo then
    //   print("foo is null or empty");
    // end
    //
    Variant result{ Variant::Type::Integer };
    result.value.asInteger = str.isEmptyString();
    return result;
}

Variant Str::binaryOp(const OpCode op, const Str & strA, const Str & strB)
{
    Variant result{ Variant::Type::Integer };
    switch (op)
    {
    // Equal comparison:
    case OpCode::CmpNotEqual :
        result.value.asInteger = !strA.cmpEqual(strB);
        break;
    case OpCode::CmpEqual :
        result.value.asInteger = strA.cmpEqual(strB);
        break;

    // Lexicographical comparison:
    case OpCode::CmpGreaterEqual :
        result.value.asInteger = (strA.compare(strB) >= 0);
        break;
    case OpCode::CmpGreater :
        result.value.asInteger = (strA.compare(strB) > 0);
        break;
    case OpCode::CmpLessEqual :
        result.value.asInteger = (strA.compare(strB) <= 0);
        break;
    case OpCode::CmpLess :
        result.value.asInteger = (strA.compare(strB) < 0);
        break;

    // Logic and/or for empty string:
    case OpCode::LogicOr :
        result.value.asInteger = (!strA.isEmptyString()) || (!strB.isEmptyString());
        break;
    case OpCode::LogicAnd :
        result.value.asInteger = (!strA.isEmptyString()) && (!strB.isEmptyString());
        break;

    default :
        MOON_RUNTIME_EXCEPTION("cannot perform binary op " + binaryOpToString(op) + " on string");
    } // switch (op)

    return result;
}

int Str::compare(const Str & other) const noexcept
{
    if (isConstString() && other.isConstString())
    {
        return cmpRcStrings(constString, other.constString);
    }
    return std::strcmp(c_str(), other.c_str());
}

bool Str::cmpEqual(const Str & other) const noexcept
{
    // We can use the optimized hash comparison when testing with ==, !=.
    if (isConstString() && other.isConstString())
    {
        return cmpRcStringsEqual(constString, other.constString);
    }
    return std::strcmp(c_str(), other.c_str());
}

int Str::getStringLength() const noexcept
{
    if (isConstString())
    {
        return static_cast<int>(constString->length);
    }
    return static_cast<int>(mutableString.length());
}

bool Str::isEmptyString() const noexcept
{
    if (isConstString())
    {
        return constString->length == 0;
    }
    return mutableString.empty();
}

bool Str::isConstString() const noexcept
{
    return constString != nullptr;
}

const char * Str::c_str() const noexcept
{
    return (constString != nullptr) ? constString->chars : mutableString.c_str();
}

Str::~Str()
{
    if (isConstString())
    {
        releaseRcString(constString);
    }
}

// ========================================================
// Script Array:
// ========================================================

Object * Array::createInstance(const TypeId * tid, Object ** gcListHead)
{
    return new Array{ tid, gcListHead };
}

// ========================================================
// Script Enum:
// ========================================================

Object * Enum::createInstance(const TypeId * tid, Object ** gcListHead)
{
    return new Enum{ tid, gcListHead };
}

// ========================================================
// Built-in type names:
// ========================================================

const BuiltInTypeDesc * getBuiltInTypeNames()
{
    static BuiltInTypeDesc builtInTypeNames[]
    {
        // Built-in type names (same as the ones used in script code):
        { ConstRcStrUPtr{ newConstRcString( "int"       ) }, nullptr, false },
        { ConstRcStrUPtr{ newConstRcString( "long"      ) }, nullptr, false },
        { ConstRcStrUPtr{ newConstRcString( "float"     ) }, nullptr, false },
        { ConstRcStrUPtr{ newConstRcString( "bool"      ) }, nullptr, false },
        { ConstRcStrUPtr{ newConstRcString( "range"     ) }, nullptr, false },
        { ConstRcStrUPtr{ newConstRcString( "any"       ) }, nullptr, false },
        { ConstRcStrUPtr{ newConstRcString( "object"    ) }, nullptr, false },
        { ConstRcStrUPtr{ newConstRcString( "function"  ) }, nullptr, false },
        { ConstRcStrUPtr{ newConstRcString( "str"       ) }, &Str::createInstance,   false },
        { ConstRcStrUPtr{ newConstRcString( "array"     ) }, &Array::createInstance, false },
        // Internal types (not actual types usable in code):
        { ConstRcStrUPtr{ newConstRcString( "varargs"   ) }, nullptr, true },
        { ConstRcStrUPtr{ newConstRcString( "void"      ) }, nullptr, true },
        { ConstRcStrUPtr{ newConstRcString( "undefined" ) }, nullptr, true },
        // Marks the end of the list:
        { nullptr, nullptr, true }
    };
    return builtInTypeNames;
}

} // namespace moon {}
