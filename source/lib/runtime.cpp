
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
// Variant printing helpers:
// ========================================================

std::string toString(const Variant var)
{
    switch (var.type)
    {
    case Variant::Type::Integer :
        {
            return toString(var.value.asInteger);
        }
    case Variant::Type::Float :
        {
            return toString(var.value.asFloat);
        }
    case Variant::Type::Function :
        {
            const auto fn = var.value.asFunction;
            return toString(fn != nullptr ? fn->name->chars : "null");
        }
    case Variant::Type::Tid :
        {
            const auto tid = var.value.asTypeId;
            return toString(tid != nullptr ? tid->name->chars : "null");
        }
    case Variant::Type::Str :
        {
            const auto str = var.value.asString;
            return toString(str != nullptr ? str->c_str() : "null");
        }
    case Variant::Type::Object :
        {
            const auto obj = var.value.asObject;
            if (obj == nullptr) { return "null"; }
            return strPrintF("%s %s",
                   obj->getTypeName().c_str(),
                   obj->getAddressString().c_str());
        }
    case Variant::Type::Array :
        {
            const auto arr = var.value.asArray;
            if (arr == nullptr) { return "null"; }
            std::string dataTypeId = arr->getDataTypeString();
            if (color::colorPrintEnabled()) // Don't want color coding for this print.
            {
                dataTypeId = dataTypeId.substr(7, dataTypeId.length() - 6 - 7);
            }
            return strPrintF("array(%s;%i) %s", dataTypeId.c_str(),
                             arr->getArrayLength(), arr->getAddressString().c_str());
        }
    case Variant::Type::Range :
        {
            const auto r = var.value.asRange;
            return strPrintF("range(%i..%i)", r.begin, r.end);
        }
    case Variant::Type::Any :
        {
            std::string anyTypeId = toString(var.anyType);
            if (color::colorPrintEnabled()) // Don't want color coding for this print.
            {
                anyTypeId = anyTypeId.substr(7, anyTypeId.length() - 6 - 7);
            }
            return strPrintF("any(tid=%s)", anyTypeId.c_str());
        }
    default :
        return "null";
    } // switch (var.type)
}

std::string toString(const Variant::Type type)
{
    static const std::string typeNames[]
    {
        color::red()     + std::string("null")     + color::restore(),
        color::blue()    + std::string("int")      + color::restore(),
        color::yellow()  + std::string("float")    + color::restore(),
        color::green()   + std::string("function") + color::restore(),
        color::cyan()    + std::string("tid")      + color::restore(),
        color::white()   + std::string("str")      + color::restore(),
        color::magenta() + std::string("object")   + color::restore(),
        color::yellow()  + std::string("array")    + color::restore(),
        color::white()   + std::string("range")    + color::restore(),
        color::blue()    + std::string("any")      + color::restore()
    };
    static_assert(arrayLength(typeNames) == UInt32(Variant::Type::Count),
                  "Keep this array in sync with the enum declaration!");

    return typeNames[UInt32(type)];
}

// ========================================================
// Function:
// ========================================================

void Function::invoke(VM & vm, const Stack::Slice args) const
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
        #if MOON_SAVE_SCRIPT_CALLSTACK
        Variant funcVar{ Variant::Type::Function };
        funcVar.value.asFunction = this;
        vm.callstack.push(funcVar);
        #endif // MOON_SAVE_SCRIPT_CALLSTACK

        // Call in the native code:
        nativeCallback(vm, args);

        // Validate the return value here because we will not hit
        // a FuncEnd instruction when returning from a native call.
        validateReturnValue(vm.getReturnValue());

        #if MOON_SAVE_SCRIPT_CALLSTACK
        vm.callstack.pop();
        #endif // MOON_SAVE_SCRIPT_CALLSTACK
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

void Function::validateArguments(const Stack::Slice args) const
{
    std::string errorMessage;
    if (!validateArguments(args, errorMessage))
    {
        MOON_RUNTIME_EXCEPTION(errorMessage);
    }
}

bool Function::validateArguments(const Stack::Slice args, std::string & errorMessageOut) const
{
    // varargs validated by the function itself.
    if (isVarArgs())
    {
        return true;
    }

    const UInt32 argsIn = args.getSize();
    if (argsIn != argumentCount)
    {
        errorMessageOut = "function '" + toString(name) + "' expected " +
                          toString(argumentCount) + " argument(s) but " +
                          toString(argsIn) + " where provided.";
        return false;
    }

    // Check the types:
    for (UInt32 a = 0; a < argsIn; ++a)
    {
        const Variant::Type typeIn = args[a].type;
        const Variant::Type typeExpected = argumentTypes[a];

        // Allow implicit conversions between numerical types.
        if (!isAssignmentValid(typeExpected, typeIn))
        {
            errorMessageOut = "function '" + toString(name) + "' expected " +
                              toString(typeExpected) + " for argument " + toString(a + 1) +
                              " but " + toString(typeIn) + " was provided.";
            return false;
        }
    }

    return true; // Valid call.
}

void Function::validateReturnValue(const Variant retVal) const
{
    std::string errorMessage;
    if (!validateReturnValue(retVal.type, errorMessage))
    {
        MOON_RUNTIME_EXCEPTION(errorMessage);
    }
}

bool Function::validateReturnValue(const Variant::Type retValType, std::string & errorMessageOut) const
{
    if (hasReturnVal() && !isAssignmentValid(*returnType, retValType))
    {
        errorMessageOut = "function '" + toString(name) + "' was expected to return " +
                          toString(*returnType) + " but instead returned " + toString(retValType);
        return false;
    }
    return true;
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
        (*funcRet) = (*returnType);
        /*memPtr += sizeof(Variant::Type);*/
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

TypeTable::TypeTable(VM & vm)
{
    // Register the built-ins:
    auto builtIns = getBuiltInTypeNames();
    for (int i = 0; builtIns[i].name != nullptr; ++i)
    {
        if (!builtIns[i].internalType)
        {
            Object * templateObj = nullptr;
            auto thisTid = addTypeId(builtIns[i].name.get(), builtIns[i].newInstance, templateObj, true);

            // Once we added the new entry, we can now update the template
            // object instance passing to the factory callback this TypeId.
            if (builtIns[i].newInstance)
            {
                templateObj = builtIns[i].newInstance(vm, thisTid);
                templateObj->setUpMembersTable();
                templateObj->markTypeTemplate();

                const_cast<TypeId *>(thisTid)->templateObject = templateObj;
            }
        }
    }

    // Null is a special case. It is not a type in its own right,
    // but a literal value. However, it still requires a dummy type id.
    nullTypeId = addTypeId("null", nullptr, nullptr, true);
    MOON_ASSERT(nullTypeId != nullptr);

    // These are used frequently, so we profit from caching them.
    intTypeId      = findTypeId("int");      MOON_ASSERT(intTypeId      != nullptr);
    longTypeId     = findTypeId("long");     MOON_ASSERT(longTypeId     != nullptr);
    floatTypeId    = findTypeId("float");    MOON_ASSERT(floatTypeId    != nullptr);
    doubleTypeId   = findTypeId("double");   MOON_ASSERT(doubleTypeId   != nullptr);
    boolTypeId     = findTypeId("bool");     MOON_ASSERT(boolTypeId     != nullptr);
    rangeTypeId    = findTypeId("range");    MOON_ASSERT(rangeTypeId    != nullptr);
    anyTypeId      = findTypeId("any");      MOON_ASSERT(anyTypeId      != nullptr);
    objectTypeId   = findTypeId("object");   MOON_ASSERT(objectTypeId   != nullptr);
    functionTypeId = findTypeId("function"); MOON_ASSERT(functionTypeId != nullptr);
    strTypeId      = findTypeId("str");      MOON_ASSERT(strTypeId      != nullptr);
    arrayTypeId    = findTypeId("array");    MOON_ASSERT(arrayTypeId    != nullptr);
    tidTypeId      = findTypeId("tid");      MOON_ASSERT(tidTypeId      != nullptr);
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
            const char * instCb  = (entry.second->newInstance    ? "yes" : "no");
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

#if MOON_PRINT_RT_OBJECT_FLAGS
static void appendObjFlags(const Object & obj, std::string & strOut)
{
    strOut += "  flags => '";
    if (obj.isAlive)       { strOut += "A"; }
    else                   { strOut += "D"; }
    if (obj.isPersistent)  { strOut += "P"; }
    if (obj.isTemplateObj) { strOut += "T"; }
    if (obj.isBuiltInType) { strOut += "B"; }
    if (obj.isStructType)  { strOut += "S"; }
    if (obj.isEnumType)    { strOut += "E"; }
    if (obj.isReachable)   { strOut += "R"; }
    strOut += "',\n";
}
#endif // MOON_PRINT_RT_OBJECT_FLAGS

// ========================================================
// Object:
// ========================================================

Object::Object(const TypeId * tid) noexcept
    : isAlive       { true    }
    , isPersistent  { false   }
    , isTemplateObj { false   }
    , isBuiltInType { false   }
    , isStructType  { false   }
    , isEnumType    { false   }
    , isReachable   { false   }
    , isSmall       { false   }
    , typeId        { tid     }
    , gcNext        { nullptr }
{
}

Object::~Object()
{
    // We share ownership of the member name
    // strings, so they must be unrefed now.
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
        bool fail = true;

        #if MOON_ALLOW_UNINITIALIZED_MEMBERS
        if (constructorArgs.getSize() < getMemberCount())
        {
            fail = false;
        }
        #endif // MOON_ALLOW_UNINITIALIZED_MEMBERS

        if (fail)
        {
            MOON_RUNTIME_EXCEPTION("'" + getTypeName() + "' constructor requires " +
                                   toString(getMemberCount()) + " arguments, but " +
                                   toString(constructorArgs.getSize()) + " where provided.");
        }
    }

    const int memberCount = constructorArgs.getSize();
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

std::string Object::getAddressString() const
{
    return strPrintF("[@" MOON_X64_PRINT_FMT "]", static_cast<UInt64>(reinterpret_cast<std::uintptr_t>(this)));
}

std::string Object::getStringRepresentation() const
{
    std::string nameStr, typeStr, valStr, finalStr;
    const int memberCount = getMemberCount();

    // Add the object's own address:
    finalStr += getAddressString() + "\n";
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
        finalStr += (members[m].data.type == Variant::Type::Str) ?
                          unescapeString(valStr.c_str()) : valStr;
        finalStr += ",\n";
    }

    #if MOON_PRINT_RT_OBJECT_FLAGS
    appendObjFlags(*this, finalStr);
    #endif // MOON_PRINT_RT_OBJECT_FLAGS

    finalStr += "}\n";
    return finalStr;
}

void Object::addMember(ConstRcString * name, const Variant data)
{
    MOON_ASSERT(!hasMember(name) && "Object already has a member with the same name!");
    // Takes shared ownership of the name.
    members.push_back({ addRcStringRef(name), data });
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

Object * newRuntimeObject(VM & vm, const TypeId * tid, const Stack::Slice constructorArgs)
{
    MOON_ASSERT(tid != nullptr);
    if (tid->newInstance == nullptr)
    {
        MOON_RUNTIME_EXCEPTION("can't dynamically allocate instance of type '" + toString(tid->name) + "'");
    }

    Object * inst = tid->newInstance(vm, tid);
    inst->initialize(constructorArgs);
    return inst;
}

void freeRuntimeObject(VM &, Object * obj)
{
    if (obj == nullptr)
    {
        return;
    }

    // Not actually freed now. Next run of the GC will reclaim it.
    obj->isAlive     = false;
    obj->isReachable = false;
}

// ========================================================
// Script Struct and Enum allocators:
// ========================================================

Object * Struct::newInstance(VM & vm, const TypeId * tid)
{
    auto obj = vm.gc.alloc<Struct>(tid);
    obj->isStructType = true;
    return obj;
}

Object * Enum::newInstance(VM & vm, const TypeId * tid)
{
    auto obj = vm.gc.alloc<Enum>(tid);
    obj->isEnumType = true;
    return obj;
}

// ========================================================
// Script String:
// ========================================================

Object * Str::newInstance(VM & vm, const TypeId * tid)
{
    auto obj = vm.gc.alloc<Str>(tid);
    obj->isBuiltInType = true;
    return obj;
}

Str * Str::newFromString(VM & vm, const std::string & str, const bool makeConst)
{
    return newFromString(vm, str.c_str(), str.length(), makeConst);
}

Str * Str::newFromString(VM & vm, const char * cstr, const UInt32 length, const bool makeConst)
{
    MOON_ASSERT(cstr != nullptr);
    auto newStr = static_cast<Str *>(Str::newInstance(vm, vm.types.strTypeId));

    // The size of the small internal buffer of a std::string should be more of less the
    // whole size of the type minus a length and pointer, likely sizeof(size_t) bytes each.
    constexpr UInt32 minConstStrLen = UInt32(sizeof(std::string)) - (UInt32(sizeof(std::size_t)) * 2);

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

Str * Str::newFromString(VM & vm, ConstRcString * rstr)
{
    MOON_ASSERT(isRcStringValid(rstr));
    auto newStr = static_cast<Str *>(Str::newInstance(vm, vm.types.strTypeId));
    newStr->constString = addRcStringRef(rstr);
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

Str * Str::newNullString(VM & vm)
{
    return Str::newFromString(vm, "(null)", std::strlen("(null)"), false);
}

Variant Str::unaryOp(const OpCode op, const Str & str)
{
    if (op != OpCode::LogicNot)
    {
        MOON_RUNTIME_EXCEPTION("cannot apply unary op " + unaryOpToString(op) + " on string");
    }
    //
    // let foo = ""; // empty string
    // let foo: str; // null string object
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

    // Logic AND/OR for empty string:
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

int Str::compare(const Str & other) const
{
    if (isConstString() && other.isConstString())
    {
        return cmpRcStrings(constString, other.constString);
    }
    return std::strcmp(c_str(), other.c_str());
}

bool Str::cmpEqual(const Str & other) const
{
    // We can use the optimized hash comparison when testing with ==, !=.
    if (isConstString() && other.isConstString())
    {
        return cmpRcStringsEqual(constString, other.constString);
    }
    return std::strcmp(c_str(), other.c_str()) == 0;
}

std::string Str::getStringRepresentation() const
{
    // Add the object's own address:
    std::string finalStr = getAddressString() + "\n";
    finalStr += getTypeName();
    finalStr += "{\n";

    finalStr += "  length => " + toString(getStringLength()) + ",\n";
    finalStr += "  is_const => " + toString(isConstString() ? "yes" : "no") + ",\n";
    finalStr += "  chars => \"" + unescapeString(c_str()) + "\",\n";

    #if MOON_PRINT_RT_OBJECT_FLAGS
    appendObjFlags(*this, finalStr);
    #endif // MOON_PRINT_RT_OBJECT_FLAGS

    finalStr += "}\n";
    return finalStr;
}

void Str::clear()
{
    if (isConstString())
    {
        releaseRcString(constString);
        constString = nullptr;
    }
    else
    {
        mutableString.clear();
    }
}

Str::~Str()
{
    if (isConstString())
    {
        releaseRcString(constString);
    }
}

// ========================================================
// Internal Array helpers:
// ========================================================

static inline Variant packVar(const UInt8 * ptr, const UInt32 dataSize, const Variant::Type dataType)
{
    Variant result{ dataType };
    switch (dataType)
    {
    // Numbers:
    case Variant::Type::Integer :
        {
            switch (dataSize)
            {
            case sizeof(Int8)  : result.value.asInteger = *reinterpret_cast<const Int8  *>(ptr); break;
            case sizeof(Int32) : result.value.asInteger = *reinterpret_cast<const Int32 *>(ptr); break;
            case sizeof(Int64) : result.value.asInteger = *reinterpret_cast<const Int64 *>(ptr); break;
            default : MOON_UNREACHABLE();
            } // switch (dataSize)
            break;
        }
    case Variant::Type::Float :
        {
            switch (dataSize)
            {
            case sizeof(Float32) : result.value.asFloat = *reinterpret_cast<const Float32 *>(ptr); break;
            case sizeof(Float64) : result.value.asFloat = *reinterpret_cast<const Float64 *>(ptr); break;
            default : MOON_UNREACHABLE();
            } // switch (dataSize)
            break;
        }
    // All pointers:
    case Variant::Type::Null     :
    case Variant::Type::Function :
    case Variant::Type::Tid      :
    case Variant::Type::Str      :
    case Variant::Type::Object   :
    case Variant::Type::Array    :
        {
            // Only one of the cases is true, of course,
            // but this keeps the code 32/64bits portable.
            switch (dataSize)
            {
            case sizeof(UInt32) : result.value.asInteger = *reinterpret_cast<const UInt32 *>(ptr); break;
            case sizeof(UInt64) : result.value.asInteger = *reinterpret_cast<const UInt64 *>(ptr); break;
            default : MOON_UNREACHABLE();
            } // switch (dataSize)
            break;
        }
    // Range (2 integers):
    case Variant::Type::Range :
        {
            MOON_ASSERT(dataSize == sizeof(Range));
            result.value.asRange.begin = reinterpret_cast<const Range *>(ptr)->begin;
            result.value.asRange.end   = reinterpret_cast<const Range *>(ptr)->end;
            break;
        }
    // Any (copy as a full Variant):
    case Variant::Type::Any :
        {
            MOON_ASSERT(dataSize == sizeof(Variant));
            result = *reinterpret_cast<const Variant *>(ptr);
            break;
        }
    default :
        MOON_UNREACHABLE();
    } // switch (dataType)
    return result;
}

static inline void unpackVar(UInt8 * ptr, const UInt32 dataSize, const Variant src)
{
    switch (src.type)
    {
    // Numbers:
    case Variant::Type::Integer :
        {
            switch (dataSize)
            {
            case sizeof(Int8)  : *reinterpret_cast<Int8  *>(ptr) = static_cast<Int8 >(src.value.asInteger); break;
            case sizeof(Int32) : *reinterpret_cast<Int32 *>(ptr) = static_cast<Int32>(src.value.asInteger); break;
            case sizeof(Int64) : *reinterpret_cast<Int64 *>(ptr) = static_cast<Int64>(src.value.asInteger); break;
            default : MOON_UNREACHABLE();
            } // switch (dataSize)
            break;
        }
    case Variant::Type::Float :
        {
            switch (dataSize)
            {
            case sizeof(Float32) : *reinterpret_cast<Float32 *>(ptr) = static_cast<Float32>(src.value.asFloat); break;
            case sizeof(Float64) : *reinterpret_cast<Float64 *>(ptr) = static_cast<Float64>(src.value.asFloat); break;
            default : MOON_UNREACHABLE();
            } // switch (dataSize)
            break;
        }
    // All pointers:
    case Variant::Type::Null     :
    case Variant::Type::Function :
    case Variant::Type::Tid      :
    case Variant::Type::Str      :
    case Variant::Type::Object   :
    case Variant::Type::Array    :
        {
            // Only one of the cases is true, of course,
            // but this keeps the code 32/64bits portable.
            switch (dataSize)
            {
            case sizeof(UInt32) : *reinterpret_cast<UInt32 *>(ptr) = static_cast<UInt32>(src.value.asInteger); break;
            case sizeof(UInt64) : *reinterpret_cast<UInt64 *>(ptr) = static_cast<UInt64>(src.value.asInteger); break;
            default : MOON_UNREACHABLE();
            } // switch (dataSize)
            break;
        }
    // Range (2 integers):
    case Variant::Type::Range :
        {
            MOON_ASSERT(dataSize == sizeof(Range));
            reinterpret_cast<Range *>(ptr)->begin = src.value.asRange.begin;
            reinterpret_cast<Range *>(ptr)->end   = src.value.asRange.end;
            break;
        }
    // Any (copy as a full Variant):
    case Variant::Type::Any :
        {
            MOON_ASSERT(dataSize == sizeof(Variant));
            *reinterpret_cast<Variant *>(ptr) = src;
            break;
        }
    default :
        MOON_UNREACHABLE();
    } // switch (src.type)
}

// ========================================================
// Script Array:
// ========================================================

Object * Array::newInstance(VM & vm, const TypeId * tid)
{
    auto obj = vm.gc.alloc<Array>(tid);
    obj->isBuiltInType = true;
    return obj;
}

Array * Array::newEmpty(VM & vm, const TypeId * dataType, const int capacityHint)
{
    auto newArray = static_cast<Array *>(Array::newInstance(vm, vm.types.arrayTypeId));
    newArray->setItemTypeSize(dataType);
    newArray->reserveCapacity(capacityHint);
    return newArray;
}

Array * Array::newFromArgs(VM & vm, const TypeId * dataType, const Stack::Slice args)
{
    auto newArray = static_cast<Array *>(Array::newInstance(vm, vm.types.arrayTypeId));

    const int argCount = args.getSize();
    newArray->setItemTypeSize(dataType);
    newArray->reserveCapacity(argCount);

    for (int i = 0; i < argCount; ++i)
    {
        newArray->push(args[i]);
    }

    return newArray;
}

Array * Array::newFromRawData(VM & vm, const TypeId * dataType, const void * data, const int lengthInItems,
                              const int sizeBytesPerItem, const Variant::Type varType)
{
    MOON_ASSERT(data != nullptr);
    auto newArray = static_cast<Array *>(Array::newInstance(vm, vm.types.arrayTypeId));
    newArray->setItemTypeSize(dataType);
    newArray->push(data, lengthInItems, sizeBytesPerItem, varType);
    return newArray;
}

void Array::push(const Variant var)
{
    MOON_ASSERT(itemSize > 0);
    MOON_ASSERT(UInt32(itemSize) <= sizeof(Variant));

    if (!isAssignmentValid(varType, var.type))
    {
        MOON_RUNTIME_EXCEPTION("trying to push " + toString(var.type) +
                               " into array of " + toString(varType));
    }

    reserveCapacity(arrayLen + 1);

    Variant temp{ varType };
    performAssignmentWithConversion(temp, var);
    unpackVar(getDataPtr() + (arrayLen * itemSize), itemSize, temp);

    ++arrayLen;
}

void Array::push(const Array & other)
{
    if (!isAssignmentValid(varType, other.varType))
    {
        MOON_RUNTIME_EXCEPTION("trying to append array of " + toString(other.varType) +
                               " into array of " + toString(varType));
    }

    reserveCapacity(arrayLen + other.arrayLen);
    appendRawData(other.getDataPtr(), other.arrayLen, other.itemSize, other.varType);
}

void Array::push(const void * data, const int lengthInItems,
                 const int sizeBytesPerItem, const Variant::Type dataType)
{
    if (!isAssignmentValid(varType, dataType))
    {
        MOON_RUNTIME_EXCEPTION("trying to push " + toString(dataType) +
                               " into array of " + toString(varType));
    }

    reserveCapacity(arrayLen + lengthInItems);
    appendRawData(data, lengthInItems, sizeBytesPerItem, dataType);
}

Variant Array::getIndex(const int index) const
{
    MOON_ASSERT(itemSize > 0);
    MOON_ASSERT(UInt32(itemSize) <= sizeof(Variant));

    if (index < 0 || index >= arrayLen)
    {
        MOON_RUNTIME_EXCEPTION("array index " + toString(index) +
                               " is out-of-bound for array of length " +
                               toString(arrayLen));
    }

    return packVar(getDataPtr() + (index * itemSize), itemSize, varType);
}

void Array::setIndex(const int index, const Variant var)
{
    MOON_ASSERT(itemSize > 0);
    MOON_ASSERT(UInt32(itemSize) <= sizeof(Variant));

    if (index < 0 || index >= arrayLen)
    {
        MOON_RUNTIME_EXCEPTION("array index " + toString(index) +
                               " is out-of-bound for array of length " +
                               toString(arrayLen));
    }

    if (!isAssignmentValid(varType, var.type))
    {
        MOON_RUNTIME_EXCEPTION("trying to insert " + toString(var.type) +
                               " into array of " + toString(varType));
    }

    Variant temp{ varType };
    performAssignmentWithConversion(temp, var);
    unpackVar(getDataPtr() + (index * itemSize), itemSize, temp);
}

void Array::setItemTypeSize(const TypeId * tid)
{
    MOON_ASSERT(tid != nullptr && isRcStringValid(tid->name));

    using VT = Variant::Type;
    const auto hashId = tid->name->hashVal;

    if      (ct::hashCString( "int"      ) == hashId) { varType = VT::Integer;  itemSize = sizeof(Int32);      }
    else if (ct::hashCString( "long"     ) == hashId) { varType = VT::Integer;  itemSize = sizeof(Int64);      }
    else if (ct::hashCString( "float"    ) == hashId) { varType = VT::Float;    itemSize = sizeof(Float32);    }
    else if (ct::hashCString( "double"   ) == hashId) { varType = VT::Float;    itemSize = sizeof(Float64);    }
    else if (ct::hashCString( "bool"     ) == hashId) { varType = VT::Integer;  itemSize = sizeof(Int8);       }
    else if (ct::hashCString( "range"    ) == hashId) { varType = VT::Range;    itemSize = sizeof(Range);      }
    else if (ct::hashCString( "any"      ) == hashId) { varType = VT::Any;      itemSize = sizeof(Variant);    }
    else if (ct::hashCString( "object"   ) == hashId) { varType = VT::Object;   itemSize = sizeof(Object *);   }
    else if (ct::hashCString( "function" ) == hashId) { varType = VT::Function; itemSize = sizeof(Function *); }
    else if (ct::hashCString( "str"      ) == hashId) { varType = VT::Str;      itemSize = sizeof(Str *);      }
    else if (ct::hashCString( "array"    ) == hashId) { varType = VT::Array;    itemSize = sizeof(Array *);    }
    else if (ct::hashCString( "tid"      ) == hashId) { varType = VT::Tid;      itemSize = sizeof(TypeId *);   }
    else
    {
        MOON_RUNTIME_EXCEPTION("bad variable type id '" + toString(tid->name) +
                               "' (" + strPrintF("0x%08X", hashId) + ")");
    }
}

void Array::reserveCapacity(const int capacityHint)
{
    if (capacityHint <= getArrayCapacity())
    {
        return;
    }

    auto vec = castTo<VecType>();
    if (!isVector)
    {
        // Save the old in-place data buffer:
        const Backing oldData = backingStore;
        const Int32   oldLen  = arrayLen;

        // Blast the old data into space...
        std::memset(&backingStore, 0, sizeof(backingStore));
        arrayLen = 0;

        // New vector gets placement newed in the old in-place backing buffer:
        construct(vec);
        isVector = true;

        // Copy the old data back in:
        vec->resize(capacityHint * itemSize);
        appendRawData(&oldData, oldLen, itemSize, varType);
    }
    else // Already in vector mode.
    {
        vec->resize(capacityHint * itemSize);
    }
}

int Array::getArrayCapacity() const noexcept
{
    MOON_ASSERT(itemSize > 0);
    MOON_ASSERT(UInt32(itemSize) <= sizeof(Variant));

    const UInt32 bytesAllocated = (isVector ? castTo<VecType>()->size() : sizeof(backingStore));
    return static_cast<int>(bytesAllocated / itemSize);
}

void Array::appendRawData(const void * data, const int lengthInItems,
                          const int sizeBytesPerItem, const Variant::Type dataType)
{
    MOON_ASSERT(data != nullptr);
    MOON_ASSERT(sizeBytesPerItem > 0);

    MOON_ASSERT(itemSize > 0);
    MOON_ASSERT(UInt32(itemSize) <= sizeof(Variant));
    MOON_ASSERT((getArrayCapacity() - arrayLen) >= lengthInItems);

    if (lengthInItems <= 0)
    {
        return;
    }

    // If everything matches, we can do a fast memcpy.
    if (varType == dataType && itemSize == sizeBytesPerItem)
    {
        std::memcpy(getDataPtr() + (arrayLen * itemSize), data, lengthInItems * itemSize);
    }
    else // Otherwise, implicit conversions are at play.
    {
        UInt8 * dstDataPtr             = getDataPtr() + (arrayLen * itemSize);
        const UInt32 dstItemSize       = itemSize;
        const Variant::Type dstVarType = varType;

        const UInt8 * srcDataPtr       = reinterpret_cast<const UInt8 *>(data);
        const UInt32 srcItemSize       = sizeBytesPerItem;
        const Variant::Type srcVarType = dataType;

        Variant dstVar{ dstVarType };
        Variant srcVar{ srcVarType };

        for (int i = 0; i < lengthInItems; ++i)
        {
            srcVar = packVar(srcDataPtr + (i * srcItemSize), srcItemSize, srcVarType);
            performAssignmentWithConversion(dstVar, srcVar);
            unpackVar(dstDataPtr + (i * dstItemSize), dstItemSize, dstVar);
        }
    }

    arrayLen += lengthInItems;
}

std::string Array::getStringRepresentation() const
{
    // Add the object's own address:
    std::string finalStr = getAddressString() + "\n";
    finalStr += getTypeName();
    finalStr += "{\n";

    finalStr += "  length => " + toString(arrayLen) + ",\n";
    finalStr += "  item_size => " + toString(itemSize) + ",\n";
    finalStr += "  item_type => " + toString(varType)  + ",\n";
    finalStr += "  dynamic => " + toString(isDynamicArray() ? "yes" : "no") + ",\n";

    const UInt32 capBytes = isVector ? castTo<VecType>()->capacity() : sizeof(backingStore);
    finalStr += "  capacity => " + toString((itemSize > 0) ? capBytes / itemSize : 0u) + ",\n";
    finalStr += "  mem_bytes => " + toString(capBytes) + ",\n";

    #if MOON_PRINT_RT_OBJECT_FLAGS
    appendObjFlags(*this, finalStr);
    #endif // MOON_PRINT_RT_OBJECT_FLAGS

    finalStr += "}\n";
    return finalStr;
}

Array::~Array()
{
    if (isVector)
    {
        destroy(castTo<VecType>());
    }
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
        { ConstRcStrUPtr{ newConstRcString( "double"    ) }, nullptr, false },
        { ConstRcStrUPtr{ newConstRcString( "bool"      ) }, nullptr, false },
        { ConstRcStrUPtr{ newConstRcString( "range"     ) }, nullptr, false },
        { ConstRcStrUPtr{ newConstRcString( "any"       ) }, nullptr, false },
        { ConstRcStrUPtr{ newConstRcString( "object"    ) }, nullptr, false },
        { ConstRcStrUPtr{ newConstRcString( "function"  ) }, nullptr, false },
        { ConstRcStrUPtr{ newConstRcString( "tid"       ) }, nullptr, false },
        { ConstRcStrUPtr{ newConstRcString( "str"       ) }, &Str::newInstance,   false },
        { ConstRcStrUPtr{ newConstRcString( "array"     ) }, &Array::newInstance, false },
        // Internal types (not actual types usable in code):
        { ConstRcStrUPtr{ newConstRcString( "varargs"   ) }, nullptr, true },
        { ConstRcStrUPtr{ newConstRcString( "void"      ) }, nullptr, true },
        { ConstRcStrUPtr{ newConstRcString( "undefined" ) }, nullptr, true },
        { ConstRcStrUPtr{ newConstRcString( "null"      ) }, nullptr, true },
        // Marks the end of the list:
        { nullptr, nullptr, true }
    };
    return builtInTypeNames;
}

// ========================================================
// Minimal unit tests for Array and Str:
// ========================================================

#if MOON_DEBUG
namespace
{
struct RuntimeSupportTests
{
    RuntimeSupportTests()
    {
        VM vm;
        testScriptArray(vm);
        testScriptString(vm);
        logStream() << "Moon: Runtime support tests passed.\n";
    }

    static void testScriptString(VM & vm)
    {
        // Small string allocated into the in-place buffer (not a ConstRcString).
        auto str1 = Str::newFromString(vm, "hello-", false);

        // Initial invariants:
        MOON_ASSERT(std::strcmp(str1->c_str(), "hello-") == 0);
        MOON_ASSERT(str1->isEmptyString()   == false);
        MOON_ASSERT(str1->isConstString()   == false);
        MOON_ASSERT(str1->getStringLength() == 6);

        // Share string reference:
        ConstRcString * rstr = newConstRcString("this is a ref counted string");
        auto str2 = Str::newFromString(vm, rstr);

        MOON_ASSERT(std::strcmp(str2->c_str(), rstr->chars) == 0);
        MOON_ASSERT(str2->isEmptyString()   == false);
        MOON_ASSERT(str2->isConstString()   == true);
        MOON_ASSERT(str2->getStringLength() == rstr->length);

        // Append into new string:
        auto str3 = Str::newFromStrings(vm, *str1, *str2, false);
        const char result[] = "hello-this is a ref counted string";
        MOON_ASSERT(std::strcmp(str3->c_str(), result) == 0);
        MOON_ASSERT(str3->isEmptyString()   == false);
        MOON_ASSERT(str3->isConstString()   == false);
        MOON_ASSERT(str3->getStringLength() == std::strlen(result));

        // Comparisons:
        MOON_ASSERT(str1->cmpEqual(*str2) == false);
        MOON_ASSERT(str2->cmpEqual(*str3) == false);
        MOON_ASSERT(str1->compare(*str2)  != 0);
        MOON_ASSERT(str2->compare(*str3)  != 0);

        // Binary ops:
        MOON_ASSERT(Str::binaryOp(OpCode::LogicOr,  *str1, *str2).toBool() == true);
        MOON_ASSERT(Str::binaryOp(OpCode::LogicAnd, *str1, *str2).toBool() == true);
        str1->clear();
        MOON_ASSERT(Str::binaryOp(OpCode::LogicOr,  *str1, *str2).toBool() == true);
        MOON_ASSERT(Str::binaryOp(OpCode::LogicAnd, *str1, *str2).toBool() == false);
        str2->clear();
        MOON_ASSERT(Str::binaryOp(OpCode::LogicOr,  *str1, *str2).toBool() == false);
        MOON_ASSERT(Str::binaryOp(OpCode::LogicAnd, *str1, *str2).toBool() == false);

        MOON_ASSERT(str1->isEmptyString() == true);
        MOON_ASSERT(str2->isEmptyString() == true);
        MOON_ASSERT(str3->isEmptyString() == false);

        releaseRcString(rstr);
        freeRuntimeObject(vm, str1);
        freeRuntimeObject(vm, str2);
        freeRuntimeObject(vm, str3);

        logStream() << "Moon: Script String test passed.\n";
    }

    static void testScriptArray(VM & vm)
    {
        // Array of Int32's (intTypeId="int")
        auto array = Array::newEmpty(vm, vm.types.intTypeId, /* capacityHint = */ 5);

        // Initial invariants:
        MOON_ASSERT(array->isEmptyArray()     == true);
        MOON_ASSERT(array->isSmallArray()     == true);
        MOON_ASSERT(array->isDynamicArray()   == false);
        MOON_ASSERT(array->getArrayLength()   == 0);
        MOON_ASSERT(array->getItemSize()      == sizeof(Int32));
        MOON_ASSERT(array->getArrayCapacity() >= 0);

        constexpr int M = 5;
        constexpr int N = 15;
        const Int32 testData[M] = { 1, 2, 3, 4, 5 };

        // Append the contents of another array:
        auto other = Array::newFromRawData(vm, vm.types.intTypeId,
                                           testData, arrayLength(testData),
                                           sizeof(Int32), Variant::Type::Integer);

        MOON_ASSERT(other->getItemSize()    == sizeof(Int32));
        MOON_ASSERT(other->getArrayLength() == arrayLength(testData));
        array->push(*other);

        // Append some more items:
        for (int i = 0; i < N; ++i)
        {
            Variant var{ Variant::Type::Integer };
            var.value.asInteger = i + M + 1;
            array->push(var);
        }

        MOON_ASSERT(array->isEmptyArray()     == false);
        MOON_ASSERT(array->isSmallArray()     == false);
        MOON_ASSERT(array->isDynamicArray()   == true);
        MOON_ASSERT(array->getArrayLength()   == N + other->getArrayLength());
        MOON_ASSERT(array->getItemSize()      == sizeof(Int32));
        MOON_ASSERT(array->getArrayCapacity() >= N);

        // Change backing store capacity:
        array->reserveCapacity(128);
        MOON_ASSERT(array->isSmallArray()     == false);
        MOON_ASSERT(array->isDynamicArray()   == true);
        MOON_ASSERT(array->getArrayLength()   == N + other->getArrayLength());
        MOON_ASSERT(array->getArrayCapacity() >= 128);

        // Validate:
        for (int i = 0; i < array->getArrayLength(); ++i)
        {
            Variant var = array->getIndex(i);
            MOON_ASSERT(var.type == Variant::Type::Integer);
            MOON_ASSERT(var.value.asInteger == i + 1);
        }

        // Pop values:
        array->pop(1);
        array->pop(2);
        array->pop(2);
        MOON_ASSERT(array->getArrayLength() == N + other->getArrayLength() - 5);

        // Reset every remaining element:
        for (int i = 0; i < array->getArrayLength(); ++i)
        {
            Variant var{ Variant::Type::Integer };
            var.value.asInteger = 42;
            array->setIndex(i, var);
        }
        // Validate the rest:
        for (int i = 0; i < array->getArrayLength(); ++i)
        {
            Variant var = array->getIndex(i);
            MOON_ASSERT(var.type == Variant::Type::Integer);
            MOON_ASSERT(var.value.asInteger == 42);
        }

        array->clear();
        MOON_ASSERT(array->isEmptyArray()   == true);
        MOON_ASSERT(array->isDynamicArray() == true); // Once it goes dynamic, it never returns to small-array
        MOON_ASSERT(array->getArrayLength() == 0);
        MOON_ASSERT(array->getItemSize()    == sizeof(Int32));

        // Now let's try pushing a few floats to test the implicit type conversion:
        for (int i = 0; i < N; ++i)
        {
            Variant var{ Variant::Type::Float };
            var.value.asFloat = static_cast<Float64>(i) + 0.1;
            array->push(var);
        }
        // And validate (now truncated integers):
        for (int i = 0; i < N; ++i)
        {
            Variant var = array->getIndex(i);
            MOON_ASSERT(var.type == Variant::Type::Integer);
            MOON_ASSERT(var.value.asInteger == i);
        }

        // And finally, test the elusive 'Any' type:
        auto anyArray = Array::newEmpty(vm, vm.types.anyTypeId, /* capacityHint = */ 2);
        {
            // Put a few integers:
            for (int i = 0; i < N; ++i)
            {
                Variant var{ Variant::Type::Integer };
                var.value.asInteger = i;
                anyArray->push(var);
            }
            // A few floats...
            for (int i = 0; i < N; ++i)
            {
                Variant var{ Variant::Type::Float };
                var.value.asFloat = static_cast<Float64>(i) + 0.5;
                anyArray->push(var);
            }

            // A couple ranges:
            Variant r{ Variant::Type::Range };
            r.value.asRange = Range{ 0, 10 };
            anyArray->push(r);
            r.value.asRange = Range{ -1, 1 };
            anyArray->push(r);

            // And finally, other Anys:
            Variant a{ Variant::Type::Any };
            a.anyType = Variant::Type::Integer;
            a.value.asInteger = 0xAA;
            anyArray->push(a);
            a.anyType = Variant::Type::Float;
            a.value.asFloat = 3.14;
            anyArray->push(a);

            // Now validate the above:
            int i = 0;
            for (int j = 0; j < N; ++j)
            {
                Variant var = anyArray->getIndex(i++);
                MOON_ASSERT(var.type    == Variant::Type::Any);
                MOON_ASSERT(var.anyType == Variant::Type::Integer);
                MOON_ASSERT(var.value.asInteger == j);
            }
            for (int j = 0; j < N; ++j)
            {
                Variant var = anyArray->getIndex(i++);
                MOON_ASSERT(var.type    == Variant::Type::Any);
                MOON_ASSERT(var.anyType == Variant::Type::Float);
                MOON_ASSERT(var.value.asFloat == static_cast<Float64>(j) + 0.5);
            }

            // Two ranges and two more numbers:
            const Variant r0 = anyArray->getIndex(i++);
            const Variant r1 = anyArray->getIndex(i++);
            MOON_ASSERT(r0.type    == Variant::Type::Any);
            MOON_ASSERT(r0.anyType == Variant::Type::Range);
            MOON_ASSERT(r1.type    == Variant::Type::Any);
            MOON_ASSERT(r1.anyType == Variant::Type::Range);
            MOON_ASSERT(r0.value.asRange.begin ==  0 && r0.value.asRange.end == 10);
            MOON_ASSERT(r1.value.asRange.begin == -1 && r1.value.asRange.end == 1);

            const Variant a0 = anyArray->getIndex(i++);
            const Variant a1 = anyArray->getIndex(i++);
            MOON_ASSERT(a0.type    == Variant::Type::Any);
            MOON_ASSERT(a0.anyType == Variant::Type::Integer);
            MOON_ASSERT(a1.type    == Variant::Type::Any);
            MOON_ASSERT(a1.anyType == Variant::Type::Float);
            MOON_ASSERT(a0.value.asInteger == 0xAA);
            MOON_ASSERT(a1.value.asFloat   == 3.14);
        }

        freeRuntimeObject(vm, array);
        freeRuntimeObject(vm, other);
        freeRuntimeObject(vm, anyArray);

        logStream() << "Moon: Script Array test passed.\n";
    }
} localRuntimeSupportTests;
} // namespace {}
#endif // MOON_DEBUG

} // namespace moon {}
