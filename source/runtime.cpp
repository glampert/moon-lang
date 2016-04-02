
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
        var.value.asString = sym.value.asString;
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
        return toString(var.value.asFunction ? var.value.asFunction->name : "null");

    case Variant::Type::Tid :
        return toString(var.value.asTypeId ? var.value.asTypeId->name->chars : "null");

    case Variant::Type::Object :
        {
            auto addr = strPrintF(" @%016llX", static_cast<std::uint64_t>(
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
    static_assert(arrayLength(typeNames) == unsigned(Variant::Type::Count),
                  "Keep this array in sync with the enum declaration!");

    return typeNames[unsigned(type)];
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
                    name, argCountStr.c_str(), flagsStr.c_str(),
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
    auto memPtr  = static_cast<std::uint8_t *>(::operator new(totalBytes));
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

// ================================================================================================
// TODO Stuff below is still early work in progress...

void TypeTable::print(std::ostream & os) const
{
    os << color::white() << "[[ begin type table dump ]]" << color::restore() << "\n";
    if (!table.empty())
    {
        os << "+--------------------------------+----------+----------+----------+-------+\n";
        os << "| name                           | built-in | alloc-cb | template | alias |\n";
        os << "+--------------------------------+----------+----------+----------+-------+\n";
        for (auto && entry : table)
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
    os << color::white() << "[[ listed " << table.size() << " types ]]" << color::restore() << "\n";
}

//--------------

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
    finalStr += strPrintF("[@%016llX]\n", static_cast<std::uint64_t>(
                            reinterpret_cast<std::uintptr_t>(this)));

    finalStr += getTypeName();
    finalStr += "{\n";
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

//--------------

Object * LangStruct::createInstance(const TypeId * tid, Object ** gcListHead)
{
    return new LangStruct{ tid, gcListHead };
}
Object * LangString::createInstance(const TypeId * tid, Object ** gcListHead)
{
    return new LangString{ tid, gcListHead };
}
Object * LangArray::createInstance(const TypeId * tid, Object ** gcListHead)
{
    return new LangArray{ tid, gcListHead };
}
Object * LangEnum::createInstance(const TypeId * tid, Object ** gcListHead)
{
    return new LangEnum{ tid, gcListHead };
}

//--------------

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

//--------------

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
        { ConstRcStrUPtr{ newConstRcString( "str"       ) }, &LangString::createInstance, false },
        { ConstRcStrUPtr{ newConstRcString( "array"     ) }, &LangArray::createInstance,  false },
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
