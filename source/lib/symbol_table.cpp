
// ================================================================================================
// -*- C++ -*-
// File: symbol_table.cpp
// Author: Guilherme R. Lampert
// Created on: 14/06/15
// Brief: Defines the Symbol type and a Symbol Table.
// ================================================================================================

#include "symbol_table.hpp"
#include "runtime.hpp"

namespace moon
{

// ========================================================
// Symbol struct and helpers:
// ========================================================

bool Symbol::cmpEqual(const Type otherType, const Value otherValue) const
{
    if (type != otherType)
    {
        return false;
    }

    // Strings compare differently.
    if (type == Type::StrLiteral)
    {
        return cmpRcStringsEqual(value.asString, otherValue.asString);
    }

    // Compare the widest integral values:
    return value.asInteger == otherValue.asInteger;
}

bool Symbol::cmpEqual(const char * otherName) const
{
    MOON_ASSERT(otherName != nullptr);
    return std::strcmp(name->chars, otherName) == 0;
}

bool Symbol::isBuiltInTypeId() const
{
    auto builtIns = getBuiltInTypeNames();
    for (int i = 0; builtIns[i].name != nullptr; ++i)
    {
        if (cmpRcStringsEqual(name, builtIns[i].name.get()))
        {
            return true;
        }
    }
    return false;
}

void Symbol::print(std::ostream & os) const
{
    std::string temp1;
    std::string temp2;
    std::string formatStr;

    #define CASE(typeTag, colorCmd, typeName, value)                                                 \
        case Symbol::Type::typeTag :                                                                 \
        {                                                                                            \
            temp1 = (lineNum != LineNumBuiltIn) ? toString(lineNum) : "built-in";                    \
            temp2 = unescapeString(toString(value).c_str());                                         \
            if (temp2.empty()) { temp2 = "<empty>"; }                                                \
            formatStr = strPrintF("| %-30s | %-8s | %s%-9s%s | %s\n",                                \
                                  (!isRcStringEmpty(name) ? name->chars : "<empty>"), temp1.c_str(), \
                                  colorCmd, typeName, color::restore(), temp2.c_str());              \
        }                                                                                            \
        break

    #define CASE_DEFAULT()                                             \
        default :                                                      \
        {                                                              \
            formatStr = strPrintF("| %-30s | %-8d | %s%-9s%s | ???\n", \
                                  name->chars, lineNum, color::red(),  \
                                  "undefined", color::restore());      \
        }                                                              \
        break

    // NOTE: This has to be updated if Symbol::Type changes!
    switch (type)
    {
        CASE( IntLiteral,   color::blue(),    "int",    value.asInteger );
        CASE( FloatLiteral, color::yellow(),  "float",  value.asFloat   );
        CASE( BoolLiteral,  color::cyan(),    "bool",   value.asBoolean );
        CASE( StrLiteral,   color::white(),   "str",    value.asString  );
        CASE( Identifier,   color::magenta(), "ident",  value.asString  );
        CASE_DEFAULT( ); // Catches Type::Undefined and anything else.
    } // switch (type)

    os << formatStr;

    #undef CASE_DEFAULT
    #undef CASE
}

Symbol::Value Symbol::valueFromIntegerStr(const char * cstr)
{
    MOON_ASSERT(cstr != nullptr);

    Symbol::Value valOut;
    char * endPtr = nullptr;

    valOut.asInteger = static_cast<Int64>(std::strtol(cstr, &endPtr, 0));
    if (endPtr == nullptr || endPtr == cstr)
    {
        MOON_RUNTIME_EXCEPTION("string literal \"" + toString(cstr) +
                               "\" cannot be converted to an integer value.");
    }
    return valOut;
}

Symbol::Value Symbol::valueFromFloatStr(const char * cstr)
{
    MOON_ASSERT(cstr != nullptr);

    Symbol::Value valOut;
    char * endPtr = nullptr;

    valOut.asFloat = static_cast<Float64>(std::strtod(cstr, &endPtr));
    if (endPtr == nullptr || endPtr == cstr)
    {
        MOON_RUNTIME_EXCEPTION("string literal \"" + toString(cstr) +
                               "\" cannot be converted to a float value.");
    }
    return valOut;
}

Symbol::Value Symbol::valueFromBoolStr(const char * cstr)
{
    MOON_ASSERT(cstr != nullptr);

    Symbol::Value valOut;
    if (std::strcmp(cstr, "true") == 0)
    {
        valOut.asBoolean = true;
    }
    else if (std::strcmp(cstr, "false") == 0)
    {
        valOut.asBoolean = false;
    }
    else
    {
        MOON_RUNTIME_EXCEPTION("bad boolean literal string \"" + toString(cstr) +
                               "\"; must be either \"true\" or \"false\".");
    }
    return valOut;
}

Symbol::Value Symbol::valueFromRcStr(ConstRcString * rstr)
{
    MOON_ASSERT(isRcStringValid(rstr));

    Symbol::Value valOut;
    valOut.asString = rstr;
    addRcStringRef(rstr);
    return valOut;
}

std::string toString(const Symbol & symbol)
{
    switch (symbol.type)
    {
    case Symbol::Type::IntLiteral   : return toString(symbol.value.asInteger);
    case Symbol::Type::FloatLiteral : return toString(symbol.value.asFloat);
    case Symbol::Type::BoolLiteral  : return toString(symbol.value.asBoolean);
    case Symbol::Type::StrLiteral   : return toString(symbol.value.asString);
    case Symbol::Type::Identifier   : return toString(symbol.value.asString);
    default                         : return "???";
    } // switch (symbol.type)
}

std::string toString(const Symbol::Type type)
{
    static const std::string typeNames[]
    {
        color::red()     + std::string("undefined") + color::restore(),
        color::blue()    + std::string("int")       + color::restore(),
        color::yellow()  + std::string("float")     + color::restore(),
        color::cyan()    + std::string("bool")      + color::restore(),
        color::white()   + std::string("str")       + color::restore(),
        color::magenta() + std::string("ident")     + color::restore()
    };
    static_assert(arrayLength(typeNames) == UInt32(Symbol::Type::Count),
                  "Keep this array in sync with the enum declaration!");

    return typeNames[UInt32(type)];
}

// ========================================================
// SymbolTable:
// ========================================================

SymbolTable::SymbolTable()
{
    //
    // Since we consolidate repeated literal/constant values
    // under the same symbol whenever possible, we can predefine
    // a few defaults that will likely be used on most programs.
    //

    Symbol::Value v[7];
    v[0].asString  = newConstRcString("");
    v[1].asInteger = 0;
    v[2].asInteger = 1;
    v[3].asBoolean = true;
    v[4].asBoolean = false;
    v[5].asFloat   = 0.0;
    v[6].asFloat   = 1.0;

    addSymbol(generateSymbolName(Symbol::Type::StrLiteral).get(),   Symbol::LineNumBuiltIn, Symbol::Type::StrLiteral,   v[0]);
    addSymbol(generateSymbolName(Symbol::Type::IntLiteral).get(),   Symbol::LineNumBuiltIn, Symbol::Type::IntLiteral,   v[1]);
    addSymbol(generateSymbolName(Symbol::Type::IntLiteral).get(),   Symbol::LineNumBuiltIn, Symbol::Type::IntLiteral,   v[2]);
    addSymbol(generateSymbolName(Symbol::Type::BoolLiteral).get(),  Symbol::LineNumBuiltIn, Symbol::Type::BoolLiteral,  v[3]);
    addSymbol(generateSymbolName(Symbol::Type::BoolLiteral).get(),  Symbol::LineNumBuiltIn, Symbol::Type::BoolLiteral,  v[4]);
    addSymbol(generateSymbolName(Symbol::Type::FloatLiteral).get(), Symbol::LineNumBuiltIn, Symbol::Type::FloatLiteral, v[5]);
    addSymbol(generateSymbolName(Symbol::Type::FloatLiteral).get(), Symbol::LineNumBuiltIn, Symbol::Type::FloatLiteral, v[6]);

    auto builtIns = getBuiltInTypeNames();
    for (int i = 0; builtIns[i].name != nullptr; ++i)
    {
        // Same name and value for identifiers.
        Symbol::Value value = Symbol::valueFromRcStr(builtIns[i].name.get());
        addSymbol(builtIns[i].name.get(), Symbol::LineNumBuiltIn, Symbol::Type::Identifier, value);
    }
}

SymbolTable::~SymbolTable()
{
    // Need to release the strings for StrLiteral and Identifier types.
    for (auto && entry : *this)
    {
        auto symbol = entry.second;
        if (symbol->type == Symbol::Type::StrLiteral ||
            symbol->type == Symbol::Type::Identifier)
        {
            releaseRcString(symbol->value.asString);
        }
    }
}

const Symbol * SymbolTable::addSymbol(ConstRcString * name, const int declLineNum, const Symbol::Type type, const Symbol::Value value)
{
    MOON_ASSERT(!findSymbol(name) && "Symbol already registered!");
    return addInternal(name, construct(symbolPool.allocate(), { name, value, declLineNum, type }));
}

const Symbol * SymbolTable::addSymbol(const char * const name, const int declLineNum, const Symbol::Type type, const Symbol::Value value)
{
    ConstRcStrUPtr key{ newConstRcString(name) };
    return addSymbol(key.get(), declLineNum, type, value);
}

const Symbol * SymbolTable::findOrDefineLiteral(ConstRcString * valueStr, const int declLineNum, const Symbol::Type typeWanted)
{
    MOON_ASSERT(isRcStringValid(valueStr));
    MOON_ASSERT(typeWanted != Symbol::Type::Identifier && "Use SymbolTable::findOrDefineIdentifier() instead!");

    Symbol::Value value;
    switch (typeWanted)
    {
    case Symbol::Type::IntLiteral :
        value = Symbol::valueFromIntegerStr(valueStr->chars);
        break;
    case Symbol::Type::FloatLiteral :
        value = Symbol::valueFromFloatStr(valueStr->chars);
        break;
    case Symbol::Type::BoolLiteral :
        value = Symbol::valueFromBoolStr(valueStr->chars);
        break;
    case Symbol::Type::StrLiteral :
        value.asString = valueStr; // No ref needed just for comparing.
        break;
    default :
        MOON_UNREACHABLE();
    } // switch (typeWanted)

    // Try to find existing:
    for (auto && entry : *this)
    {
        if (entry.second->cmpEqual(typeWanted, value))
        {
            return entry.second;
        }
    }

    // Nothing found, define a new one:
    if (typeWanted == Symbol::Type::StrLiteral)
    {
        addRcStringRef(value.asString); // Must add a ref for storing.
    }
    return addSymbol(generateSymbolName(typeWanted).get(), declLineNum, typeWanted, value);
}

const Symbol * SymbolTable::findOrDefineLiteral(const char * const valueStr, const int declLineNum, const Symbol::Type typeWanted)
{
    MOON_ASSERT(valueStr != nullptr);
    MOON_ASSERT(typeWanted != Symbol::Type::Identifier && "Use SymbolTable::findOrDefineIdentifier() instead!");

    Symbol::Value value;
    ConstRcString tempRcStr;

    switch (typeWanted)
    {
    case Symbol::Type::IntLiteral :
        value = Symbol::valueFromIntegerStr(valueStr);
        break;
    case Symbol::Type::FloatLiteral :
        value = Symbol::valueFromFloatStr(valueStr);
        break;
    case Symbol::Type::BoolLiteral :
        value = Symbol::valueFromBoolStr(valueStr);
        break;
    case Symbol::Type::StrLiteral :
        // No ref needed just for comparing, so use a stack temporary:
        tempRcStr.chars    = valueStr;
        tempRcStr.length   = static_cast<UInt32>(std::strlen(valueStr));
        tempRcStr.hashVal  = hashCString(valueStr);
        tempRcStr.refCount = 1;
        value.asString     = &tempRcStr;
        break;
    default :
        MOON_UNREACHABLE();
    } // switch (typeWanted)

    // Try to find existing:
    for (auto && entry : *this)
    {
        if (entry.second->cmpEqual(typeWanted, value))
        {
            return entry.second;
        }
    }

    // Nothing found, define a new one:
    if (typeWanted == Symbol::Type::StrLiteral)
    {
        // Copy the RC string now:
        value.asString = newConstRcString(valueStr); // Ref = 1
    }
    return addSymbol(generateSymbolName(typeWanted).get(), declLineNum, typeWanted, value);
}

const Symbol * SymbolTable::findOrDefineIdentifier(ConstRcString * name, const int declLineNum)
{
    if (auto symbol = findSymbol(name))
    {
        MOON_ASSERT(symbol->type == Symbol::Type::Identifier);
        return symbol;
    }

    // Name and string value are the same for identifiers.
    return addSymbol(name, declLineNum, Symbol::Type::Identifier, Symbol::valueFromRcStr(name));
}

const Symbol * SymbolTable::findOrDefineIdentifier(const char * const name, const int declLineNum)
{
    if (auto symbol = findSymbol(name))
    {
        MOON_ASSERT(symbol->type == Symbol::Type::Identifier);
        return symbol;
    }

    // Name and string value are the same for identifiers.
    ConstRcStrUPtr rstr{ newConstRcString(name) };
    return addSymbol(rstr.get(), declLineNum, Symbol::Type::Identifier, Symbol::valueFromRcStr(rstr.get()));
}

const Symbol * SymbolTable::findOrDefineIntLiteral(const Int64 value)
{
    char valueStr[128];
    std::snprintf(valueStr, arrayLength(valueStr), MOON_I64_PRINT_FMT, value);
    return findOrDefineLiteral(valueStr, Symbol::LineNumBuiltIn, Symbol::Type::IntLiteral);
}

const Symbol * SymbolTable::findOrDefineFloatLiteral(const Float64 value)
{
    char valueStr[128];
    std::snprintf(valueStr, arrayLength(valueStr), "%lf", value);
    return findOrDefineLiteral(valueStr, Symbol::LineNumBuiltIn, Symbol::Type::FloatLiteral);
}

const Symbol * SymbolTable::findOrDefineBoolLiteral(const bool value)
{
    return findOrDefineLiteral((value ? "true" : "false"), Symbol::LineNumBuiltIn, Symbol::Type::BoolLiteral);
}

const Symbol * SymbolTable::findOrDefineStrLiteral(const char * const valueStr)
{
    return findOrDefineLiteral(valueStr, Symbol::LineNumBuiltIn, Symbol::Type::StrLiteral);
}

ConstRcStrUPtr SymbolTable::generateSymbolName(const Symbol::Type type)
{
    char tempStr[128];
    switch (type)
    {
    case Symbol::Type::IntLiteral :
        std::snprintf(tempStr, arrayLength(tempStr), "$I_%u", nextSymId++);
        break;
    case Symbol::Type::FloatLiteral :
        std::snprintf(tempStr, arrayLength(tempStr), "$F_%u", nextSymId++);
        break;
    case Symbol::Type::BoolLiteral :
        std::snprintf(tempStr, arrayLength(tempStr), "$B_%u", nextSymId++);
        break;
    case Symbol::Type::StrLiteral :
        std::snprintf(tempStr, arrayLength(tempStr), "$S_%u", nextSymId++);
        break;
    default :
        MOON_UNREACHABLE();
    } // switch (typeWanted)

    return ConstRcStrUPtr{ newConstRcString(tempStr) };
}

void SymbolTable::print(std::ostream & os) const
{
    os << color::white() << "[[ begin symbol table dump ]]" << color::restore() << "\n";
    if (!isEmpty())
    {
        os << "+--------------------------------+----------+-----------+----------+\n";
        os << "| name                           | src-line | type      | value    |\n";
        os << "+--------------------------------+----------+-----------+----------+\n";
        for (auto && entry : *this)
        {
            entry.second->print(os);
        }
    }
    else
    {
        os << "(empty)\n";
    }
    os << color::white() << "[[ listed " << getSize() << " symbols ]]" << color::restore() << "\n";
}

} // namespace moon {}
