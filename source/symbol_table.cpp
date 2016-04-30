
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
    valOut.asString = rstr; // NOTE: Ref not added by design.
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
    addStrLiteral("",       Symbol::LineNumBuiltIn);
    addIntLiteral("0",      Symbol::LineNumBuiltIn);
    addIntLiteral("1",      Symbol::LineNumBuiltIn);
    addBoolLiteral("true",  Symbol::LineNumBuiltIn);
    addBoolLiteral("false", Symbol::LineNumBuiltIn);
    addFloatLiteral("0.0",  Symbol::LineNumBuiltIn);
    addFloatLiteral("1.0",  Symbol::LineNumBuiltIn);

    auto builtIns = getBuiltInTypeNames();
    for (int i = 0; builtIns[i].name != nullptr; ++i)
    {
        addIdentifier(builtIns[i].name.get(), Symbol::LineNumBuiltIn);
    }
}

const Symbol * SymbolTable::findOrDefineIntValue(const Int64 value)
{
    auto key = toString(value);
    if (auto symbol = findSymbol(key.c_str()))
    {
        return symbol;
    }

    // Define new built-in:
    Symbol::Value intVal;
    intVal.asInteger = value;
    return addSymbol(key.c_str(), Symbol::LineNumBuiltIn, Symbol::Type::IntLiteral, intVal);
}

const Symbol * SymbolTable::findOrDefineStrValue(const char * value)
{
    if (auto symbol = findSymbol(value))
    {
        return symbol;
    }
    return addStrLiteral(value, Symbol::LineNumBuiltIn);
}

const Symbol * SymbolTable::findOrDefineIdentifier(const char * name)
{
    if (auto symbol = findSymbol(name))
    {
        return symbol;
    }
    return addIdentifier(name, Symbol::LineNumBuiltIn);
}

const Symbol * SymbolTable::addSymbol(ConstRcString * name, const int declLineNum,
                                      const Symbol::Type type, const Symbol::Value value)
{
    MOON_ASSERT(!findSymbol(name) && "Symbol already registered!");
    return addInternal(name, construct(symbolPool.allocate(), { name, value, declLineNum, type }));
}

const Symbol * SymbolTable::addSymbol(const char * name, const int declLineNum,
                                      const Symbol::Type type, const Symbol::Value value)
{
    ConstRcStrUPtr key{ newConstRcString(name) };
    return addSymbol(key.get(), declLineNum, type, value);
}

const Symbol * SymbolTable::addIdentifier(const char * value, const int declLineNum)
{
    ConstRcStrUPtr key{ newConstRcString(value) };
    return addSymbol(key.get(), declLineNum, Symbol::Type::Identifier, Symbol::valueFromRcStr(key.get()));
}

const Symbol * SymbolTable::addStrLiteral(const char * value, const int declLineNum)
{
    ConstRcStrUPtr key{ newConstRcString(value) };
    return addSymbol(key.get(), declLineNum, Symbol::Type::StrLiteral, Symbol::valueFromRcStr(key.get()));
}

const Symbol * SymbolTable::addIdentifier(ConstRcString * value, const int declLineNum)
{
    return addSymbol(value, declLineNum, Symbol::Type::Identifier, Symbol::valueFromRcStr(value));
}

const Symbol * SymbolTable::addIntLiteral(const char * value, const int declLineNum)
{
    return addSymbol(value, declLineNum, Symbol::Type::IntLiteral, Symbol::valueFromIntegerStr(value));
}

const Symbol * SymbolTable::addFloatLiteral(const char * value, const int declLineNum)
{
    return addSymbol(value, declLineNum, Symbol::Type::FloatLiteral, Symbol::valueFromFloatStr(value));
}

const Symbol * SymbolTable::addBoolLiteral(const char * value, const int declLineNum)
{
    return addSymbol(value, declLineNum, Symbol::Type::BoolLiteral, Symbol::valueFromBoolStr(value));
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
