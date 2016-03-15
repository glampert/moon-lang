
// ================================================================================================
// -*- C++ -*-
// File: symbol_table.cpp
// Author: Guilherme R. Lampert
// Created on: 14/06/15
// Brief: Defines the Symbol type and a Symbol Table.
// ================================================================================================

#include "symbol_table.hpp"

namespace moon
{

// ========================================================
// Symbol methods and helpers:
// ========================================================

Symbol::Symbol(const char * nameRef, const int declLineNum, const Type symType, const Value symVal) noexcept
    : name    { nameRef     }
    , value   { symVal      }
    , lineNum { declLineNum }
    , type    { symType     }
{
    MOON_ASSERT(name != nullptr);
}

bool Symbol::cmpEqual(const Type otherType, const Value otherValue) const noexcept
{
    if (type != otherType)
    {
        return false;
    }

    // Strings might require a full character-wise compare:
    if (type == Type::StrLiteral)
    {
        // Optimize for same memory:
        if (value.asStringPtr == otherValue.asStringPtr)
        {
            return true;
        }

        const char * otherStr = otherValue.asStringPtr;
        auto otherStrLen = std::strlen(otherStr);
        if (otherStrLen > 1)
        {
            // Avoid possible leading and training double quotes in the input:
            if (otherStr[otherStrLen - 1] == '"')
            {
                --otherStrLen;
            }
            if (otherStr[0] == '"')
            {
                --otherStrLen;
                ++otherStr;
            }
        }

        return std::strncmp(value.asStringPtr, otherStr, otherStrLen) == 0;
    }

    // Compare the widest integral values:
    return value.asInteger == otherValue.asInteger;
}

void Symbol::print(std::ostream & os) const
{
    std::string temp1;
    std::string temp2;
    char formatStr[512] = {'\0'};

    #define CASE(typeId, colorCmd, typeName, value)                                                  \
        case Symbol::Type::typeId :                                                                  \
        {                                                                                            \
            temp1 = (lineNum != LineNumBuiltIn) ? toString(lineNum) : "built-in";                    \
            temp2 = toString(value);                                                                 \
            std::snprintf(formatStr, arrayLength(formatStr), "| %-30s | %-8s | %s%-9s%s | %s\n",     \
                          name, temp1.c_str(), colorCmd, typeName, color::restore(), temp2.c_str()); \
        }                                                                                            \
        break

    #define CASE_DEFAULT()                                                                        \
        default :                                                                                 \
        {                                                                                         \
            std::snprintf(formatStr, arrayLength(formatStr), "| %-30s | %-8d | %s%-9s%s | ???\n", \
                          name, lineNum, color::red(), "undefined", color::restore());            \
        }                                                                                         \
        break

    // NOTE: This has to be updated if Symbol::Type changes!
    switch (type)
    {
        CASE( IntLiteral,   color::blue(),    "int",    value.asInteger   );
        CASE( FloatLiteral, color::yellow(),  "float",  value.asFloat     );
        CASE( BoolLiteral,  color::cyan(),    "bool",   value.asBoolean   );
        CASE( StrLiteral,   color::white(),   "string", value.asStringPtr );
        CASE( Identifier,   color::magenta(), "ident",  value.asStringPtr );
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

    valOut.asInteger = static_cast<LangLong>(std::strtol(cstr, &endPtr, 0));
    if (endPtr == nullptr || endPtr == cstr)
    {
        MOON_RUNTIME_EXCEPTION("String literal \"" + toString(cstr) +
                               "\" cannot be converted to an integer value.");
    }
    return valOut;
}

Symbol::Value Symbol::valueFromFloatStr(const char * cstr)
{
    MOON_ASSERT(cstr != nullptr);

    Symbol::Value valOut;
    char * endPtr = nullptr;

    valOut.asFloat = static_cast<LangFloat>(std::strtod(cstr, &endPtr));
    if (endPtr == nullptr || endPtr == cstr)
    {
        MOON_RUNTIME_EXCEPTION("String literal \"" + toString(cstr) +
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
        MOON_RUNTIME_EXCEPTION("Bad boolean literal string \"" + toString(cstr) +
                               "\". Must be either \"true\" or \"false\".");
    }
    return valOut;
}

Symbol::Value Symbol::valueFromCStr(const char * cstr)
{
    MOON_ASSERT(cstr != nullptr);
    Symbol::Value valOut;
    valOut.asStringPtr = cstr;
    return valOut;
}

std::ostream & operator << (std::ostream & os, const Symbol & sym)
{
    sym.print(os);
    return os;
}

std::string toString(const Symbol & sym)
{
    switch (sym.type)
    {
    case Symbol::Type::IntLiteral   : return toString(sym.value.asInteger);
    case Symbol::Type::FloatLiteral : return toString(sym.value.asFloat);
    case Symbol::Type::BoolLiteral  : return toString(sym.value.asBoolean);
    case Symbol::Type::StrLiteral   : return toString(sym.value.asStringPtr);
    case Symbol::Type::Identifier   : return toString(sym.value.asStringPtr);
    default                         : return "???";
    } // switch (sym.type)
}

// ========================================================
// findSymInTable():
// ========================================================

template
<
    Symbol::Type TypeTag,
    Symbol::Value (* ConvFunc)(const char *)
>
const Symbol * findSymInTable(const char * value, const SymbolTable::SymTable & table)
{
    MOON_ASSERT(value != nullptr);
    const auto testVal = ConvFunc(value);
    for (auto && entry : table)
    {
        if (entry.second->cmpEqual(TypeTag, testVal))
        {
            return entry.second;
        }
    }
    return nullptr;
}

// ========================================================
// SymbolTable class methods:
// ========================================================

SymbolTable::SymbolTable()
    : nextLiteralIndex { 0 }
{
    //
    // Since we consolidate repeated literal/constant values
    // under the same symbol whenever possible, we can predefine
    // a few defaults that will likely be used on most programs.
    //
    addIntLiteral("0",         Symbol::LineNumBuiltIn);
    addIntLiteral("1",         Symbol::LineNumBuiltIn);
    addBoolLiteral("true",     Symbol::LineNumBuiltIn);
    addBoolLiteral("false",    Symbol::LineNumBuiltIn);
    addFloatLiteral("0.0",     Symbol::LineNumBuiltIn);
    addFloatLiteral("1.0",     Symbol::LineNumBuiltIn);

    // Built-in type names:
    addIdentifier("int",       Symbol::LineNumBuiltIn);
    addIdentifier("long",      Symbol::LineNumBuiltIn);
    addIdentifier("float",     Symbol::LineNumBuiltIn);
    addIdentifier("bool",      Symbol::LineNumBuiltIn);
    addIdentifier("string",    Symbol::LineNumBuiltIn);
    addIdentifier("array",     Symbol::LineNumBuiltIn);
    addIdentifier("range",     Symbol::LineNumBuiltIn);
    addIdentifier("any",       Symbol::LineNumBuiltIn);

    // Internal types (not actual types usable in code):
    addIdentifier("object",    Symbol::LineNumBuiltIn);
    addIdentifier("varargs",   Symbol::LineNumBuiltIn);
    addIdentifier("void",      Symbol::LineNumBuiltIn);
    addIdentifier("undefined", Symbol::LineNumBuiltIn);
}

SymbolTable::~SymbolTable()
{
    // Symbol names are allocated dynamically by the Lexer,
    // passed to the parser via yylval and then given to the
    // table when a new symbol is encountered. The table is
    // the official owner of that memory after a new symbol
    // is added. So this is the proper place to free them.
    // TODO
    // either free the `name` pointers here
    // or do nothing if using a string stack!
    //
    // Also note that literals might have a generated name not coming from the lexer!!!
    // (thought they are always assumed to have been allocated with cloneCStringNoQuotes)
}

const Symbol * SymbolTable::findIntLiteral(const char * value) const
{
    return findSymInTable<Symbol::Type::IntLiteral, &Symbol::valueFromIntegerStr>(value, table);
}

const Symbol * SymbolTable::findFloatLiteral(const char * value) const
{
    return findSymInTable<Symbol::Type::FloatLiteral, &Symbol::valueFromFloatStr>(value, table);
}

const Symbol * SymbolTable::findBoolLiteral(const char * value) const
{
    return findSymInTable<Symbol::Type::BoolLiteral, &Symbol::valueFromBoolStr>(value, table);
}

const Symbol * SymbolTable::findStrLiteral(const char * value) const
{
    return findSymInTable<Symbol::Type::StrLiteral, &Symbol::valueFromCStr>(value, table);
}

const Symbol * SymbolTable::addIntLiteral(const char * value, const int declLineNum)
{
    MOON_ASSERT(!findIntLiteral(value));
    return addSymbol(makeLiteralConstName(Symbol::Type::IntLiteral), declLineNum,
                     Symbol::Type::IntLiteral, Symbol::valueFromIntegerStr(value));
}

const Symbol * SymbolTable::addFloatLiteral(const char * value, const int declLineNum)
{
    MOON_ASSERT(!findFloatLiteral(value));
    return addSymbol(makeLiteralConstName(Symbol::Type::FloatLiteral), declLineNum,
                     Symbol::Type::FloatLiteral, Symbol::valueFromFloatStr(value));
}

const Symbol * SymbolTable::addBoolLiteral(const char * value, const int declLineNum)
{
    MOON_ASSERT(!findBoolLiteral(value));
    return addSymbol(makeLiteralConstName(Symbol::Type::BoolLiteral), declLineNum,
                     Symbol::Type::BoolLiteral, Symbol::valueFromBoolStr(value));
}

const Symbol * SymbolTable::addStrLiteral(const char * value, const int declLineNum)
{
    MOON_ASSERT(!findStrLiteral(value));
    return addSymbol(makeLiteralConstName(Symbol::Type::StrLiteral), declLineNum,
                     Symbol::Type::StrLiteral, Symbol::valueFromCStr(cloneCStringNoQuotes(value)));
}

const Symbol * SymbolTable::findOrDefineValue(const LangLong value)
{
    Symbol::Value intVal;
    intVal.asInteger = value;

    for (auto && entry : table)
    {
        if (entry.second->cmpEqual(Symbol::Type::IntLiteral, intVal))
        {
            return entry.second; // Already have this value defined.
        }
    }

    // Define new built-in:
    return addSymbol(makeLiteralConstName(Symbol::Type::IntLiteral),
                     Symbol::LineNumBuiltIn, Symbol::Type::IntLiteral, intVal);
}

const Symbol * SymbolTable::addIdentifier(const char * value, const int declLineNum)
{
    const char * symName = cloneCStringNoQuotes(value);
    return addSymbol(symName, declLineNum, Symbol::Type::Identifier, Symbol::valueFromCStr(symName));
}

const Symbol * SymbolTable::findSymbol(const char * name) const
{
    MOON_ASSERT(name != nullptr);
    const auto it = table.find(name);
    if (it != std::end(table))
    {
        return it->second;
    }
    return nullptr; // Not found.
}

const Symbol * SymbolTable::addSymbol(const char * name, const int declLineNum,
                                      const Symbol::Type type, const Symbol::Value value)
{
    MOON_ASSERT(!findSymbol(name));
    Symbol * sym = symbolPool.allocate();
    construct(sym, name, declLineNum, type, value);
    table[name] = sym;
    return sym;
}

const char * SymbolTable::cloneCStringNoQuotes(const char * sourcePtr)
{
    MOON_ASSERT(sourcePtr != nullptr);

    auto sourceLen = std::strlen(sourcePtr);
    if (sourceLen == 0)
    {
        return getEmptyCString();
    }

    if (sourceLen > 1)
    {
        // Avoid possible leading and training double quotes:
        if (sourcePtr[sourceLen - 1] == '"')
        {
            --sourceLen;
        }
        if (sourcePtr[0] == '"')
        {
            ++sourcePtr;
            --sourceLen;
        }
    }

    auto newStr = new char[sourceLen + 1];
    std::memcpy(newStr, sourcePtr, sourceLen);
    newStr[sourceLen] = '\0';
    return newStr;
}

const char * SymbolTable::makeLiteralConstName(const Symbol::Type type)
{
    char temp[256] = {'\0'};
    const char * typeId;

    switch (type)
    {
    case Symbol::Type::IntLiteral   : { typeId = "I"; break; }
    case Symbol::Type::FloatLiteral : { typeId = "F"; break; }
    case Symbol::Type::BoolLiteral  : { typeId = "B"; break; }
    case Symbol::Type::StrLiteral   : { typeId = "S"; break; }
    case Symbol::Type::Identifier   : { typeId = "X"; break; }
    default                         : { typeId = "?"; break; }
    } // switch (type)

    std::snprintf(temp, arrayLength(temp), "$%s_lit_%u", typeId, nextLiteralIndex++);
    return cloneCStringNoQuotes(temp);
}

bool SymbolTable::isEmpty() const noexcept
{
    return table.empty();
}

std::size_t SymbolTable::getSize() const noexcept
{
    return table.size();
}

void SymbolTable::print(std::ostream & os) const
{
    os << color::white() << "[[ begin symbol table dump ]]" << color::restore() << "\n";
    if (!table.empty())
    {
        os << "+--------------------------------+----------+-----------+----------+\n";
        os << "| name                           | src-line | type      | value    |\n";
        os << "+--------------------------------+----------+-----------+----------+\n";
        for (auto && entry : table)
        {
            entry.second->print(os);
        }
    }
    else
    {
        os << "(empty)\n";
    }
    os << color::white() << "[[ listed " << table.size() << " symbols ]]" << color::restore() << "\n";
}

std::ostream & operator << (std::ostream & os, const SymbolTable & symTable)
{
    symTable.print(os);
    return os;
}

} // namespace moon {}
