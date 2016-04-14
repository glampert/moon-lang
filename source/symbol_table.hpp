
// ================================================================================================
// -*- C++ -*-
// File: symbol_table.hpp
// Author: Guilherme R. Lampert
// Created on: 13/06/15
// Brief: Defines the Symbol type and a Symbol Table.
// ================================================================================================

#ifndef MOON_SYMBOL_TABLE_HPP
#define MOON_SYMBOL_TABLE_HPP

#include "common.hpp"
#include "registry.hpp"
#include "pool.hpp"

#ifndef MOON_SYMBOL_POOL_GRANULARITY
    #define MOON_SYMBOL_POOL_GRANULARITY 512
#endif // MOON_SYMBOL_POOL_GRANULARITY

namespace moon
{

// ========================================================
// struct Symbol:
// ========================================================

struct Symbol final
{
    static constexpr int LineNumBuiltIn = -1;

    enum class Type : UInt8
    {
        Undefined,
        IntLiteral,
        FloatLiteral,
        BoolLiteral,
        StrLiteral,
        Identifier,

        // Number of types. Internal use.
        Count
    };

    union Value
    {
        Int64           asInteger;
        Float64         asFloat;
        bool            asBoolean;
        ConstRcString * asString;

        Value() noexcept { asInteger = 0; }
    };

    ConstRcString * name;    // Reference to the symbol name in the parent table. Not owned by Symbol.
    const Value     value;   // Intrinsic value of the symbol.
    const int       lineNum; // Line number in the source file where it was found. Negative for built-ins.
    const Type      type;    // Type that defines the symbol's value.

    // Value conversion helpers:
    static Value valueFromIntegerStr(const char * cstr);
    static Value valueFromFloatStr(const char * cstr);
    static Value valueFromBoolStr(const char * cstr);
    static Value valueFromRcStr(ConstRcString * rstr);

    // Compare this type and value for equality with the provided params.
    bool cmpEqual(Type otherType, Value otherValue) const noexcept;

    // Test if the symbol's name is equal to one of the built-in types, e.g.: int, float, string, etc.
    bool isBuiltInTypeId() const noexcept;

    // Print the symbol as a table row for use by the SymbolTable.
    void print(std::ostream & os) const;
};

std::string toString(const Symbol & sym); // Symbol value to string.
std::string toString(Symbol::Type type);  // Symbol type to string.

// ========================================================
// class SymbolTable:
// ========================================================

class SymbolTable final
    : public Registry<const Symbol *>
{
public:

    SymbolTable();

    // Symbol access by name/id. Null returned if not found.
    const Symbol * findSymbol(ConstRcString * const name) const { return findInternal(name); }
    const Symbol * findSymbol(const char * name) const          { return findInternal(name); }

    // Find equivalent constant in the table or define a new built-in literal value for it.
    const Symbol * findOrDefineIntValue(Int64 value);        // IntLiteral
    const Symbol * findOrDefineStrValue(const char * value); // StrLiteral

    // Add new symbol. Symbol must NOT be in the table. Fails with an assertion
    // if a symbol with the same NAME is already present. Returns the new symbol.
    // Note: The symbol table will add ref to the name string, so generally you'll
    // want to pass this method a string acquired from newConstRcString().
    const Symbol * addSymbol(ConstRcString * name, int declLineNum, Symbol::Type type, Symbol::Value value);
    const Symbol * addSymbol(const char * name, int declLineNum, Symbol::Type type, Symbol::Value value);

    // Add new literal constants. Constant must NOT be in the table. Fails with an assertion
    // if a symbol with the same VALUE is already present. All will return the new symbol.
    const Symbol * addIntLiteral(const char * value, int declLineNum);
    const Symbol * addFloatLiteral(const char * value, int declLineNum);
    const Symbol * addBoolLiteral(const char * value, int declLineNum);
    const Symbol * addStrLiteral(const char * value, int declLineNum);
    const Symbol * addIdentifier(const char * value, int declLineNum);
    const Symbol * addIdentifier(ConstRcString * value, int declLineNum);

    // Print the whole table in table format (no pun intended).
    void print(std::ostream & os) const;

private:

    // Symbols are allocated from here. The table holds pointers into this pool.
    Pool<Symbol, MOON_SYMBOL_POOL_GRANULARITY> symbolPool;
};

inline std::ostream & operator << (std::ostream & os, const SymbolTable & symTable)
{
    symTable.print(os);
    return os;
}

} // namespace moon {}

#endif // MOON_SYMBOL_TABLE_HPP
