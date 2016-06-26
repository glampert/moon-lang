
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
        ConstRcString * asString; // Reference held if type == StrLiteral | Identifier.

        Value() noexcept { asInteger = 0; }
    };

    ConstRcString * name;    // Pointer to the symbol name in the parent table. Not owned/referenced by Symbol.
    const Value     value;   // Intrinsic value of the symbol.
    const int       lineNum; // Line number in the source file where it was found. Negative for built-ins.
    const Type      type;    // Type that defines the symbol's value.

    // Value conversion helpers:
    static Value valueFromIntegerStr(const char * cstr);
    static Value valueFromFloatStr(const char * cstr);
    static Value valueFromBoolStr(const char * cstr);
    static Value valueFromRcStr(ConstRcString * rstr); // Adds a ref

    // Compare this type and value for equality with the provided params.
    bool cmpEqual(Type otherType, Value otherValue) const;

    // Compare the provide string (or hash thereof) for equality with this symbol's *name*.
    bool cmpEqual(const char * otherName) const;
    bool cmpEqual(const UInt32 otherNameHash) const noexcept
    {
        return name->hashVal == otherNameHash;
    }

    // Test if the symbol's name is equal to one of the built-in types, e.g.: int, float, string, etc.
    bool isBuiltInTypeId() const;

    // Print the symbol as a table row for use by the SymbolTable.
    void print(std::ostream & os) const;
};

std::string toString(const Symbol & symbol); // Symbol value to string.
std::string toString(Symbol::Type type);     // Symbol type to string.

// ========================================================
// class SymbolTable:
// ========================================================

class SymbolTable final
    : public Registry<const Symbol *>
{
public:

     SymbolTable();
    ~SymbolTable();

    // Symbol access by name/id. Null returned if not found.
    const Symbol * findSymbol(ConstRcString * const name) const { return findInternal(name); }
    const Symbol * findSymbol(const char * name) const          { return findInternal(name); }

    // Add new symbol. Symbol must NOT be in the table. Fails with an assertion
    // if a symbol with the same NAME is already present. Returns the new symbol.
    // Note: The symbol table will add ref to the name string, so generally you'll
    // want to pass this method a string acquired from newConstRcString().
    const Symbol * addSymbol(ConstRcString * name, int declLineNum, Symbol::Type type, Symbol::Value value);
    const Symbol * addSymbol(const char * name, int declLineNum, Symbol::Type type, Symbol::Value value);

    // Find a registered literal value with the equivalent string value or define a new one.
    // The name used to register the new literal will be auto-generated.
    const Symbol * findOrDefineLiteral(ConstRcString * valueStr, int declLineNum, Symbol::Type typeWanted);
    const Symbol * findOrDefineLiteral(const char * valueStr, int declLineNum, Symbol::Type typeWanted);

    // Find a registered identifier with the given name or define a new one.
    const Symbol * findOrDefineIdentifier(ConstRcString * name, int declLineNum);
    const Symbol * findOrDefineIdentifier(const char * name, int declLineNum);

    // Shorthand to define built-in strings or number values:
    const Symbol * findOrDefineIntLiteral(Int64 value);
    const Symbol * findOrDefineFloatLiteral(Float64 value);
    const Symbol * findOrDefineBoolLiteral(bool value);
    const Symbol * findOrDefineStrLiteral(const char * valueStr);

    // Print the whole table in table format (no pun intended).
    void print(std::ostream & os) const;

    // Auto-generated name for literal symbol types (IntLiteral, FloatLiteral, BoolLiteral, StrLiteral only).
    ConstRcStrUPtr generateSymbolName(Symbol::Type type);

private:

    // For generated names.
    UInt32 nextSymId = 0;

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
