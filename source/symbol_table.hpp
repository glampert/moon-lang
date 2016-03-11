
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
#include "pool.hpp"

#include <iostream>

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

    enum class Type : std::uint8_t
    {
        Undefined,
        IntLiteral,
        FloatLiteral,
        BoolLiteral,
        StrLiteral,
        Identifier
    };

    union Value
    {
        LangLong     asInteger;
        LangFloat    asFloat;
        LangBool     asBoolean;
        const char * asStringPtr;

        Value() noexcept { asInteger = 0; }
    };

    static Value valueFromIntegerStr(const char * cstr);
    static Value valueFromFloatStr(const char * cstr);
    static Value valueFromBoolStr(const char * cstr);
    static Value valueFromCStr(const char * cstr);

    const char * const name;    // Reference to the symbol name in the parent table. Not owned by Symbol.
    const Value        value;   // Intrinsic value of the symbol.
    const int          lineNum; // Line number in the source file where it was found. Negative for built-ins.
    const Type         type;    // Type that defines the symbol's value.

    // Symbols are normally only created by the SymbolTable.
    Symbol(const char * nameRef, int declLineNum, Type symType, Value symVal) noexcept;

    // Compare this type and value for equality with the provided params.
    bool cmpEqual(Type otherType, Value otherValue) const noexcept;

    // Print the symbol as a table row for use by the SymbolTable.
    void print(std::ostream & os = std::cout) const;
};

std::string toString(const Symbol & sym);                           // Symbol *value* to string.
std::ostream & operator << (std::ostream & os, const Symbol & sym); // Calls Symbol::print().

// ========================================================
// class SymbolTable:
// ========================================================

class SymbolTable final
{
public:

    using SymTable = HashTable<const Symbol *>;

     SymbolTable();
    ~SymbolTable();

    // Not copyable.
    SymbolTable(const SymbolTable &) = delete;
    SymbolTable & operator = (const SymbolTable &) = delete;

    // Symbol access by name/id. Null returned if not found.
    const Symbol * findSymbol(const char * name) const;

    // Find equivalent constant in the table or define a new built-in literal value for it.
    const Symbol * findOrDefineValue(LangLong value);

    // Add new symbol. Symbol must NOT be in the table. Fails with an assertion
    // if a symbol with the same NAME is already present. Returns the new symbol.
    // Note: The symbol table will take ownership of the name string, so generally
    // you'll want to pass this method a string acquired from cloneCStringNoQuotes().
    const Symbol * addSymbol(const char * name, int declLineNum, Symbol::Type type, Symbol::Value value);

    // Find symbols generate from literal constants by value.
    // The input of the query is the symbol's VALUE as a string.
    const Symbol * findIntLiteral(const char * value) const;
    const Symbol * findFloatLiteral(const char * value) const;
    const Symbol * findBoolLiteral(const char * value) const;
    const Symbol * findStrLiteral(const char * value) const; // Double-quotes ignored.

    // Add new literal constants. Constant must NOT be in the table. Fail with an assertion
    // if a symbol with the same VALUE is already present. All will return the new symbol.
    const Symbol * addIntLiteral(const char * value, int declLineNum);
    const Symbol * addFloatLiteral(const char * value, int declLineNum);
    const Symbol * addBoolLiteral(const char * value, int declLineNum);
    const Symbol * addStrLiteral(const char * value, int declLineNum);
    const Symbol * addIdentifier(const char * value, int declLineNum);

    // Miscellaneous queries:
    bool isEmpty() const noexcept;
    std::size_t getSize() const noexcept;

    // Print the whole table in table format (no pun intended).
    void print(std::ostream & os = std::cout) const;

    // Clone string into new memory. If the string is enclosed in
    // double-quotes, the quotation characters are ignored.
    const char * cloneCStringNoQuotes(const char * sourcePtr);

private:

    // Name generator for literal values:
    const char * makeLiteralConstName(Symbol::Type type);

    // Counter for makeLiteralConstName(). Built-in names are sequential.
    std::uint32_t nextLiteralIndex;

    // The table holds references to its backing store 'symbolPool'.
    SymTable table;

    // Symbols are stored here, the table holds pointers into this pool.
    ObjectPool<Symbol, MOON_SYMBOL_POOL_GRANULARITY> symbolPool;
};

// Calls SymbolTable::print().
std::ostream & operator << (std::ostream & os, const SymbolTable & symTable);

} // namespace moon {}

#endif // MOON_SYMBOL_TABLE_HPP
