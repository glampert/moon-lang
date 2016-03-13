
// ================================================================================================
// -*- C++ -*-
// File: common.hpp
// Author: Guilherme R. Lampert
// Created on: 11/06/15
// Brief: Bison Parser and Flex Lexer interface header and other common forward declarations.
// ================================================================================================

#ifndef MOON_COMMON_HPP
#define MOON_COMMON_HPP

#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <string>
#include <stdexcept>
#include <unordered_map>

#include "output.hpp"

#ifndef INC_LEXER
    // This header file is a built-in that comes with then Flex install.
    #include <FlexLexer.h>
#endif // INC_LEXER

namespace moon
{

// ========================================================
// Some forward declarations for the parser:
// ========================================================

struct Symbol;
class SymbolTable;

class SyntaxTree;
class SyntaxTreeNode;

class Parser;
struct ParseContext;

class VM;
class Compiler;

// ========================================================
// Built-in language types:
// ========================================================

using LangBool  = bool;
using LangFloat = float;
using LangInt   = std::int32_t;
using LangLong  = std::int64_t;

static_assert(sizeof(LangBool)  == 1, "Expected 8-bits boolean!");
static_assert(sizeof(LangInt)   == 4, "Expected 32-bits integer!");
static_assert(sizeof(LangLong)  == 8, "Expected 64-bits long!");
static_assert(sizeof(LangFloat) == 4, "Expected 32-bits float!");

// ========================================================
// Parser/Lexer aux types:
// ========================================================

class Lexer final
    : public yyFlexLexer
{
public:

    // Not copyable.
    Lexer(const Lexer &) = delete;
    Lexer & operator = (const Lexer &) = delete;

    Lexer(ParseContext & parseCtx, std::istream & source)
        : yyFlexLexer { &source  }
        , ctx         { parseCtx }
    { }

    ParseContext & ctx;

    // Methods defined in lexer.lxx:
    void lexOnInput(char * buf, int & result, int maxSize);
    void lexOnError(const char * message);
    void lexOnSingleLineComment();
    void lexOnMultiLineComment();
    void lexOnNewLine();
    void lexOnIdentifier();
    void lexOnStrLiteral();
    void lexOnIntLiteral();
    void lexOnFloatLiteral();
    void lexOnBoolLiteral();
};

union SemanticVal
{
    const Symbol   * asSymbol;
    SyntaxTreeNode * asSTNode;
};

struct ParseContext final
{
    SemanticVal       * yylval   = nullptr;
    Lexer             * lexer    = nullptr;
    Parser            * parser   = nullptr;
    SymbolTable       * symTable = nullptr;
    SyntaxTree        * syntTree = nullptr;
    std::string       * currText = nullptr; // [optional]
    const std::string * srcFile  = nullptr; // [optional]
};

// Bison needs this #define.
#define YYSTYPE SemanticVal

// This is the function called by the parser to get a token from the lexer.
// Bison requires it to be named yylex(), but we can control which parameters it takes.
int yylex(SemanticVal * yylval, ParseContext & ctx);

// ========================================================
// Exceptions thrown by the compiler and runtime:
// ========================================================

class CompilerException final
    : public std::runtime_error
{
public:
    using std::runtime_error::runtime_error;
};

class LexerException final
    : public std::runtime_error
{
public:
    using std::runtime_error::runtime_error;
};

class ParserException final
    : public std::runtime_error
{
public:
    using std::runtime_error::runtime_error;
};

class RuntimeException final
    : public std::runtime_error
{
public:
    using std::runtime_error::runtime_error;
};

// ========================================================
// Miscellaneous utilities:
// ========================================================

std::string trimTrailingFloatZeros(std::string trimmed);
std::size_t hashCString(const char * cstr) noexcept;
const char * getEmptyCString() noexcept;

template<typename T, unsigned N>
constexpr unsigned arrayLength(const T (&)[N]) noexcept
{
    return N;
}

struct CStrHasher final
{
    std::size_t operator()(const char * cstr) const noexcept
    {
        return hashCString(cstr);
    }
};

struct CStrCmpEqual final
{
    bool operator()(const char * a, const char * b) const noexcept
    {
        MOON_ASSERT(a != nullptr && b != nullptr);
        if (a == b) // Optimize for same memory.
        {
            return true;
        }
        return std::strcmp(a, b) == 0;
    }
};

template<typename T>
using HashTable = std::unordered_map<const char *, T, CStrHasher, CStrCmpEqual>;

// ========================================================
// toString() helpers:
// ========================================================

inline std::string toString(const std::uint8_t  value) { return std::to_string(value);    }
inline std::string toString(const std::uint16_t value) { return std::to_string(value);    }
inline std::string toString(const std::uint32_t value) { return std::to_string(value);    }
inline std::string toString(const std::uint64_t value) { return std::to_string(value);    }
inline std::string toString(const LangBool      value) { return value ? "true" : "false"; }
inline std::string toString(const LangInt       value) { return std::to_string(value);    }
inline std::string toString(const LangLong      value) { return std::to_string(value);    }
inline std::string toString(const char *        value) { return value != nullptr ? value : "(null)"; }
inline std::string toString(const LangFloat     value) { return trimTrailingFloatZeros(std::to_string(value)); }

} // namespace moon {}

// Parser has to be included here because it
// will reference the Lexer and ParseContext.
#ifndef INC_PARSER
    #include "generated/parser.hpp"
#endif // INC_PARSER

#endif // MOON_COMMON_HPP
