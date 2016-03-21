
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
#include <exception>
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

class BaseException
    : public std::exception
{
public:

    explicit BaseException(const char * message);
    explicit BaseException(const std::string & message);

    virtual const char * what() const noexcept override;
    virtual ~BaseException();

private:

    // Sized buffer to avoid allocating extra memory with strings.
    static constexpr int MaxMessageLen = 1024;
    char messageBuffer[MaxMessageLen];
};

struct LexerException    final : public BaseException { using BaseException::BaseException; };
struct ParserException   final : public BaseException { using BaseException::BaseException; };
struct CompilerException final : public BaseException { using BaseException::BaseException; };
struct RuntimeException  final : public BaseException { using BaseException::BaseException; };
struct ScriptException   final : public BaseException { using BaseException::BaseException; };

// ========================================================
// Miscellaneous utilities:
// ========================================================

// Fast 32-bits hash of a null-terminated C string:
constexpr std::uint32_t NullHash = 0;
std::uint32_t hashCString(const char * cstr) noexcept;

// Get an empty C string ("\0").
const char * getEmptyCString() noexcept;

// Remove insignificant trailing zeros from a float, possibly also removing the '.'
std::string trimTrailingFloatZeros(std::string trimmed);

// Replaces non-printable escape chars with the printable equivalent (i.e.: an actual "\n", for '\n' char).
std::string unescapeString(const char * escaped);

// Replaces escape sequences by the equivalent character code (i.e. a "\n" by the '\n' character).
std::string escapeString(const char * unescaped);

template<typename T, unsigned N>
constexpr unsigned arrayLength(const T (&)[N]) noexcept
{
    return N;
}

struct CStrHasher final
{
    std::size_t operator()(const char * cstr) const noexcept
    { return hashCString(cstr); }
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
using HashTableCStr = std::unordered_map<const char *, T, CStrHasher, CStrCmpEqual>;

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
inline std::string toString(const char *        value) { return value != nullptr ? value : "null"; }
inline std::string toString(const LangFloat     value) { return trimTrailingFloatZeros(std::to_string(value)); }

// ========================================================
// RcString => Reference counted string types:
// ========================================================

struct ConstRcString final
{
    const char *        chars;    // Immutable null-terminated C-string.
    const std::uint32_t length;   // Length in characters, not including the null terminator.
    const std::uint32_t hashVal;  // Precomputed hashCString() since the string is immutable.
    std::uint32_t       refCount; // Current reference count. Deleted when it drops to zero.
};

struct MutableRcString final
{
    char *        chars;    // Mutable null-terminated C-string. Hash not precomputed in this case.
    std::uint32_t length;   // Mutable length. Not counting the null terminator.
    std::uint32_t refCount; // Current reference count. Deleted when it drops to zero.
};

// The const Ref Counted String has a precomputed hash of the string, so comparisons
// are constant-time. The string gets deallocated when the last reference is released.
ConstRcString * newConstRcString(const char * cstr);
void addRcStringRef(ConstRcString * rstr);
void releaseRcString(ConstRcString * rstr);

// The immutable Ref Counted String on the other hand has no precomputed hash,
// so comparisons are always a traditional strcmp().
MutableRcString * newMutableRcString(const char * cstr);
void addRcStringRef(MutableRcString * rstr);
void releaseRcString(MutableRcString * rstr);

// ------------------------------------

// Ref Counted String comparison:
template<typename RcStringType>
inline int cmpRcStrings(const RcStringType * a, const RcStringType * b) noexcept
{
    MOON_ASSERT(a != nullptr && b != nullptr);
    if (a->chars == b->chars) // Optimize for same memory
    {
        return 0;
    }
    if (a->length != b->length) // Optimize for strings of different length
    {
        return (a->length < b->length) ? -1 : +1;
    }
    return std::strncmp(a->chars, b->chars, a->length);
}

inline bool cmpRcStringsEqual(const ConstRcString * a, const ConstRcString * b) noexcept
{
    MOON_ASSERT(a != nullptr && b != nullptr);
    // Cheap hash comparison for the immutable strings.
    return a->hashVal == b->hashVal;
}

inline bool cmpRcStringsEqual(const MutableRcString * a, const MutableRcString * b) noexcept
{
    // The mutable string doesn't store a precomputed hash,
    // so we always do a full per-character comparison.
    return cmpRcStrings(a, b) == 0;
}

// ------------------------------------

struct CRcStrHasher final
{
    std::size_t operator()(const ConstRcString * rstr) const noexcept
    { return rstr->hashVal; }
};
struct CRcStrCmpEqual final
{
    bool operator()(const ConstRcString * a, const ConstRcString * b) const noexcept
    { return cmpRcStringsEqual(a, b); }
};
template<typename T>
using HashTableCRcStr = std::unordered_map<ConstRcString *, T, CRcStrHasher, CRcStrCmpEqual>;

// ------------------------------------

struct MRcStrHasher final
{
    std::size_t operator()(const MutableRcString * rstr) const noexcept
    { return hashCString(rstr->chars); }
};
struct MRcStrCmpEqual final
{
    bool operator()(const MutableRcString * a, const MutableRcString * b) const noexcept
    { return cmpRcStringsEqual(a, b); }
};
template<typename T>
using HashTableMRcStr = std::unordered_map<MutableRcString *, T, MRcStrHasher, MRcStrCmpEqual>;

// ------------------------------------

} // namespace moon {}

// Parser has to be included here because it
// will reference the Lexer and ParseContext.
#ifndef INC_PARSER
    #include "generated/parser.hpp"
#endif // INC_PARSER

#endif // MOON_COMMON_HPP
