
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

#include <utility>
#include <string>
#include <memory>
#include <exception>
#include <unordered_map>

#include "output.hpp"

#ifndef INC_LEXER
    // This header file is a built-in that comes bundled with the Flex install.
    #include <FlexLexer.h>
#endif // INC_LEXER

namespace moon
{

// ========================================================
// Some forward declarations for the Lexer & Parser:
// ========================================================

struct Symbol;
class SymbolTable;

struct Function;
class FunctionTable;

struct TypeId;
class TypeTable;

struct SyntaxTreeNode;
class SyntaxTree;

struct ParseContext;
class Parser;

class VM;
class Object;
class Compiler;

// ========================================================
// Shorthand type aliases:
// ========================================================

using Int8     = std::int8_t;
using Int16    = std::int16_t;
using Int32    = std::int32_t;
using Int64    = std::int64_t;
using UInt8    = std::uint8_t;
using UInt16   = std::uint16_t;
using UInt32   = std::uint32_t;
using UInt64   = std::uint64_t;
using Float32  = float;
using Float64  = double;

static_assert(sizeof(Int8)    == 1, "Expected 8-bits  integer!");
static_assert(sizeof(Int16)   == 2, "Expected 16-bits integer!");
static_assert(sizeof(Int32)   == 4, "Expected 32-bits integer!");
static_assert(sizeof(Int64)   == 8, "Expected 64-bits integer!");
static_assert(sizeof(UInt8)   == 1, "Expected 8-bits  integer!");
static_assert(sizeof(UInt16)  == 2, "Expected 16-bits integer!");
static_assert(sizeof(UInt32)  == 4, "Expected 32-bits integer!");
static_assert(sizeof(UInt64)  == 8, "Expected 64-bits integer!");
static_assert(sizeof(Float32) == 4, "Expected 32-bits float!");
static_assert(sizeof(Float64) == 8, "Expected 64-bits float!");

// ========================================================
// Parser/Lexer aux types:
// ========================================================

// We inherit from yyFlexLexer to be able to declare the
// ParseContext member, which will be needed to pass some
// information around that the Parser and Lexer must share.
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
    SemanticVal       * yylval    = nullptr;
    Lexer             * lexer     = nullptr;
    Parser            * parser    = nullptr;
    SymbolTable       * symTable  = nullptr;
    FunctionTable     * fnTable   = nullptr;
    TypeTable         * typeTable = nullptr;
    SyntaxTree        * syntTree  = nullptr;
    Object            ** objList  = nullptr;
    std::string       * currText  = nullptr; // [optional]
    const std::string * srcFile   = nullptr; // [optional]
};

// Bison needs this #define.
#define YYSTYPE SemanticVal

// This is the function called by the Parser to get a token from the Lexer.
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

struct Range final
{
    Int32 start;
    Int32 end;
};

// Fast 32-bits hash of a null-terminated C string:
constexpr UInt32 NullHash = 0;
UInt32 hashCString(const char * cstr);

// Get an empty C string ("\0").
const char * getEmptyCString() noexcept;

// Remove insignificant trailing zeros from a float, possibly also removing the '.'
std::string trimTrailingFloatZeros(std::string trimmed);

// Replaces non-printable escape chars with the printable equivalent (i.e.: an actual "\n", for '\n' char).
std::string unescapeString(const char * escaped);

// Replaces escape sequences by the equivalent character code (i.e. a "\n" by the '\n' character).
std::string escapeString(const char * unescaped);

// Temporary formatting buffer used is fixed to 2048
// chars, so longer strings will get truncated!
#ifdef __GNUC__
std::string strPrintF(const char * format, ...) __attribute__((format(printf, 1, 2)));
#else // !__GNUC__
std::string strPrintF(const char * format, ...);
#endif // __GNUC__

// ------------------------------------

template<typename T, UInt32 N>
constexpr UInt32 arrayLength(const T (&)[N]) noexcept
{
    return N;
}

struct CStrHasher final
{
    std::size_t operator()(const char * cstr) const
    { return hashCString(cstr); }
};
struct CStrCmpEqual final
{
    bool operator()(const char * a, const char * b) const
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

inline std::string toString(const bool value)    { return value ? "true" : "false"; }
inline std::string toString(const char * value)  { return (value != nullptr) ? value : "null"; }

inline std::string toString(const Int8  value)   { return std::to_string(value); }
inline std::string toString(const Int16 value)   { return std::to_string(value); }
inline std::string toString(const Int32 value)   { return std::to_string(value); }
inline std::string toString(const Int64 value)   { return std::to_string(value); }

inline std::string toString(const UInt8  value)  { return std::to_string(value); }
inline std::string toString(const UInt16 value)  { return std::to_string(value); }
inline std::string toString(const UInt32 value)  { return std::to_string(value); }
inline std::string toString(const UInt64 value)  { return std::to_string(value); }

inline std::string toString(const Float32 value) { return trimTrailingFloatZeros(std::to_string(value)); }
inline std::string toString(const Float64 value) { return trimTrailingFloatZeros(std::to_string(value)); }

// ========================================================
// Compile-time version of hashCString():
// ========================================================

namespace ct
{

//
// Compile-time implementation of the one-at-a-time hash for strings:
//  https://en.wikipedia.org/wiki/Jenkins_hash_function
//
// Inspired by this CR question:
//  https://codereview.stackexchange.com/questions/93775/compile-time-sieve-of-eratosthenes
// And:
//  http://stackoverflow.com/questions/28675727/using-crc32-algorithm-to-hash-string-at-compile-time
//

constexpr UInt32 addSHL(UInt32 h, UInt32 shift) { return h + (h << shift); }
constexpr UInt32 addSHR(UInt32 h, UInt32 shift) { return h + (h >> shift); }
constexpr UInt32 xorSHR(UInt32 h, UInt32 shift) { return h ^ (h >> shift); }

constexpr UInt32 hashOATFinish(UInt32 h)
{
    // h += (h <<  3)
    // h ^= (h >> 11)
    // h += (h << 15)
    return addSHL(xorSHR(addSHL(h, 3), 11), 15);
}

constexpr UInt32 hashOATStep(UInt32 h, UInt32 c)
{
    // h += c
    // h += (h << 10)
    // h ^= (h >>  6)
    return xorSHR(addSHL(h + c, 10), 6);
}

constexpr UInt32 hashOneAtATime(const char * cstr, UInt32 length, UInt32 h)
{
    // hashOneAtATime is equivalent to the main hash loop unrolled recursively.
    return (length != 0) ? hashOneAtATime(cstr + 1, length - 1, hashOATStep(h, *cstr)) : hashOATFinish(h);
}

constexpr UInt32 lengthOfCString(const char * cstr)
{
    return (*cstr != '\0') ? (lengthOfCString(cstr + 1) + 1) : 0;
}

constexpr UInt32 hashCString(const char * cstr)
{
    return hashOneAtATime(cstr, lengthOfCString(cstr), 0);
}

} // namespace ct {}

// ========================================================
// ConstRcString => Simple reference counted string type:
// ========================================================

struct ConstRcString final
{
    const char * const chars;    // Immutable null-terminated C-string.
    const UInt32       length;   // Length in characters, not including the null terminator.
    const UInt32       hashVal;  // Precomputed hashCString() since the string is immutable.
    UInt32             refCount; // Current reference count. Deleted when it drops to zero.
};

// The const Ref Counted String has a precomputed hash of the string, so equal comparison
// is constant-time. The string gets deallocated when the last reference is released.
ConstRcString * newConstRcString(const char * cstr, UInt32 maxCharsToCopy);
ConstRcString * newConstRcString(const char * cstr);
ConstRcString * addRcStringRef(ConstRcString * rstr);
void releaseRcString(ConstRcString * rstr);

// ------------------------------------

inline int cmpRcStrings(const ConstRcString * a, const ConstRcString * b)
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

inline bool cmpRcStringsEqual(const ConstRcString * a, const ConstRcString * b)
{
    MOON_ASSERT(a != nullptr && b != nullptr);
    return a->hashVal == b->hashVal;
}

inline bool isRcStringEmpty(const ConstRcString * rstr) noexcept
{
    return rstr->length == 0;
}

inline bool isRcStringValid(const ConstRcString * rstr) noexcept
{
    return rstr != nullptr && rstr->chars != nullptr && rstr->refCount != 0;
}

inline std::string toString(const ConstRcString * rstr)
{
    return isRcStringValid(rstr) ? rstr->chars : "null";
}

// ------------------------------------

struct CRcStrDeleter final
{
    void operator()(ConstRcString * rstr) const
    { releaseRcString(rstr); }
};
using ConstRcStrUPtr = std::unique_ptr<ConstRcString, CRcStrDeleter>;

struct CRcStrHasher final
{
    std::size_t operator()(const ConstRcString * rstr) const
    { return rstr->hashVal; }
};
struct CRcStrCmpEqual final
{
    bool operator()(const ConstRcString * a, const ConstRcString * b) const
    { return cmpRcStringsEqual(a, b); }
};
template<typename T>
using HashTableConstRcStr = std::unordered_map<ConstRcString *, T, CRcStrHasher, CRcStrCmpEqual>;

// ------------------------------------

} // namespace moon {}

// Parser has to be included here because it
// will reference the Lexer and ParseContext.
#ifndef INC_PARSER
    #include "generated/parser.hpp"
#endif // INC_PARSER

#endif // MOON_COMMON_HPP
