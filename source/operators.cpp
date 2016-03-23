
// ================================================================================================
// -*- C++ -*-
// File: operators.cpp
// Author: Guilherme R. Lampert
// Created on: 20/03/16
// Brief: Validation and execution of unary/binary operators on different type categories.
// ================================================================================================

#include "runtime.hpp"
#include <cmath> // std::fmod

namespace moon
{

// ------------------------------------------------------------------
// BINARY OPERATOR TABLES:
//
// Sub (op -)
// Add (op +)
// Mul (op *)
// Div (op /)
// Mod (op %)
// +---------------+---------+-------+--------+--------------+
// | ARITHMETICAL  | Integer | Float | String | Objects/Null |
// +---------------+---------+-------+--------+--------------+
// | Integer:      | Y       | Y†    | N      | N            |
// | Result Type:  | Integer | Float | *      | *            |
// +---------------+---------+-------+--------+--------------+
// | Float:        | Y†      | Y†    | N      | N            |
// | Result Type:  | Float   | Float | *      | *            |
// +---------------+---------+-------+--------+--------------+
// | String:       | N       | N     | Y•     | N            |
// | Result Type:  | *       | *     | String | *            |
// +---------------+---------+-------+--------+--------------+
// | Objects/Null: | N       | N     | N      | N            |
// | Result Type:  | *       | *     | *      | *            |
// +---------------+---------+-------+--------+--------------+
// NOTES:
// • - Operator + is defined for strings to perform concatenation.
// † - % with float-float or float-integer requires special handling
//
// ------------------------------------------------------------------
//
// CmpNotEqual     (op !=  )
// CmpEqual        (op ==  )
// CmpGreaterEqual (op >=  )
// CmpGreater      (op >   )
// CmpLessEqual    (op <=  )
// CmpLess         (op <   )
// LogicOr         (op or  )
// LogicAnd        (op and )
// NOTE: Result Type is always a boolean.
// +---------------+---------+-------+--------+--------------+
// | LOGICAL       | Integer | Float | String | Objects/Null |
// +---------------+---------+-------+--------+--------------+
// | Integer:      | Y       | Y     | N      | N            |
// +---------------+---------+-------+--------+--------------+
// | Float:        | Y       | Y     | N      | N            |
// +---------------+---------+-------+--------+--------------+
// | String:       | N       | N     | Y•     | N            |
// +---------------+---------+-------+--------+--------------+
// | Objects/Null: | N       | N     | N      | Y            |
// +---------------+---------+-------+--------+--------------+
// NOTES:
// • - Defined except for 'or' and 'and'
//
// ------------------------------------------------------------------

using VT = Variant::Type;
using OpApplyCB = Variant (*)(Variant varA, Variant varB);

constexpr unsigned FirstOp = static_cast<unsigned>(OpCode::CmpNotEqual);
constexpr unsigned LastOp  = static_cast<unsigned>(OpCode::Mul);
constexpr unsigned NumOps  = (LastOp - FirstOp) + 1;

std::string binaryOpToString(const OpCode op)
{
    static const std::string op2string[NumOps]
    {
        "!=",  // CmpNotEqual
        "==",  // CmpEqual
        ">=",  // CmpGreaterEqual
        ">",   // CmpGreater
        "<=",  // CmpLessEqual
        "<",   // CmpLess
        "or",  // LogicOr
        "and", // LogicAnd
        "-",   // Sub
        "+",   // Add
        "%",   // Mod
        "/",   // Div
        "*"    // Mul
    };

    const auto idxOp = static_cast<unsigned>(op);
    if (idxOp < FirstOp || idxOp > LastOp)
    {
        return toString(op);
    }
    return color::magenta() + op2string[idxOp - FirstOp] + color::restore();
}

std::string unaryOpToString(const OpCode op)
{
    switch (op)
    {
    case OpCode::LogicNot : return color::magenta() + toString("not") + color::restore();
    case OpCode::Negate   : return color::magenta() + toString("-")   + color::restore();
    case OpCode::Plus     : return color::magenta() + toString("+")   + color::restore();
    default               : return toString(op);
    } // switch (op)
}

// ========================================================
// Handlers for the numerical binary ops and conversions:
// ========================================================

#define OP_HANDLER_COMMON(funcName, resType, resVal, lhs, op, rhs)  \
    static Variant funcName(const Variant varA, const Variant varB) \
    {                                                               \
        Variant result{ resType };                                  \
        result.value.resVal = (varA.value.lhs op varB.value.rhs);   \
        return result;                                              \
    }

#define OP_HANDLER_INT_FLOAT_LOGICAL(opName, op)                                                  \
    OP_HANDLER_COMMON( opName ## _IntInt,     VT::Integer, asInteger, asInteger, op,  asInteger ) \
    OP_HANDLER_COMMON( opName ## _FloatFloat, VT::Integer, asInteger, asFloat,   op,  asFloat   ) \
    OP_HANDLER_COMMON( opName ## _IntFloat,   VT::Integer, asInteger, asInteger, op,  asFloat   ) \
    OP_HANDLER_COMMON( opName ## _FloatInt,   VT::Integer, asInteger, asFloat,   op,  asInteger )

#define OP_HANDLER_INT_FLOAT_ARITHMETICAL(opName, op)                                             \
    OP_HANDLER_COMMON( opName ## _IntInt,     VT::Integer, asInteger, asInteger, op,  asInteger ) \
    OP_HANDLER_COMMON( opName ## _FloatFloat, VT::Float,   asFloat,   asFloat,   op,  asFloat   ) \
    OP_HANDLER_COMMON( opName ## _IntFloat,   VT::Float,   asFloat,   asInteger, op,  asFloat   ) \
    OP_HANDLER_COMMON( opName ## _FloatInt,   VT::Float,   asFloat,   asFloat,   op,  asInteger )

OP_HANDLER_INT_FLOAT_LOGICAL( CmpNotEqual,     !=  )
OP_HANDLER_INT_FLOAT_LOGICAL( CmpEqual,        ==  )
OP_HANDLER_INT_FLOAT_LOGICAL( CmpGreaterEqual, >=  )
OP_HANDLER_INT_FLOAT_LOGICAL( CmpGreater,      >   )
OP_HANDLER_INT_FLOAT_LOGICAL( CmpLessEqual,    <=  )
OP_HANDLER_INT_FLOAT_LOGICAL( CmpLess,         <   )
OP_HANDLER_INT_FLOAT_LOGICAL( LogicOr,         or  )
OP_HANDLER_INT_FLOAT_LOGICAL( LogicAnd,        and )
OP_HANDLER_INT_FLOAT_ARITHMETICAL( Sub, - )
OP_HANDLER_INT_FLOAT_ARITHMETICAL( Add, + )
OP_HANDLER_INT_FLOAT_ARITHMETICAL( Mul, * )

//
// Mod is common for all type combos:
//
static Variant moduloOpCommon(const Variant varA, const Variant varB)
{
    // Integers just perform standard %
    if (varA.type == VT::Integer && varB.type == VT::Integer)
    {
        Variant result{ VT::Integer };
        result.value.asInteger = varA.value.asInteger % varB.value.asInteger;
        return result;
    }

    // If one of the sides is a float, both get promoted
    // to floating-point and we use std::fmod() instead.
    const double a = (varA.type == VT::Integer) ? static_cast<double>(varA.value.asInteger) : varA.value.asFloat;
    const double b = (varB.type == VT::Integer) ? static_cast<double>(varB.value.asInteger) : varB.value.asFloat;

    Variant result{ VT::Float };
    result.value.asFloat = std::fmod(a, b);
    return result;
}
static Variant (*Mod_IntInt    )(Variant, Variant) = &moduloOpCommon;
static Variant (*Mod_FloatFloat)(Variant, Variant) = &moduloOpCommon;
static Variant (*Mod_IntFloat  )(Variant, Variant) = &moduloOpCommon;
static Variant (*Mod_FloatInt  )(Variant, Variant) = &moduloOpCommon;

//
// Div just has to look out for integer division by zero:
//
static Variant Div_IntInt(const Variant varA, const Variant varB)
{
    if (varB.isZero())
    {
        MOON_RUNTIME_EXCEPTION("integer division by zero!");
    }
    Variant result{ VT::Integer };
    result.value.asInteger = (varA.value.asInteger / varB.value.asInteger);
    return result;
}
static Variant Div_FloatInt(const Variant varA, const Variant varB)
{
    if (varB.isZero())
    {
        MOON_RUNTIME_EXCEPTION("integer division by zero!");
    }
    Variant result{ VT::Float };
    result.value.asFloat = (varA.value.asFloat / varB.value.asInteger);
    return result;
}
OP_HANDLER_COMMON( Div_FloatFloat, VT::Float, asFloat, asFloat,   /, asFloat )
OP_HANDLER_COMMON( Div_IntFloat,   VT::Float, asFloat, asInteger, /, asFloat )

#define OP_TABLE_ENTRY(opName) \
    { opName ## _IntInt, opName ## _FloatFloat, opName ## _IntFloat, opName ## _FloatInt }

static const struct {
    OpApplyCB IntInt;
    OpApplyCB FloatFloat;
    OpApplyCB IntFloat;
    OpApplyCB FloatInt;
} numericalOpsCallbacks[NumOps] {
    OP_TABLE_ENTRY( CmpNotEqual     ),
    OP_TABLE_ENTRY( CmpEqual        ),
    OP_TABLE_ENTRY( CmpGreaterEqual ),
    OP_TABLE_ENTRY( CmpGreater      ),
    OP_TABLE_ENTRY( CmpLessEqual    ),
    OP_TABLE_ENTRY( CmpLess         ),
    OP_TABLE_ENTRY( LogicOr         ),
    OP_TABLE_ENTRY( LogicAnd        ),
    OP_TABLE_ENTRY( Sub             ),
    OP_TABLE_ENTRY( Add             ),
    OP_TABLE_ENTRY( Mod             ),
    OP_TABLE_ENTRY( Div             ),
    OP_TABLE_ENTRY( Mul             )
};

#undef OP_TABLE_ENTRY
#undef OP_HANDLER_COMMON
#undef OP_HANDLER_INT_FLOAT_LOGICAL
#undef OP_HANDLER_INT_FLOAT_ARITHMETICAL

// ========================================================
// Validation tables for the binary operators:
// ========================================================

// CmpNotEqual  CmpEqual  CmpGreaterEqual  CmpGreater  CmpLessEqual  CmpLess
//
// LogicOr  LogicAnd
//
// Sub  Add  Mod  Div  Mul
struct OpDefs
{
    // 1 if the binary operator is defined, 0 if not.
    std::int8_t ops[NumOps];
};

//                                   !=   ==   >=   >    <=   <      or   and    -    +    %    /    *
static const OpDefs null_null   = {{ 1,   1,   1,   1,   1,   1,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs null_int    = {{ 0,   0,   0,   0,   0,   0,     0,   0,     0,   0,   0,   0,   0 }};
static const OpDefs null_float  = {{ 0,   0,   0,   0,   0,   0,     0,   0,     0,   0,   0,   0,   0 }};
static const OpDefs null_str    = {{ 0,   0,   0,   0,   0,   0,     0,   0,     0,   0,   0,   0,   0 }};
static const OpDefs null_func   = {{ 1,   1,   1,   1,   1,   1,     1,   1,     0,   0,   0,   0,   0 }};
//                                   !=   ==   >=   >    <=   <      or   and    -    +    %    /    *
static const OpDefs int_null    = {{ 0,   0,   0,   0,   0,   0,     0,   0,     0,   0,   0,   0,   0 }};
static const OpDefs int_int     = {{ 1,   1,   1,   1,   1,   1,     1,   1,     1,   1,   1,   1,   1 }};
static const OpDefs int_float   = {{ 1,   1,   1,   1,   1,   1,     1,   1,     1,   1,   1,   1,   1 }};
static const OpDefs int_str     = {{ 0,   0,   0,   0,   0,   0,     0,   0,     0,   0,   0,   0,   0 }};
static const OpDefs int_func    = {{ 0,   0,   0,   0,   0,   0,     0,   0,     0,   0,   0,   0,   0 }};
//                                   !=   ==   >=   >    <=   <      or   and    -    +    %    /    *
static const OpDefs float_null  = {{ 0,   0,   0,   0,   0,   0,     0,   0,     0,   0,   0,   0,   0 }};
static const OpDefs float_int   = {{ 1,   1,   1,   1,   1,   1,     1,   1,     1,   1,   1,   1,   1 }};
static const OpDefs float_float = {{ 1,   1,   1,   1,   1,   1,     1,   1,     1,   1,   1,   1,   1 }};
static const OpDefs float_str   = {{ 0,   0,   0,   0,   0,   0,     0,   0,     0,   0,   0,   0,   0 }};
static const OpDefs float_func  = {{ 0,   0,   0,   0,   0,   0,     0,   0,     0,   0,   0,   0,   0 }};
//                                   !=   ==   >=   >    <=   <      or   and    -    +    %    /    *
static const OpDefs str_null    = {{ 0,   0,   0,   0,   0,   0,     0,   0,     0,   0,   0,   0,   0 }};
static const OpDefs str_int     = {{ 0,   0,   0,   0,   0,   0,     0,   0,     0,   0,   0,   0,   0 }};
static const OpDefs str_float   = {{ 0,   0,   0,   0,   0,   0,     0,   0,     0,   0,   0,   0,   0 }};
static const OpDefs str_str     = {{ 1,   1,   1,   1,   1,   1,     0,   0,     0,   1,   0,   0,   0 }};
static const OpDefs str_func    = {{ 0,   0,   0,   0,   0,   0,     0,   0,     0,   0,   0,   0,   0 }};
//                                   !=   ==   >=   >    <=   <      or   and    -    +    %    /    *
static const OpDefs func_null   = {{ 1,   1,   1,   1,   1,   1,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs func_int    = {{ 0,   0,   0,   0,   0,   0,     0,   0,     0,   0,   0,   0,   0 }};
static const OpDefs func_float  = {{ 0,   0,   0,   0,   0,   0,     0,   0,     0,   0,   0,   0,   0 }};
static const OpDefs func_str    = {{ 0,   0,   0,   0,   0,   0,     0,   0,     0,   0,   0,   0,   0 }};
static const OpDefs func_func   = {{ 1,   1,   1,   1,   1,   1,     1,   1,     0,   0,   0,   0,   0 }};

constexpr unsigned Cols = static_cast<unsigned>(VT::Count);
constexpr unsigned Rows = static_cast<unsigned>(VT::Count);
static const OpDefs * opsTable[Cols][Rows]
{
/*                  Null       Integer       Float        String      Function */
/* Null     */ { &null_null,  &null_int,  &null_float,  &null_str,  &null_func  },
/* Integer  */ { &int_null,   &int_int,   &int_float,   &int_str,   &int_func   },
/* Float    */ { &float_null, &float_int, &float_float, &float_str, &float_func },
/* String   */ { &str_null,   &str_int,   &str_float,   &str_str,   &str_func   },
/* Function */ { &func_null,  &func_int,  &func_float,  &func_str,  &func_func  },
};

// ========================================================
// Local helper functions:
// ========================================================

[[noreturn]] static void unreachable(const int lineNum)
{
    runtimeAssertionError("Unreachable switch clause entered! Fix this!",
                          __FILE__, lineNum);
}

static Variant binaryOpOnStrings(const OpCode op, const Variant varA, const Variant varB)
{
    // Concatenate strings with operator +
    if (op == OpCode::Add)
    {
        const auto aLen = std::strlen(varA.value.asStringPtr);
        const auto bLen = std::strlen(varB.value.asStringPtr);

        //FIXME temp: str is never freed for now...
        auto newStr = new char[aLen + bLen + 1];
        std::strcpy(newStr, varA.value.asStringPtr);
        std::strcat(newStr, varB.value.asStringPtr);

        Variant result{ VT::String };
        result.value.asStringPtr = newStr;
        return result;
    }

    // Lexicographical comparison:
    Variant result{ VT::Integer };
    const auto cmpResult = std::strcmp(varA.value.asStringPtr, varB.value.asStringPtr);
    switch (op)
    {
    case OpCode::CmpNotEqual     : { result.value.asInteger = (cmpResult != 0); break; }
    case OpCode::CmpEqual        : { result.value.asInteger = (cmpResult == 0); break; }
    case OpCode::CmpGreaterEqual : { result.value.asInteger = (cmpResult >= 0); break; }
    case OpCode::CmpGreater      : { result.value.asInteger = (cmpResult >  0); break; }
    case OpCode::CmpLessEqual    : { result.value.asInteger = (cmpResult <= 0); break; }
    case OpCode::CmpLess         : { result.value.asInteger = (cmpResult <  0); break; }
    default                      : { unreachable(__LINE__); }
    } // switch (op)
    return result;
}

static Variant binaryOpOnObjects(const OpCode op, const Variant varA, const Variant varB)
{
    Variant result{ VT::Integer };
    switch (op)
    {
    case OpCode::CmpNotEqual     : { result.value.asInteger = (varA.value.asVoidPtr !=  varB.value.asVoidPtr); break; }
    case OpCode::CmpEqual        : { result.value.asInteger = (varA.value.asVoidPtr ==  varB.value.asVoidPtr); break; }
    case OpCode::CmpGreaterEqual : { result.value.asInteger = (varA.value.asVoidPtr >=  varB.value.asVoidPtr); break; }
    case OpCode::CmpGreater      : { result.value.asInteger = (varA.value.asVoidPtr >   varB.value.asVoidPtr); break; }
    case OpCode::CmpLessEqual    : { result.value.asInteger = (varA.value.asVoidPtr <=  varB.value.asVoidPtr); break; }
    case OpCode::CmpLess         : { result.value.asInteger = (varA.value.asVoidPtr <   varB.value.asVoidPtr); break; }
    case OpCode::LogicOr         : { result.value.asInteger = (varA.value.asVoidPtr or  varB.value.asVoidPtr); break; }
    case OpCode::LogicAnd        : { result.value.asInteger = (varA.value.asVoidPtr and varB.value.asVoidPtr); break; }
    default                      : { unreachable(__LINE__); }
    } // switch (op)
    return result;
}

// ========================================================
// isBinaryOpValid():
// ========================================================

bool isBinaryOpValid(const OpCode op, const Variant::Type typeA, const Variant::Type typeB) noexcept
{
    const auto idxA  = static_cast<unsigned>(typeA);
    const auto idxB  = static_cast<unsigned>(typeB);
    const auto idxOp = static_cast<unsigned>(op) - FirstOp;

    MOON_ASSERT(idxOp < NumOps);
    MOON_ASSERT(idxA < Cols && idxB < Rows);

    // Constant-time table lookup.
    return opsTable[idxA][idxB]->ops[idxOp];
}

// ========================================================
// performBinaryOp():
// ========================================================

Variant performBinaryOp(const OpCode op, const Variant varA, const Variant varB)
{
    if (!isBinaryOpValid(op, varA.type, varB.type))
    {
        MOON_RUNTIME_EXCEPTION("cannot perform binary op " + binaryOpToString(op) + " with " +
                               toString(varA.type) + " and " + toString(varB.type));
    }

    // We have already validated above, so some cases are unreachable.
    const auto idxOp = static_cast<unsigned>(op) - FirstOp;
    switch (varA.type)
    {
    // int and float have implicit conversions:
    case VT::Integer :
        {
            switch (varB.type)
            {
            case VT::Integer :
                return numericalOpsCallbacks[idxOp].IntInt(varA, varB);
            case VT::Float :
                return numericalOpsCallbacks[idxOp].IntFloat(varA, varB);
            default :
                unreachable(__LINE__);
            } // switch (varB.type)
        }
    case VT::Float :
        {
            switch (varB.type)
            {
            case VT::Integer :
                return numericalOpsCallbacks[idxOp].FloatInt(varA, varB);
            case VT::Float :
                return numericalOpsCallbacks[idxOp].FloatFloat(varA, varB);
            default :
                unreachable(__LINE__);
            } // switch (varB.type)
        }

    // Strings only work with other strings:
    case VT::String :
        return binaryOpOnStrings(op, varA, varB);

    // Generic objects only allows basic comparisons:
    case VT::Null :
    case VT::Function :
        return binaryOpOnObjects(op, varA, varB);

    default :
        unreachable(__LINE__);
    } // switch (varA.type)
}

// ========================================================
// isUnaryOpValid():
// ========================================================

bool isUnaryOpValid(const OpCode op, const Variant::Type type) noexcept
{
    if (op == OpCode::LogicNot)
    {
        return type == VT::Null    ||
               type == VT::Integer ||
               type == VT::Float   ||
               type == VT::Function;
    }

    // +, -
    return type == VT::Integer || type == VT::Float;
}

// ========================================================
// performUnaryOp():
// ========================================================

Variant performUnaryOp(const OpCode op, const Variant var)
{
    if (!isUnaryOpValid(op, var.type))
    {
        MOON_RUNTIME_EXCEPTION("cannot apply unary op " + unaryOpToString(op) +
                               " on " + toString(var.type));
    }

    // Produces a boolean result
    #define CASE1(opType, unaryOp)                                    \
        case OpCode::opType :                                         \
        {                                                             \
            switch (var.type)                                         \
            {                                                         \
            case VT::Integer :                                        \
                result.value.asInteger = unaryOp var.value.asInteger; \
                result.type = VT::Integer;                            \
                break;                                                \
            case VT::Float :                                          \
                result.value.asInteger = unaryOp var.value.asFloat;   \
                result.type = VT::Integer;                            \
                break;                                                \
            default :                                                 \
                result.value.asInteger = unaryOp var.value.asVoidPtr; \
                result.type = VT::Integer;                            \
                break;                                                \
            }                                                         \
            break;                                                    \
        }

    // Preserves the type of the input
    #define CASE2(opType, unaryOp)                                    \
        case OpCode::opType :                                         \
        {                                                             \
            switch (var.type)                                         \
            {                                                         \
            case VT::Integer :                                        \
                result.value.asInteger = unaryOp var.value.asInteger; \
                result.type = VT::Integer;                            \
                break;                                                \
            case VT::Float :                                          \
                result.value.asFloat = unaryOp var.value.asFloat;     \
                result.type = VT::Float;                              \
                break;                                                \
            default :                                                 \
                unreachable(__LINE__);                                \
            }                                                         \
            break;                                                    \
        }

    Variant result;
    switch (op)
    {
    CASE1( LogicNot, not );
    CASE2( Negate,   -   );
    CASE2( Plus,     +   );
    default : unreachable(__LINE__);
    } // switch (op)
    return result;

    #undef CASE1
    #undef CASE2
}

// ========================================================
// Minimal unit tests:
// ========================================================

//
// Testing a few of the table entries in isBinaryOpValid() for consistency
// with the expected and a few usage cases for performBinaryOp().
//
#if DEBUG
namespace
{

struct OpsTableTest
{
    OpsTableTest()
    {
        // Logical:
        MOON_ASSERT(isBinaryOpValid(OpCode::CmpNotEqual, VT::Integer, VT::Float)    == true);
        MOON_ASSERT(isBinaryOpValid(OpCode::CmpEqual,    VT::Float,   VT::Integer)  == true);
        MOON_ASSERT(isBinaryOpValid(OpCode::CmpLess,     VT::String,  VT::String)   == true);
        MOON_ASSERT(isBinaryOpValid(OpCode::LogicAnd,    VT::Float,   VT::Float)    == true);

        MOON_ASSERT(isBinaryOpValid(OpCode::CmpNotEqual, VT::Integer, VT::String)   == false);
        MOON_ASSERT(isBinaryOpValid(OpCode::CmpEqual,    VT::Float,   VT::Function) == false);
        MOON_ASSERT(isBinaryOpValid(OpCode::CmpLess,     VT::String,  VT::Null)     == false);
        MOON_ASSERT(isBinaryOpValid(OpCode::LogicAnd,    VT::String,  VT::String)   == false);

        // Arithmetical:
        MOON_ASSERT(isBinaryOpValid(OpCode::Add, VT::String,  VT::String)   == true);
        MOON_ASSERT(isBinaryOpValid(OpCode::Add, VT::Integer, VT::Float)    == true);
        MOON_ASSERT(isBinaryOpValid(OpCode::Sub, VT::Float,   VT::Integer)  == true);
        MOON_ASSERT(isBinaryOpValid(OpCode::Mod, VT::Float,   VT::Float)    == true);
        MOON_ASSERT(isBinaryOpValid(OpCode::Mod, VT::Integer, VT::Float)    == true);

        MOON_ASSERT(isBinaryOpValid(OpCode::Add, VT::String,  VT::Float)    == false);
        MOON_ASSERT(isBinaryOpValid(OpCode::Add, VT::Integer, VT::String)   == false);
        MOON_ASSERT(isBinaryOpValid(OpCode::Sub, VT::Float,   VT::Function) == false);
        MOON_ASSERT(isBinaryOpValid(OpCode::Mod, VT::String,  VT::Integer)  == false);
        MOON_ASSERT(isBinaryOpValid(OpCode::Div, VT::String,  VT::String)   == false);

        std::cout << "TEST: Operations table test passed." << std::endl;
    }
} localOpsTableTest;

struct BinOpsTest
{
    BinOpsTest()
    {
        Variant result;
        Variant varA, varB;

        // Logical:
        varA.type = VT::Integer;
        varA.value.asInteger = 1;
        varB.type = VT::Float;
        varB.value.asFloat = 1;
        result = performBinaryOp(OpCode::CmpEqual, varA, varB);
        MOON_ASSERT(result.type == VT::Integer && result.toBool() == true);

        varA.type = VT::String;
        varA.value.asStringPtr = "Hello";
        varB.type = VT::String;
        varB.value.asStringPtr = "World";
        result = performBinaryOp(OpCode::CmpNotEqual, varA, varB);
        MOON_ASSERT(result.type == VT::Integer && result.toBool() == true);

        varA.type = VT::Null;
        varA.value.asVoidPtr = nullptr;
        varB.type = VT::Function;
        varB.value.asFunctionPtr = nullptr;
        result = performBinaryOp(OpCode::LogicOr, varA, varB);
        MOON_ASSERT(result.type == VT::Integer && result.toBool() == false);

        // Arithmetical:
        varA.type = VT::Integer;
        varA.value.asInteger = 2;
        varB.type = VT::Float;
        varB.value.asFloat = 2;
        result = performBinaryOp(OpCode::Mul, varA, varB);
        MOON_ASSERT(result.type == VT::Float && result.value.asFloat == 4);

        varA.type = VT::Float;
        varA.value.asFloat = 3;
        varB.type = VT::Float;
        varB.value.asFloat = 2;
        result = performBinaryOp(OpCode::Mod, varA, varB);
        MOON_ASSERT(result.type == VT::Float && result.value.asFloat == 1);

        varA.type = VT::Integer;
        varA.value.asInteger = 3;
        varB.type = VT::Integer;
        varB.value.asInteger = 2;
        result = performBinaryOp(OpCode::Mod, varA, varB);
        MOON_ASSERT(result.type == VT::Integer && result.value.asInteger == 1);

        varA.type = VT::String;
        varA.value.asStringPtr = "Hello";
        varB.type = VT::String;
        varB.value.asStringPtr = "World";
        result = performBinaryOp(OpCode::Add, varA, varB);
        MOON_ASSERT(result.type == VT::String && std::strcmp(result.value.asStringPtr, "HelloWorld") == 0);

        std::cout << "TEST: Binary ops test passed." << std::endl;
    }
} localBinOpsTest;

} // namespace {}
#endif // DEBUG

} // namespace moon {}
