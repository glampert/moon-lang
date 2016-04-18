
// ================================================================================================
// -*- C++ -*-
// File: operators.cpp
// Author: Guilherme R. Lampert
// Created on: 20/03/16
// Brief: Validation and execution of unary/binary operators on different type categories.
// ================================================================================================

#include "vm.hpp"
#include <cmath> // std::fmod

namespace moon
{

// ========================================================
// Validation tables for the binary operators:
// ========================================================

static constexpr UInt32 FirstOp = static_cast<UInt32>(OpCode::CmpNotEqual);
static constexpr UInt32 LastOp  = static_cast<UInt32>(OpCode::Mul);
static constexpr UInt32 NumOps  = (LastOp - FirstOp) + 1;

// CmpNotEqual  CmpEqual  CmpGreaterEqual  CmpGreater  CmpLessEqual  CmpLess
//
// LogicOr  LogicAnd
//
// Sub  Add  Mod  Div  Mul
struct OpDefs
{
    // 1 if the binary operator is defined, 0 if not.
    Int8 ops[NumOps];
};

//                                   !=   ==   >=   >    <=   <      or   and    -    +    %    /    *
static const OpDefs null_null   = {{ 1,   1,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs null_int    = {{ 0,   0,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs null_float  = {{ 0,   0,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs null_func   = {{ 1,   1,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs null_tid    = {{ 1,   1,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs null_str    = {{ 1,   1,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs null_obj    = {{ 1,   1,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs null_arr    = {{ 1,   1,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs null_range  = {{ 0,   0,   0,   0,   0,   0,     0,   0,     0,   0,   0,   0,   0 }};
static const OpDefs null_any    = {{ 1,   1,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
//                                   !=   ==   >=   >    <=   <      or   and    -    +    %    /    *
static const OpDefs int_null    = {{ 0,   0,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs int_int     = {{ 1,   1,   1,   1,   1,   1,     1,   1,     1,   1,   1,   1,   1 }};
static const OpDefs int_float   = {{ 1,   1,   1,   1,   1,   1,     1,   1,     1,   1,   1,   1,   1 }};
static const OpDefs int_func    = {{ 0,   0,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs int_tid     = {{ 0,   0,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs int_str     = {{ 0,   0,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs int_obj     = {{ 0,   0,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs int_arr     = {{ 0,   0,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs int_range   = {{ 0,   0,   0,   0,   0,   0,     0,   0,     0,   0,   0,   0,   0 }};
static const OpDefs int_any     = {{ 1,   1,   1,   1,   1,   1,     1,   1,     1,   1,   1,   1,   1 }};
//                                   !=   ==   >=   >    <=   <      or   and    -    +    %    /    *
static const OpDefs float_null  = {{ 0,   0,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs float_int   = {{ 1,   1,   1,   1,   1,   1,     1,   1,     1,   1,   1,   1,   1 }};
static const OpDefs float_float = {{ 1,   1,   1,   1,   1,   1,     1,   1,     1,   1,   1,   1,   1 }};
static const OpDefs float_func  = {{ 0,   0,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs float_tid   = {{ 0,   0,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs float_str   = {{ 0,   0,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs float_obj   = {{ 0,   0,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs float_arr   = {{ 0,   0,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs float_range = {{ 0,   0,   0,   0,   0,   0,     0,   0,     0,   0,   0,   0,   0 }};
static const OpDefs float_any   = {{ 1,   1,   1,   1,   1,   1,     1,   1,     1,   1,   1,   1,   1 }};
//                                   !=   ==   >=   >    <=   <      or   and    -    +    %    /    *
static const OpDefs func_null   = {{ 1,   1,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs func_int    = {{ 0,   0,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs func_float  = {{ 0,   0,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs func_func   = {{ 1,   1,   1,   1,   1,   1,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs func_tid    = {{ 0,   0,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs func_str    = {{ 0,   0,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs func_obj    = {{ 0,   0,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs func_arr    = {{ 0,   0,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs func_range  = {{ 0,   0,   0,   0,   0,   0,     0,   0,     0,   0,   0,   0,   0 }};
static const OpDefs func_any    = {{ 1,   1,   1,   1,   1,   1,     1,   1,     0,   0,   0,   0,   0 }};
//                                   !=   ==   >=   >    <=   <      or   and    -    +    %    /    *
static const OpDefs tid_null    = {{ 1,   1,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs tid_int     = {{ 0,   0,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs tid_float   = {{ 0,   0,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs tid_func    = {{ 0,   0,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs tid_tid     = {{ 1,   1,   1,   1,   1,   1,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs tid_str     = {{ 0,   0,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs tid_obj     = {{ 0,   0,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs tid_arr     = {{ 0,   0,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs tid_range   = {{ 0,   0,   0,   0,   0,   0,     0,   0,     0,   0,   0,   0,   0 }};
static const OpDefs tid_any     = {{ 1,   1,   1,   1,   1,   1,     1,   1,     0,   0,   0,   0,   0 }};
//                                   !=   ==   >=   >    <=   <      or   and    -    +    %    /    *
static const OpDefs str_null    = {{ 1,   1,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs str_int     = {{ 0,   0,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs str_float   = {{ 0,   0,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs str_func    = {{ 0,   0,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs str_tid     = {{ 0,   0,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs str_str     = {{ 1,   1,   1,   1,   1,   1,     1,   1,     0,   1,   0,   0,   0 }};
static const OpDefs str_obj     = {{ 0,   0,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs str_arr     = {{ 0,   0,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs str_range   = {{ 0,   0,   0,   0,   0,   0,     0,   0,     0,   0,   0,   0,   0 }};
static const OpDefs str_any     = {{ 1,   1,   1,   1,   1,   1,     1,   1,     0,   1,   0,   0,   0 }};
//                                   !=   ==   >=   >    <=   <      or   and    -    +    %    /    *
static const OpDefs obj_null    = {{ 1,   1,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs obj_int     = {{ 0,   0,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs obj_float   = {{ 0,   0,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs obj_func    = {{ 0,   0,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs obj_tid     = {{ 0,   0,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs obj_str     = {{ 0,   0,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs obj_obj     = {{ 1,   1,   1,   1,   1,   1,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs obj_arr     = {{ 0,   0,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs obj_range   = {{ 0,   0,   0,   0,   0,   0,     0,   0,     0,   0,   0,   0,   0 }};
static const OpDefs obj_any     = {{ 1,   1,   1,   1,   1,   1,     1,   1,     0,   0,   0,   0,   0 }};
//                                   !=   ==   >=   >    <=   <      or   and    -    +    %    /    *
static const OpDefs arr_null    = {{ 1,   1,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs arr_int     = {{ 0,   0,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs arr_float   = {{ 0,   0,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs arr_func    = {{ 0,   0,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs arr_tid     = {{ 0,   0,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs arr_str     = {{ 0,   0,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs arr_obj     = {{ 0,   0,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs arr_arr     = {{ 1,   1,   1,   1,   1,   1,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs arr_range   = {{ 0,   0,   0,   0,   0,   0,     0,   0,     0,   0,   0,   0,   0 }};
static const OpDefs arr_any     = {{ 1,   1,   1,   1,   1,   1,     1,   1,     0,   0,   0,   0,   0 }};
//                                   !=   ==   >=   >    <=   <      or   and    -    +    %    /    *
static const OpDefs range_null  = {{ 0,   0,   0,   0,   0,   0,     0,   0,     0,   0,   0,   0,   0 }};
static const OpDefs range_int   = {{ 0,   0,   0,   0,   0,   0,     0,   0,     0,   0,   0,   0,   0 }};
static const OpDefs range_float = {{ 0,   0,   0,   0,   0,   0,     0,   0,     0,   0,   0,   0,   0 }};
static const OpDefs range_func  = {{ 0,   0,   0,   0,   0,   0,     0,   0,     0,   0,   0,   0,   0 }};
static const OpDefs range_tid   = {{ 0,   0,   0,   0,   0,   0,     0,   0,     0,   0,   0,   0,   0 }};
static const OpDefs range_str   = {{ 0,   0,   0,   0,   0,   0,     0,   0,     0,   0,   0,   0,   0 }};
static const OpDefs range_obj   = {{ 0,   0,   0,   0,   0,   0,     0,   0,     0,   0,   0,   0,   0 }};
static const OpDefs range_arr   = {{ 0,   0,   0,   0,   0,   0,     0,   0,     0,   0,   0,   0,   0 }};
static const OpDefs range_range = {{ 1,   1,   1,   1,   1,   1,     0,   0,     0,   0,   0,   0,   0 }};
static const OpDefs range_any   = {{ 1,   1,   1,   1,   1,   1,     0,   0,     0,   0,   0,   0,   0 }};
//                                   !=   ==   >=   >    <=   <      or   and    -    +    %    /    *
static const OpDefs any_null    = {{ 1,   1,   0,   0,   0,   0,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs any_int     = {{ 1,   1,   1,   1,   1,   1,     1,   1,     1,   1,   1,   1,   1 }};
static const OpDefs any_float   = {{ 1,   1,   1,   1,   1,   1,     1,   1,     1,   1,   1,   1,   1 }};
static const OpDefs any_func    = {{ 1,   1,   1,   1,   1,   1,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs any_tid     = {{ 1,   1,   1,   1,   1,   1,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs any_str     = {{ 1,   1,   1,   1,   1,   1,     1,   1,     0,   1,   0,   0,   0 }};
static const OpDefs any_obj     = {{ 1,   1,   1,   1,   1,   1,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs any_arr     = {{ 1,   1,   1,   1,   1,   1,     1,   1,     0,   0,   0,   0,   0 }};
static const OpDefs any_range   = {{ 1,   1,   1,   1,   1,   1,     0,   0,     0,   0,   0,   0,   0 }};
static const OpDefs any_any     = {{ 1,   1,   1,   1,   1,   1,     1,   1,     1,   1,   1,   1,   1 }};

// We need one row and column for each type in the Variant enum.
static constexpr UInt32 Cols = static_cast<UInt32>(Variant::Type::Count);
static constexpr UInt32 Rows = static_cast<UInt32>(Variant::Type::Count);
static const OpDefs * opsTable[Cols][Rows]
{
/*                 Null        Integer       Float       Function       Tid         Str       Object       Array        Range        Any     */
/* Null     */ { &null_null,  &null_int,  &null_float,  &null_func,  &null_tid,  &null_str,  &null_obj,  &null_arr,  &null_range,  &null_any  },
/* Integer  */ { &int_null,   &int_int,   &int_float,   &int_func,   &int_tid,   &int_str,   &int_obj,   &int_arr,   &int_range,   &int_any   },
/* Float    */ { &float_null, &float_int, &float_float, &float_func, &float_tid, &float_str, &float_obj, &float_arr, &float_range, &float_any },
/* Function */ { &func_null,  &func_int,  &func_float,  &func_func,  &func_tid,  &func_str,  &func_obj,  &func_arr,  &func_range,  &func_any  },
/* Tid      */ { &tid_null,   &tid_int,   &tid_float,   &tid_func,   &tid_tid,   &tid_str,   &tid_obj,   &tid_arr,   &tid_range,   &tid_any   },
/* Str      */ { &str_null,   &str_int,   &str_float,   &str_func,   &str_tid,   &str_str,   &str_obj,   &str_arr,   &str_range,   &str_any   },
/* Object   */ { &obj_null,   &obj_int,   &obj_float,   &obj_func,   &obj_tid,   &obj_str,   &obj_obj,   &obj_arr,   &obj_range,   &obj_any   },
/* Array    */ { &arr_null,   &arr_int,   &arr_float,   &arr_func,   &arr_tid,   &arr_str,   &arr_obj,   &arr_arr,   &arr_range,   &arr_any   },
/* Range    */ { &range_null, &range_int, &range_float, &range_func, &range_tid, &range_str, &range_obj, &range_arr, &range_range, &range_any },
/* Any      */ { &any_null,   &any_int,   &any_float,   &any_func,   &any_tid,   &any_str,   &any_obj,   &any_arr,   &any_range,   &any_any   }
};

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

#define OP_HANDLER_INT_FLOAT_LOGICAL(opName, op)                                                             \
    OP_HANDLER_COMMON( opName ## _IntInt,     Variant::Type::Integer, asInteger, asInteger, op,  asInteger ) \
    OP_HANDLER_COMMON( opName ## _FloatFloat, Variant::Type::Integer, asInteger, asFloat,   op,  asFloat   ) \
    OP_HANDLER_COMMON( opName ## _IntFloat,   Variant::Type::Integer, asInteger, asInteger, op,  asFloat   ) \
    OP_HANDLER_COMMON( opName ## _FloatInt,   Variant::Type::Integer, asInteger, asFloat,   op,  asInteger )

#define OP_HANDLER_INT_FLOAT_ARITHMETICAL(opName, op)                                                        \
    OP_HANDLER_COMMON( opName ## _IntInt,     Variant::Type::Integer, asInteger, asInteger, op,  asInteger ) \
    OP_HANDLER_COMMON( opName ## _FloatFloat, Variant::Type::Float,   asFloat,   asFloat,   op,  asFloat   ) \
    OP_HANDLER_COMMON( opName ## _IntFloat,   Variant::Type::Float,   asFloat,   asInteger, op,  asFloat   ) \
    OP_HANDLER_COMMON( opName ## _FloatInt,   Variant::Type::Float,   asFloat,   asFloat,   op,  asInteger )

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
    // Integers just perform standard % modulo
    if (varA.type == Variant::Type::Integer &&
        varB.type == Variant::Type::Integer)
    {
        Variant result{ Variant::Type::Integer };
        result.value.asInteger = varA.value.asInteger % varB.value.asInteger;
        return result;
    }

    // If one of the sides is a float, both get promoted
    // to floating-point and we use std::fmod() instead.
    const Float64 a = (varA.type == Variant::Type::Integer) ? static_cast<Float64>(varA.value.asInteger) : varA.value.asFloat;
    const Float64 b = (varB.type == Variant::Type::Integer) ? static_cast<Float64>(varB.value.asInteger) : varB.value.asFloat;

    Variant result{ Variant::Type::Float };
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
    Variant result{ Variant::Type::Integer };
    result.value.asInteger = (varA.value.asInteger / varB.value.asInteger);
    return result;
}
static Variant Div_FloatInt(const Variant varA, const Variant varB)
{
    if (varB.isZero())
    {
        MOON_RUNTIME_EXCEPTION("integer division by zero!");
    }
    Variant result{ Variant::Type::Float };
    result.value.asFloat = (varA.value.asFloat / varB.value.asInteger);
    return result;
}
OP_HANDLER_COMMON( Div_FloatFloat, Variant::Type::Float, asFloat, asFloat,   /, asFloat )
OP_HANDLER_COMMON( Div_IntFloat,   Variant::Type::Float, asFloat, asInteger, /, asFloat )

#define OP_TABLE_ENTRY(opName) \
    { opName ## _IntInt, opName ## _FloatFloat, opName ## _IntFloat, opName ## _FloatInt }

static const struct {
    using OpApplyCB = Variant (*)(Variant varA, Variant varB);
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
// Miscellaneous helper functions:
// ========================================================

std::string binaryOpToString(const OpCode op)
{
    static const std::string op2string[]
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
    static_assert(arrayLength(op2string) == NumOps, "Size mismatch!");

    const auto idxOp = static_cast<UInt32>(op);
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

static Variant binaryOpOnPointers(const OpCode op, const Variant varA, const Variant varB)
{
    Variant result{ Variant::Type::Integer }; // Boolean result.
    switch (op)
    {
    case OpCode::CmpNotEqual     : result.value.asInteger = (varA.value.asVoidPtr !=  varB.value.asVoidPtr); break;
    case OpCode::CmpEqual        : result.value.asInteger = (varA.value.asVoidPtr ==  varB.value.asVoidPtr); break;
    case OpCode::CmpGreaterEqual : result.value.asInteger = (varA.value.asVoidPtr >=  varB.value.asVoidPtr); break;
    case OpCode::CmpGreater      : result.value.asInteger = (varA.value.asVoidPtr >   varB.value.asVoidPtr); break;
    case OpCode::CmpLessEqual    : result.value.asInteger = (varA.value.asVoidPtr <=  varB.value.asVoidPtr); break;
    case OpCode::CmpLess         : result.value.asInteger = (varA.value.asVoidPtr <   varB.value.asVoidPtr); break;
    case OpCode::LogicOr         : result.value.asInteger = (varA.value.asVoidPtr or  varB.value.asVoidPtr); break;
    case OpCode::LogicAnd        : result.value.asInteger = (varA.value.asVoidPtr and varB.value.asVoidPtr); break;
    default                      : MOON_UNREACHABLE();
    } // switch (op)
    return result;
}

static Variant binaryOpOnStrings(const OpCode op, const Variant varA, const Variant varB, VM * pVM)
{
    const Str * strA = varA.getAsString();
    const Str * strB = varB.getAsString();

    // Concatenate strings with operator +
    if (op == OpCode::Add)
    {
        MOON_ASSERT(pVM != nullptr);
        Variant newString{ Variant::Type::Str };

        // null str + null str = "(null)"
        if (strA == nullptr && strB == nullptr)
        {
            newString.value.asString = Str::newNullString(*pVM);
        }
        else
        {
            if (strA == nullptr) { strA = Str::newNullString(*pVM); }
            if (strB == nullptr) { strB = Str::newNullString(*pVM); }
            newString.value.asString = Str::newFromStrings(*pVM, *strA, *strB, false);
        }
        return newString;
    }

    // If either is null, fall-back to pointer test:
    if (strA == nullptr || strB == nullptr)
    {
        return binaryOpOnPointers(op, varA, varB);
    }

    // Lexicographical string comparison:
    return Str::binaryOp(op, *strA, *strB);
}

static Variant binaryOpOnRanges(const OpCode op, const Variant varA, const Variant varB)
{
    const auto rangeA = varA.getAsRange();
    const auto rangeB = varB.getAsRange();

    Variant result{ Variant::Type::Integer }; // Boolean result.
    switch (op)
    {
    case OpCode::CmpNotEqual     : result.value.asInteger = (rangeA.begin != rangeB.begin || rangeA.end != rangeB.end); break;
    case OpCode::CmpEqual        : result.value.asInteger = (rangeA.begin == rangeB.begin && rangeA.end == rangeB.end); break;
    case OpCode::CmpGreaterEqual : result.value.asInteger = (rangeLength(rangeA) >= rangeLength(rangeB)); break;
    case OpCode::CmpGreater      : result.value.asInteger = (rangeLength(rangeA) >  rangeLength(rangeB)); break;
    case OpCode::CmpLessEqual    : result.value.asInteger = (rangeLength(rangeA) <= rangeLength(rangeB)); break;
    case OpCode::CmpLess         : result.value.asInteger = (rangeLength(rangeA) <  rangeLength(rangeB)); break;
    default                      : MOON_UNREACHABLE();
    } // switch (op)
    return result;
}

// ========================================================
// isBinaryOpValid():
// ========================================================

bool isBinaryOpValid(const OpCode op, const Variant::Type typeA, const Variant::Type typeB)
{
    const auto idxA  = static_cast<UInt32>(typeA);
    const auto idxB  = static_cast<UInt32>(typeB);
    const auto idxOp = static_cast<UInt32>(op) - FirstOp;

    MOON_ASSERT(idxOp < NumOps);
    MOON_ASSERT(idxA < Cols && idxB < Rows);
    MOON_ASSERT(opsTable[idxA][idxB] != nullptr);

    // Cheap constant-time table lookup.
    return opsTable[idxA][idxB]->ops[idxOp];
}

// ========================================================
// performBinaryOp():
// ========================================================

Variant performBinaryOp(const OpCode op, const Variant varA, const Variant varB, VM * pVM)
{
    if (varA.isAny())
    {
        return performBinaryOp(op, varA.unwrapAny(), varB, pVM);
    }
    if (varB.isAny())
    {
        return performBinaryOp(op, varA, varB.unwrapAny(), pVM);
    }

    if (!isBinaryOpValid(op, varA.type, varB.type))
    {
        MOON_RUNTIME_EXCEPTION("cannot perform binary op " + binaryOpToString(op) + " with " +
                               toString(varA.type) + " and " + toString(varB.type));
    }

    // We have already validated above, so some cases are unreachable.
    const auto idxOp = static_cast<UInt32>(op) - FirstOp;
    switch (varA.type)
    {
    // int and float have implicit conversions:
    case Variant::Type::Integer :
        {
            switch (varB.type)
            {
            case Variant::Type::Integer : return numericalOpsCallbacks[idxOp].IntInt(varA, varB);
            case Variant::Type::Float   : return numericalOpsCallbacks[idxOp].IntFloat(varA, varB);
            default : MOON_UNREACHABLE();
            } // switch (varB.type)
        }
    case Variant::Type::Float :
        {
            switch (varB.type)
            {
            case Variant::Type::Integer : return numericalOpsCallbacks[idxOp].FloatInt(varA, varB);
            case Variant::Type::Float   : return numericalOpsCallbacks[idxOp].FloatFloat(varA, varB);
            default : MOON_UNREACHABLE();
            } // switch (varB.type)
        }
    // Strings operate with other strings, but can also compare with pointers:
    case Variant::Type::Str :
        {
            if (varB.type == Variant::Type::Str) // Both strings?
            {
                return binaryOpOnStrings(op, varA, varB, pVM);
            }
            else // Otherwise treat as raw pointer comparison.
            {
                return binaryOpOnPointers(op, varA, varB);
            }
        }
    // Generic objects only allows basic pointer comparisons:
    case Variant::Type::Tid :
    case Variant::Type::Null :
    case Variant::Type::Array :
    case Variant::Type::Object :
    case Variant::Type::Function :
        {
            return binaryOpOnPointers(op, varA, varB);
        }
    // Other special cases:
    case Variant::Type::Range :
        {
            return binaryOpOnRanges(op, varA, varB);
        }
    default :
        MOON_UNREACHABLE();
    } // switch (varA.type)
}

// ========================================================
// isUnaryOpValid():
// ========================================================

bool isUnaryOpValid(const OpCode op, const Variant::Type type)
{
    if (op == OpCode::LogicNot)
    {
        // Things that can be null/zero:
        return type == Variant::Type::Null     ||
               type == Variant::Type::Integer  ||
               type == Variant::Type::Float    ||
               type == Variant::Type::Function ||
               type == Variant::Type::Tid      ||
               type == Variant::Type::Str      ||
               type == Variant::Type::Object   ||
               type == Variant::Type::Array    ||
               type == Variant::Type::Any;
    }

    // unary -, + only for numbers.
    MOON_ASSERT(op == OpCode::Negate || op == OpCode::Plus);
    return (type == Variant::Type::Integer ||
            type == Variant::Type::Float);
}

// ========================================================
// performUnaryOp():
// ========================================================

Variant performUnaryOp(const OpCode op, const Variant var)
{
    if (var.isAny())
    {
        return performUnaryOp(op, var.unwrapAny());
    }

    if (!isUnaryOpValid(op, var.type))
    {
        MOON_RUNTIME_EXCEPTION("cannot apply unary op " + unaryOpToString(op) +
                               " on " + toString(var.type));
    }

    // Produces a boolean result (returned as integer)
    #define CASE_BOOL_OUTPUT(opType, unOp)                                \
        case OpCode::opType:                                              \
        {                                                                 \
            switch (var.type)                                             \
            {                                                             \
            case Variant::Type::Integer :                                 \
                result.value.asInteger = unOp var.value.asInteger;        \
                result.type = Variant::Type::Integer;                     \
                break;                                                    \
            case Variant::Type::Float :                                   \
                result.value.asInteger = unOp var.value.asFloat;          \
                result.type = Variant::Type::Integer;                     \
                break;                                                    \
            default :                                                     \
                if (var.type == Variant::Type::Str &&                     \
                    var.value.asString != nullptr)                        \
                {                                                         \
                    result = Str::unaryOp(op, *(var.value.asString));     \
                }                                                         \
                else                                                      \
                {                                                         \
                    result.value.asInteger = unOp var.value.asVoidPtr;    \
                    result.type = Variant::Type::Integer;                 \
                }                                                         \
                break;                                                    \
            }                                                             \
            break;                                                        \
        }

    // Preserves the type of the input (only used for number types)
    #define CASE_NUM_OUTPUT(opType, unOp)                                 \
        case OpCode::opType :                                             \
        {                                                                 \
            switch (var.type)                                             \
            {                                                             \
            case Variant::Type::Integer :                                 \
                result.value.asInteger = unOp var.value.asInteger;        \
                result.type = Variant::Type::Integer;                     \
                break;                                                    \
            case Variant::Type::Float :                                   \
                result.value.asFloat = unOp var.value.asFloat;            \
                result.type = Variant::Type::Float;                       \
                break;                                                    \
            default :                                                     \
                MOON_UNREACHABLE();                                       \
            }                                                             \
            break;                                                        \
        }

    Variant result;
    switch (op)
    {
    CASE_BOOL_OUTPUT( LogicNot, not );
    CASE_NUM_OUTPUT(  Negate,   -   );
    CASE_NUM_OUTPUT(  Plus,     +   );
    default : MOON_UNREACHABLE();
    } // switch (op)
    return result;

    #undef CASE_BOOL_OUTPUT
    #undef CASE_NUM_OUTPUT
}

// ========================================================
// isAssignmentValid():
// ========================================================

bool isAssignmentValid(const Variant::Type destType, const Variant::Type srcType)
{
    // Integers and floats convert implicitly.
    if (destType == Variant::Type::Integer ||
        destType == Variant::Type::Float)
    {
        return (srcType == Variant::Type::Integer ||
                srcType == Variant::Type::Float);
    }
    // 'Any' type can receive all the other types.
    else if (destType == Variant::Type::Any)
    {
        return true;
    }
    else // Types must match exactly.
    {
        return destType == srcType;
    }
}

// ========================================================
// performAssignmentWithConversion():
// ========================================================

void performAssignmentWithConversion(Variant & dest, const Variant source)
{
    // 'Any' can assign if the underlaying types match.
    if (source.isAny())
    {
        performAssignmentWithConversion(dest, source.unwrapAny());
        return;
    }

    if (!isAssignmentValid(dest.type, source.type))
    {
        MOON_RUNTIME_EXCEPTION("cannot assign " + toString(source.type) +
                               " to " + toString(dest.type));
    }

    #define CASE_NUMBER_TYPE(destType, destVal)              \
        case destType :                                      \
        {                                                    \
            switch (source.type)                             \
            {                                                \
            case Variant::Type::Integer :                    \
                dest.value.destVal = source.value.asInteger; \
                break;                                       \
            case Variant::Type::Float :                      \
                dest.value.destVal = source.value.asFloat;   \
                break;                                       \
            default :                                        \
                MOON_UNREACHABLE();                          \
            }                                                \
            break;                                           \
        }

    #define CASE_COMMON_TYPE(destType, destVal)              \
        case destType :                                      \
        {                                                    \
            if (source.type == destType)                     \
            {                                                \
                dest.value.destVal = source.value.destVal;   \
            }                                                \
            else                                             \
            {                                                \
                MOON_UNREACHABLE();                          \
            }                                                \
            break;                                           \
        }

    switch (dest.type)
    {
    // Numbers with implicit conversion:
    CASE_NUMBER_TYPE( Variant::Type::Integer,  asInteger  );
    CASE_NUMBER_TYPE( Variant::Type::Float,    asFloat    );

    // Common type that require exact type matching:
    CASE_COMMON_TYPE( Variant::Type::Function, asFunction );
    CASE_COMMON_TYPE( Variant::Type::Tid,      asTypeId   );
    CASE_COMMON_TYPE( Variant::Type::Str,      asString   );
    CASE_COMMON_TYPE( Variant::Type::Object,   asObject   );
    CASE_COMMON_TYPE( Variant::Type::Array,    asArray    );
    CASE_COMMON_TYPE( Variant::Type::Range,    asRange    );
    CASE_COMMON_TYPE( Variant::Type::Null,     asVoidPtr  );

    // The special 'Any' type:
    case Variant::Type::Any :
        {
            // Copy value as raw:
            dest.value = source.value;
            // Preserve the tag to reconstruct the original type later:
            dest.anyType = (source.isAny() ? source.anyType : source.type);
            break;
        }
    default :
        MOON_UNREACHABLE();
    } // switch (dest.type)

    #undef CASE_NUMBER_TYPE
    #undef CASE_COMMON_TYPE
}

// ========================================================
// Minimal unit tests:
// ========================================================

//
// Testing a few of the table entries in isBinaryOpValid() for consistency
// with the expected and a few usage cases for performBinaryOp().
//
#if MOON_DEBUG
namespace
{
struct OpsTableTest
{
    OpsTableTest()
    {
        // Logical:
        MOON_ASSERT(isBinaryOpValid(OpCode::CmpNotEqual, Variant::Type::Integer, Variant::Type::Float)    == true);
        MOON_ASSERT(isBinaryOpValid(OpCode::CmpEqual,    Variant::Type::Float,   Variant::Type::Integer)  == true);
        MOON_ASSERT(isBinaryOpValid(OpCode::CmpLess,     Variant::Type::Str,     Variant::Type::Str)      == true);
        MOON_ASSERT(isBinaryOpValid(OpCode::LogicAnd,    Variant::Type::Float,   Variant::Type::Float)    == true);

        MOON_ASSERT(isBinaryOpValid(OpCode::CmpNotEqual, Variant::Type::Integer, Variant::Type::Str)      == false);
        MOON_ASSERT(isBinaryOpValid(OpCode::CmpEqual,    Variant::Type::Float,   Variant::Type::Function) == false);
        MOON_ASSERT(isBinaryOpValid(OpCode::CmpLess,     Variant::Type::Str,     Variant::Type::Null)     == false);
        MOON_ASSERT(isBinaryOpValid(OpCode::LogicAnd,    Variant::Type::Str,     Variant::Type::Str)      == true);
        MOON_ASSERT(isBinaryOpValid(OpCode::LogicOr,     Variant::Type::Str,     Variant::Type::Str)      == true);

        // Arithmetical:
        MOON_ASSERT(isBinaryOpValid(OpCode::Add, Variant::Type::Str,     Variant::Type::Str)      == true);
        MOON_ASSERT(isBinaryOpValid(OpCode::Add, Variant::Type::Integer, Variant::Type::Float)    == true);
        MOON_ASSERT(isBinaryOpValid(OpCode::Sub, Variant::Type::Float,   Variant::Type::Integer)  == true);
        MOON_ASSERT(isBinaryOpValid(OpCode::Mod, Variant::Type::Float,   Variant::Type::Float)    == true);
        MOON_ASSERT(isBinaryOpValid(OpCode::Mod, Variant::Type::Integer, Variant::Type::Float)    == true);

        MOON_ASSERT(isBinaryOpValid(OpCode::Add, Variant::Type::Str,     Variant::Type::Float)    == false);
        MOON_ASSERT(isBinaryOpValid(OpCode::Add, Variant::Type::Integer, Variant::Type::Str)      == false);
        MOON_ASSERT(isBinaryOpValid(OpCode::Sub, Variant::Type::Float,   Variant::Type::Function) == false);
        MOON_ASSERT(isBinaryOpValid(OpCode::Mod, Variant::Type::Str,     Variant::Type::Integer)  == false);
        MOON_ASSERT(isBinaryOpValid(OpCode::Div, Variant::Type::Str,     Variant::Type::Str)      == false);

        logStream() << "Moon: Operations table test passed.\n";
    }
} localOpsTableTest;

struct BinOpsTest
{
    BinOpsTest()
    {
        Variant varA;
        Variant varB;
        Variant result;

        // Logical:
        varA.type = Variant::Type::Integer;
        varA.value.asInteger = 1;
        varB.type = Variant::Type::Float;
        varB.value.asFloat = 1;
        result = performBinaryOp(OpCode::CmpEqual, varA, varB, nullptr);
        MOON_ASSERT(result.type == Variant::Type::Integer && result.toBool() == true);

        varA.type = Variant::Type::Null;
        varA.value.asVoidPtr = nullptr;
        varB.type = Variant::Type::Function;
        varB.value.asFunction = nullptr;
        result = performBinaryOp(OpCode::LogicOr, varA, varB, nullptr);
        MOON_ASSERT(result.type == Variant::Type::Integer && result.toBool() == false);

        // Arithmetical:
        varA.type = Variant::Type::Integer;
        varA.value.asInteger = 2;
        varB.type = Variant::Type::Float;
        varB.value.asFloat = 2;
        result = performBinaryOp(OpCode::Mul, varA, varB, nullptr);
        MOON_ASSERT(result.type == Variant::Type::Float && result.value.asFloat == 4);

        varA.type = Variant::Type::Float;
        varA.value.asFloat = 3;
        varB.type = Variant::Type::Float;
        varB.value.asFloat = 2;
        result = performBinaryOp(OpCode::Mod, varA, varB, nullptr);
        MOON_ASSERT(result.type == Variant::Type::Float && result.value.asFloat == 1);

        varA.type = Variant::Type::Integer;
        varA.value.asInteger = 3;
        varB.type = Variant::Type::Integer;
        varB.value.asInteger = 2;
        result = performBinaryOp(OpCode::Mod, varA, varB, nullptr);
        MOON_ASSERT(result.type == Variant::Type::Integer && result.value.asInteger == 1);

        logStream() << "Moon: Binary ops test passed.\n";
    }
} localBinOpsTest;
} // namespace {}
#endif // MOON_DEBUG

} // namespace moon {}
