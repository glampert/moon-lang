
// ================================================================================================
// -*- C++ -*-
// File: syntax_tree.cpp
// Author: Guilherme R. Lampert
// Created on: 15/06/15
// Brief: Syntax Tree definition.
// ================================================================================================

#include "syntax_tree.hpp"
#include "symbol_table.hpp"

namespace moon
{

// ========================================================
// Abstract Syntax Tree helpers:
// ========================================================

std::string toString(const SyntaxTreeNode::Type nodeType)
{
    static const std::string typeNames[]
    {
        color::green()   + std::string("NOOP")                      + color::restore(),
        color::magenta() + std::string("TRANSLATION_UNIT")          + color::restore(),
        color::green()   + std::string("STATEMENT")                 + color::restore(),
        color::red()     + std::string("IF_THEN_STATEMENT")         + color::restore(),
        color::red()     + std::string("IF_THEN_ELSE_STATEMENT")    + color::restore(),
        color::red()     + std::string("IF_THEN_ELSEIF_STATEMENT")  + color::restore(),
        color::red()     + std::string("LOOP_STATEMENT")            + color::restore(),
        color::red()     + std::string("WHILE_STATEMENT")           + color::restore(),
        color::red()     + std::string("FOR_STATEMENT")             + color::restore(),
        color::red()     + std::string("MATCH_STATEMENT")           + color::restore(),
        color::red()     + std::string("MATCH_CASE_STATEMENT")      + color::restore(),
        color::red()     + std::string("MATCH_DEFAULT_STATEMENT")   + color::restore(),
        color::white()   + std::string("FUNC_DECL_STATEMENT")       + color::restore(),
        color::white()   + std::string("ENUM_DECL_STATEMENT")       + color::restore(),
        color::white()   + std::string("STRUCT_DECL_STATEMENT")     + color::restore(),
        color::white()   + std::string("TYPE_ALIAS_DECL_STATEMENT") + color::restore(),
        color::white()   + std::string("VAR_DECL_STATEMENT")        + color::restore(),
        color::white()   + std::string("RETURN_STATEMENT")          + color::restore(),
        color::white()   + std::string("BREAK_STATEMENT")           + color::restore(),
        color::white()   + std::string("CONTINUE_STATEMENT")        + color::restore(),
        color::cyan()    + std::string("EXPR_RANGE")                + color::restore(),
        color::cyan()    + std::string("EXPR_ARRAY_LITERAL")        + color::restore(),
        color::cyan()    + std::string("EXPR_ARRAY_SUBSCRIPT")      + color::restore(),
        color::cyan()    + std::string("EXPR_FUNC_CALL")            + color::restore(),
        color::cyan()    + std::string("EXPR_MEMBER_REF")           + color::restore(),
        color::cyan()    + std::string("EXPR_NAME_IDENT")           + color::restore(),
        color::cyan()    + std::string("EXPR_TYPE_IDENT")           + color::restore(),
        color::cyan()    + std::string("EXPR_LITERAL_CONST")        + color::restore(),
        color::cyan()    + std::string("EXPR_OBJ_CONSTRUCTOR")      + color::restore(),
        color::magenta() + std::string("EXPR_TYPECAST")             + color::restore(),
        color::magenta() + std::string("EXPR_TYPEOF")               + color::restore(),
        color::blue()    + std::string("EXPR_ASSIGN")               + color::restore(),
        color::blue()    + std::string("EXPR_CMP_NOT_EQUAL")        + color::restore(),
        color::blue()    + std::string("EXPR_CMP_EQUAL")            + color::restore(),
        color::blue()    + std::string("EXPR_CMP_GREATER_EQUAL")    + color::restore(),
        color::blue()    + std::string("EXPR_CMP_GREATER_THAN")     + color::restore(),
        color::blue()    + std::string("EXPR_CMP_LESS_EQUAL")       + color::restore(),
        color::blue()    + std::string("EXPR_CMP_LESS_THAN")        + color::restore(),
        color::magenta() + std::string("EXPR_LOGIC_OR")             + color::restore(),
        color::magenta() + std::string("EXPR_LOGIC_AND")            + color::restore(),
        color::magenta() + std::string("EXPR_LOGIC_NOT")            + color::restore(),
        color::blue()    + std::string("EXPR_SUBTRACT")             + color::restore(),
        color::blue()    + std::string("EXPR_ADD")                  + color::restore(),
        color::blue()    + std::string("EXPR_MODULO")               + color::restore(),
        color::blue()    + std::string("EXPR_DIVIDE")               + color::restore(),
        color::blue()    + std::string("EXPR_MULTIPLY")             + color::restore(),
        color::blue()    + std::string("EXPR_SUB_ASSIGN")           + color::restore(),
        color::blue()    + std::string("EXPR_ADD_ASSIGN")           + color::restore(),
        color::blue()    + std::string("EXPR_MOD_ASSIGN")           + color::restore(),
        color::blue()    + std::string("EXPR_DIV_ASSIGN")           + color::restore(),
        color::blue()    + std::string("EXPR_MUL_ASSIGN")           + color::restore(),
        color::magenta() + std::string("EXPR_UNARY_MINUS")          + color::restore(),
        color::magenta() + std::string("EXPR_UNARY_PLUS")           + color::restore()
    };
    static_assert(arrayLength(typeNames) == UInt32(SyntaxTreeNode::Type::Count),
                  "Keep this array in sync with the enum declaration!");

    return typeNames[UInt32(nodeType)];
}

std::string toString(const SyntaxTreeNode::Eval evalType)
{
    static const std::string typeNames[]
    {
        color::red()    + std::string("EVAL_UNDEF")   + color::restore(),
        color::yellow() + std::string("EVAL_NULL")    + color::restore(),
        color::yellow() + std::string("EVAL_VOID")    + color::restore(),
        color::yellow() + std::string("EVAL_VARARGS") + color::restore(),
        color::yellow() + std::string("EVAL_INT")     + color::restore(),
        color::yellow() + std::string("EVAL_LONG")    + color::restore(),
        color::yellow() + std::string("EVAL_FLOAT")   + color::restore(),
        color::yellow() + std::string("EVAL_DOUBLE")  + color::restore(),
        color::yellow() + std::string("EVAL_BOOL")    + color::restore(),
        color::yellow() + std::string("EVAL_STR")     + color::restore(),
        color::yellow() + std::string("EVAL_ARRAY")   + color::restore(),
        color::yellow() + std::string("EVAL_RANGE")   + color::restore(),
        color::yellow() + std::string("EVAL_ANY")     + color::restore(),
        color::yellow() + std::string("EVAL_FUNC")    + color::restore(),
        color::yellow() + std::string("EVAL_TID")     + color::restore(),
        color::yellow() + std::string("EVAL_UDT")     + color::restore()
    };
    static_assert(arrayLength(typeNames) == UInt32(SyntaxTreeNode::Eval::Count),
                  "Keep this array in sync with the enum declaration!");

    return typeNames[UInt32(evalType)];
}

// ========================================================
// SyntaxTreeNode:
// ========================================================

SyntaxTreeNode::SyntaxTreeNode(const Type type,
                               const Symbol * sym,
                               const SyntaxTreeNode * child0,
                               const SyntaxTreeNode * child1,
                               const SyntaxTreeNode * child2,
                               const SyntaxTreeNode::Eval eval) noexcept
    : symbol   { sym  }
    , nodeType { type }
    , evalType { eval }
{
    children[0] = child0;
    children[1] = child1;
    children[2] = child2;
}

void SyntaxTreeNode::print(const int level, const int childIndex, std::ostream & os) const
{
    std::string prefix = "+-";
    if (level != 0)
    {
        for (int l = 0; l < level; ++l)
        {
            prefix += "---";
        }
    }

    os << prefix << toString(nodeType) << "(" << level << ":" << childIndex << ", ";
    if (symbol != nullptr)
    {
        if (symbol->type == Symbol::Type::StrLiteral)
        {
            os << "'" << color::magenta() << unescapeString(toString(*symbol).c_str()) << color::restore() << "', ";
        }
        else
        {
            os << "'" << color::magenta() << toString(*symbol) << color::restore() << "', ";
        }
    }
    os << toString(evalType) << ")\n";

    if (nodeType == Type::Statement)
    {
        for (Int32 i = arrayLength(children) - 1; i >= 0; --i)
        {
            if (children[i]) { children[i]->print(level + 1, i, os); }
        }
    }
    else
    {
        for (UInt32 i = 0; i < arrayLength(children); ++i)
        {
            if (children[i]) { children[i]->print(level + 1, i, os); }
        }
    }
}

// ========================================================
// SyntaxTree:
// ========================================================

SyntaxTreeNode * SyntaxTree::newNode(const SyntaxTreeNode::Type type,
                                     const SyntaxTreeNode * child0,
                                     const SyntaxTreeNode * child1,
                                     const SyntaxTreeNode * child2,
                                     const SyntaxTreeNode::Eval eval)
{
    return construct(nodePool.allocate(), type, nullptr, child0, child1, child2, eval);
}

SyntaxTreeNode * SyntaxTree::newNodeWithSymbol(const SyntaxTreeNode::Type type,
                                               const Symbol * symbol,
                                               const SyntaxTreeNode * child0,
                                               const SyntaxTreeNode * child1,
                                               const SyntaxTreeNode * child2,
                                               const SyntaxTreeNode::Eval eval)
{
    return construct(nodePool.allocate(), type, symbol, child0, child1, child2, eval);
}

SyntaxTreeNode * SyntaxTree::newNodeWithEval(const SyntaxTreeNode::Type type,
                                             const Symbol * symbol,
                                             const SyntaxTreeNode::Eval eval)
{
    return construct(nodePool.allocate(), type, symbol, nullptr, nullptr, nullptr, eval);
}

void SyntaxTree::print(std::ostream & os) const
{
    os << color::white() << "[[ begin syntax tree dump ]]" << color::restore() << "\n";
    if (!isEmpty() && root != nullptr)
    {
        root->print(0, 0, os);
    }
    else
    {
        os << "(empty)\n";
    }
    os << color::white() << "[[ listed " << getSize() << " tree nodes ]]" << color::restore() << "\n";
}

void SyntaxTree::checkNodeRecursive(const SyntaxTreeNode * const node)
{
    if (node == nullptr)
    {
        return;
    }

    if (static_cast<UInt32>(node->nodeType) >= static_cast<UInt32>(SyntaxTreeNode::Type::Count))
    {
        MOON_INTERNAL_EXCEPTION("invalid node type detected!");
    }

    if (static_cast<UInt32>(node->evalType) >= static_cast<UInt32>(SyntaxTreeNode::Eval::Count))
    {
        MOON_INTERNAL_EXCEPTION("invalid node eval detected!");
    }

    if (node->evalType == SyntaxTreeNode::Eval::Undefined)
    {
        if (node->symbol != nullptr)
        {
            MOON_INTERNAL_EXCEPTION("eval of node '" + toString(node->nodeType) + "' ('" +
                                    toString(node->symbol->name) + "') is undefined");
        }
        else
        {
            MOON_INTERNAL_EXCEPTION("eval of node '" + toString(node->nodeType) + "' is undefined");
        }
    }

    // NOTE: We could also check if the expected number
    // of children is correct for each node type, check
    // that nodes requiring a symbol have one, etc. This
    // adds to overhead, but gives a more thorough debug
    // checking setup. I'll leave the idea as a to-do for now.

    for (UInt32 i = 0; i < arrayLength(node->children); ++i)
    {
        checkNodeRecursive(node->getChild(i));
    }
}

void SyntaxTree::validateNodes() const
{
    checkNodeRecursive(root);
}

} // namespace moon {}
