
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
        color::magenta() + std::string("TRANSLATION_UNIT")          + color::restore(),
        color::magenta() + std::string("MODULE_DEFINITION")         + color::restore(),
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
        color::cyan()    + std::string("EXPR_NAME_IDENT")           + color::restore(),
        color::cyan()    + std::string("EXPR_TYPE_IDENT")           + color::restore(),
        color::cyan()    + std::string("EXPR_LITERAL_CONST")        + color::restore(),
        color::cyan()    + std::string("EXPR_OBJ_CONSTRUCTOR")      + color::restore(),
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
    static_assert(arrayLength(typeNames) == unsigned(SyntaxTreeNode::Type::Count),
                  "Keep this array in sync with the enum declaration!");

    return typeNames[unsigned(nodeType)];
}

std::string toString(const SyntaxTreeNode::Eval evalType)
{
    static const std::string typeNames[]
    {
        color::yellow() + std::string("EVAL_UNDEF")   + color::restore(),
        color::yellow() + std::string("EVAL_VOID")    + color::restore(),
        color::yellow() + std::string("EVAL_VARARGS") + color::restore(),
        color::yellow() + std::string("EVAL_INT")     + color::restore(),
        color::yellow() + std::string("EVAL_LONG")    + color::restore(),
        color::yellow() + std::string("EVAL_FLOAT")   + color::restore(),
        color::yellow() + std::string("EVAL_BOOL")    + color::restore(),
        color::yellow() + std::string("EVAL_STRING")  + color::restore(),
        color::yellow() + std::string("EVAL_ARRAY")   + color::restore(),
        color::yellow() + std::string("EVAL_RANGE")   + color::restore(),
        color::yellow() + std::string("EVAL_ANY")     + color::restore(),
        color::yellow() + std::string("EVAL_UDT")     + color::restore()
    };
    static_assert(arrayLength(typeNames) == unsigned(SyntaxTreeNode::Eval::Count),
                  "Keep this array in sync with the enum declaration!");

    return typeNames[unsigned(evalType)];
}

SyntaxTreeNode::Eval evalTypeFromSymbol(const Symbol & sym)
{
    switch (sym.type)
    {
    case Symbol::Type::IntLiteral   : return SyntaxTreeNode::Eval::Long;
    case Symbol::Type::FloatLiteral : return SyntaxTreeNode::Eval::Float;
    case Symbol::Type::BoolLiteral  : return SyntaxTreeNode::Eval::Bool;
    case Symbol::Type::StrLiteral   : return SyntaxTreeNode::Eval::String;
    case Symbol::Type::Identifier   : return SyntaxTreeNode::Eval::UDT;
    default                         : return SyntaxTreeNode::Eval::Undefined;
    } // switch (sym.type)
}

const Symbol * symbolFromEval(const SymbolTable & symTable, const SyntaxTreeNode::Eval eval)
{
    // These are built-ins always present in the symbol table. Check symbol_table.cpp.
    switch (eval)
    {
    case SyntaxTreeNode::Eval::Void    : return symTable.findSymbol("void");
    case SyntaxTreeNode::Eval::VarArgs : return symTable.findSymbol("varargs");
    case SyntaxTreeNode::Eval::Int     : return symTable.findSymbol("int");
    case SyntaxTreeNode::Eval::Long    : return symTable.findSymbol("long");
    case SyntaxTreeNode::Eval::Float   : return symTable.findSymbol("float");
    case SyntaxTreeNode::Eval::Bool    : return symTable.findSymbol("bool");
    case SyntaxTreeNode::Eval::String  : return symTable.findSymbol("string");
    case SyntaxTreeNode::Eval::Array   : return symTable.findSymbol("array");
    case SyntaxTreeNode::Eval::Range   : return symTable.findSymbol("range");
    case SyntaxTreeNode::Eval::Any     : return symTable.findSymbol("any");
    case SyntaxTreeNode::Eval::UDT     : return symTable.findSymbol("object");
    default                            : return symTable.findSymbol("undefined");
    } // switch (eval)
}

// ========================================================
// SyntaxTreeNode class methods:
// ========================================================

SyntaxTreeNode::SyntaxTreeNode(const Type type,
                               const Symbol * sym,
                               const SyntaxTreeNode * child0,
                               const SyntaxTreeNode * child1,
                               const SyntaxTreeNode * child2,
                               const SyntaxTreeNode::Eval eval)
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
    if (symbol)
    {
        os << "'" << color::magenta() << unescapeString(toString(*symbol).c_str()) << color::restore() << "', ";
    }
    os << toString(evalType) << ")\n";

    if (nodeType == Type::Statement)
    {
        for (int i = arrayLength(children) - 1; i >= 0; --i)
        {
            if (children[i]) { children[i]->print(level + 1, i, os); }
        }
    }
    else
    {
        for (unsigned i = 0; i < arrayLength(children); ++i)
        {
            if (children[i]) { children[i]->print(level + 1, i, os); }
        }
    }
}

// ========================================================
// SyntaxTree class methods:
// ========================================================

SyntaxTree::SyntaxTree()
    : root      { nullptr }
    , nodeCount { 0 }
{ }

bool SyntaxTree::isEmpty() const noexcept
{
    return nodeCount == 0;
}

const SyntaxTreeNode * SyntaxTree::getRoot() const noexcept
{
    return root;
}

void SyntaxTree::setRoot(const SyntaxTreeNode * newRoot) noexcept
{
    root = newRoot;
}

std::size_t SyntaxTree::getSize() const noexcept
{
    return nodeCount;
}

SyntaxTreeNode * SyntaxTree::newNode(const SyntaxTreeNode::Type type,
                                     const SyntaxTreeNode * child0,
                                     const SyntaxTreeNode * child1,
                                     const SyntaxTreeNode * child2,
                                     const SyntaxTreeNode::Eval eval)
{
    return construct(allocNode(), type, nullptr, child0, child1, child2, eval);
}

SyntaxTreeNode * SyntaxTree::newNodeWithSymbol(const SyntaxTreeNode::Type type,
                                               const Symbol * symbol,
                                               const SyntaxTreeNode * child0,
                                               const SyntaxTreeNode * child1,
                                               const SyntaxTreeNode * child2,
                                               const SyntaxTreeNode::Eval eval)
{
    return construct(allocNode(), type, symbol, child0, child1, child2, eval);
}

SyntaxTreeNode * SyntaxTree::newNodeWithEval(const SyntaxTreeNode::Type type,
                                             const Symbol * symbol,
                                             const SyntaxTreeNode::Eval eval)
{
    return construct(allocNode(), type, symbol, nullptr, nullptr, nullptr, eval);
}

SyntaxTreeNode * SyntaxTree::allocNode()
{
    auto node = nodePool.allocate();
    ++nodeCount;
    return node;
}

void SyntaxTree::print(std::ostream & os) const
{
    os << color::white() << "[[ begin syntax tree dump ]]" << color::restore() << "\n";
    if (nodeCount != 0 && root != nullptr)
    {
        root->print(0, 0, os);
    }
    else
    {
        os << "(empty)\n";
    }
    os << color::white() << "[[ listed " << nodeCount << " tree nodes ]]" << color::restore() << "\n";
}

std::ostream & operator << (std::ostream & os, const SyntaxTree & syntaxTree)
{
    syntaxTree.print(os);
    return os;
}

} // namespace moon {}
