
// ================================================================================================
// -*- C++ -*-
// File: syntax_tree.hpp
// Author: Guilherme R. Lampert
// Created on: 14/06/15
// Brief: Syntax Tree definition.
// ================================================================================================

#ifndef MOON_SYNTAX_TREE_HPP
#define MOON_SYNTAX_TREE_HPP

#include "common.hpp"
#include "pool.hpp"

#include <iostream>

#ifndef MOON_AST_NODE_POOL_GRANULARITY
    #define MOON_AST_NODE_POOL_GRANULARITY 512
#endif // MOON_AST_NODE_POOL_GRANULARITY

namespace moon
{

// ========================================================
// class SyntaxTreeNode:
// ========================================================

class SyntaxTreeNode final
{
public:

    enum class Eval : std::uint8_t
    {
        // Placeholder types (used internally):
        Undefined,
        Void,
        VarArgs,

        // Actual types referenced in code:
        Int,
        Long,
        Float,
        Bool,
        String,
        Array,
        Range,
        Any,

        // User Defined Type flag (must be accompanied by an identifiers/symbol).
        UDT,

        // Sentinel value; used internally.
        Count
    };

    enum class Type : std::uint8_t
    {
        // Other:
        TranslationUnit,
        ModuleDefinition,

        // Statements/declarations:
        Statement,
        IfThenStatement,
        IfThenElseStatement,
        IfThenElseIfStatement,
        LoopStatement,
        WhileStatement,
        ForStatement,
        MatchStatement,
        MatchCaseStatement,
        MatchDefaultStatement,
        FuncDeclStatement,
        EnumDeclStatement,
        StructDeclStatement,
        TypeAliasDeclStatement,
        VarDeclStatement,
        ReturnStatement,
        BreakStatement,
        ContinueStatement,

        // Expressions:
        ExprRange,
        ExprArrayLiteral,
        ExprArraySubscript,
        ExprFuncCall,
        ExprNameIdent,
        ExprTypeIdent,
        ExprLiteralConst,
        ExprObjectConstructor,
        ExprAssign,
        ExprCmpNotEqual,
        ExprCmpEqual,
        ExprCmpGreaterEqual,
        ExprCmpGreaterThan,
        ExprCmpLessEqual,
        ExprCmpLessThan,
        ExprLogicOr,
        ExprLogicAnd,
        ExprLogicNot,
        ExprSubtract,
        ExprAdd,
        ExprModulo,
        ExprDivide,
        ExprMultiply,
        ExprSubAssign,
        ExprAddAssign,
        ExprModAssign,
        ExprDivAssign,
        ExprMulAssign,
        ExprUnaryMinus,
        ExprUnaryPlus,

        // Sentinel value; used internally.
        Count
    };

    // All-in-one constructor:
    SyntaxTreeNode(Type type,
            const Symbol * sym,
            const SyntaxTreeNode * child0,
            const SyntaxTreeNode * child1,
            const SyntaxTreeNode * child2,
            SyntaxTreeNode::Eval eval);

    // Helper used internally by SyntaxTree to recursively print its nodes.
    void print(int level, int childIndex, std::ostream & os = std::cout) const;

    //
    // Accessors:
    //
    void setChild(const int index, const SyntaxTreeNode * child) noexcept
    {
        MOON_ASSERT(index >= 0 && index < 3);
        children[index] = child;
    }
    const SyntaxTreeNode * getChild(const int index) const noexcept
    {
        MOON_ASSERT(index >= 0 && index < 3);
        return children[index];
    }
    const Symbol * getChildSymbol(const int index) const noexcept
    {
        MOON_ASSERT(index >= 0 && index < 3);
        return children[index]->symbol;
    }
    const Symbol * getSymbol() const noexcept { return symbol; }
    Type getType() const noexcept { return nodeType; }
    Eval getEval() const noexcept { return evalType; }

private:

    const SyntaxTreeNode * children[3]; // Pointers to the these node's children.
    const Symbol * symbol;              // Pointer to a symbol, if applicable.
    const Type nodeType;                // What type of node is it? See the `Type` enum.
    Eval evalType;                      // Evaluation or "return" type of the node. May be deduced after construction.
};

// Node enum constants to printable strings (with color tags):
std::string toString(SyntaxTreeNode::Type nodeType);
std::string toString(SyntaxTreeNode::Eval evalType);

// Deduce AST node evaluation type from its symbol or vice-versa:
SyntaxTreeNode::Eval evalTypeFromSymbol(const Symbol & sym);
const Symbol * symbolFromEval(const SymbolTable & symTable, SyntaxTreeNode::Eval eval);

// ========================================================
// class SyntaxTree:
// ========================================================

class SyntaxTree final
{
public:

    SyntaxTree();

    // Not copyable.
    SyntaxTree(const SyntaxTree &) = delete;
    SyntaxTree & operator = (const SyntaxTree &) = delete;

    // Miscellaneous queries:
    bool isEmpty() const noexcept;
    std::size_t getSize() const noexcept;
    const SyntaxTreeNode * getRoot() const noexcept;
    void setRoot(const SyntaxTreeNode * newRoot) noexcept;

    // Simple recursive listing of each node for debug logging.
    void print(std::ostream & os = std::cout) const;

    //
    // Allocate and construct a new node, incrementing the tree's node count.
    // However, the new node is still not linked to the tree. After allocation,
    // a node is normally passed to another `newNode*()` call to link it as a child/leaf.
    //

    SyntaxTreeNode * newNode(SyntaxTreeNode::Type type,
                             const SyntaxTreeNode * child0 = nullptr,
                             const SyntaxTreeNode * child1 = nullptr,
                             const SyntaxTreeNode * child2 = nullptr,
                             SyntaxTreeNode::Eval eval = SyntaxTreeNode::Eval::Undefined);

    SyntaxTreeNode * newNodeWithSymbol(SyntaxTreeNode::Type type,
                                       const Symbol * symbol,
                                       const SyntaxTreeNode * child0 = nullptr,
                                       const SyntaxTreeNode * child1 = nullptr,
                                       const SyntaxTreeNode * child2 = nullptr,
                                       SyntaxTreeNode::Eval eval = SyntaxTreeNode::Eval::Undefined);

    SyntaxTreeNode * newNodeWithEval(SyntaxTreeNode::Type type,
                                     const Symbol * symbol,
                                     SyntaxTreeNode::Eval eval);

private:

    // Fetch a node from the pool and increment nodeCount.
    SyntaxTreeNode * allocNode();

    // Also a reference to a node in the pool.
    const SyntaxTreeNode * root;

    // Nodes allocated from pool. For our purposes, the tree size.
    std::size_t nodeCount;

    // All nodes are sourced from this pool.
    ObjectPool<SyntaxTreeNode, MOON_AST_NODE_POOL_GRANULARITY> nodePool;
};

std::ostream & operator << (std::ostream & os, const SyntaxTree & syntaxTree);

} // namespace moon {}

#endif // MOON_SYNTAX_TREE_HPP
