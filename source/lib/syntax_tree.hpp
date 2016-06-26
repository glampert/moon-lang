
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

namespace moon
{

// ========================================================
// struct SyntaxTreeNode:
// ========================================================

struct SyntaxTreeNode final
{
    enum class Eval : UInt8
    {
        // Placeholder types (used internally):
        Undefined,
        Null,
        Void,
        VarArgs,

        // Actual types referenced in code:
        Int,
        Long,
        Float,
        Double,
        Bool,
        Str,
        Array,
        Range,
        Any,
        Func,
        Tid,

        // User Defined Type flag.
        // (accompanied by an identifier/symbol).
        UDT,

        // Sentinel value; used internally.
        Count
    };

    enum class Type : UInt8
    {
        // Other:
        NoOp,
        TranslationUnit,

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
        ExprMemberRef,
        ExprNameIdent,
        ExprTypeIdent,
        ExprLiteralConst,
        ExprObjectConstructor,
        ExprTypecast,
        ExprTypeof,
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

    const SyntaxTreeNode * children[3]; // Pointers to this node's children, if any.
    const Symbol * const symbol;        // Pointer to a symbol, if applicable.
    const Type nodeType;                // What type of node is this. See the 'Type' enum above.
    Eval evalType;                      // Evaluation or "return value" type of the node. Might be updated after construction.

    // All-in-one constructor:
    SyntaxTreeNode(Type type,
            const Symbol * sym,
            const SyntaxTreeNode * child0,
            const SyntaxTreeNode * child1,
            const SyntaxTreeNode * child2,
            SyntaxTreeNode::Eval eval) noexcept;

    // Helper used internally by SyntaxTree to recursively print its nodes.
    void print(int level, int childIndex, std::ostream & os) const;

    //
    // Accessors with debug bounds checking:
    //
    void setChild(const int index, const SyntaxTreeNode * child)
    {
        MOON_ASSERT(index >= 0 && index < 3);
        children[index] = child;
    }
    const SyntaxTreeNode * getChild(const int index) const
    {
        MOON_ASSERT(index >= 0 && index < 3);
        return children[index];
    }
    const Symbol * getChildSymbol(const int index) const
    {
        MOON_ASSERT(index >= 0 && index < 3);
        MOON_ASSERT(children[index] != nullptr);
        return children[index]->symbol;
    }
};

// Node enum constants to printable strings (with color tags):
std::string toString(SyntaxTreeNode::Type nodeType);
std::string toString(SyntaxTreeNode::Eval evalType);

// ========================================================
// class SyntaxTree:
// ========================================================

class SyntaxTree final
{
public:

    SyntaxTree() = default;
    SyntaxTree(const SyntaxTree &) = delete;
    SyntaxTree & operator = (const SyntaxTree &) = delete;

    bool isEmpty() const noexcept { return nodePool.getObjectsAlive() == 0; }
    int  getSize() const noexcept { return nodePool.getObjectsAlive();      }

    void setRoot(const SyntaxTreeNode * newRoot) noexcept { root = newRoot; }
    const SyntaxTreeNode * getRoot() const noexcept       { return root;    }

    // Simple recursive listing of each node for debug logging.
    void print(std::ostream & os) const;

    // Does a full tree traversal and performs basic checking on each
    // node for consistency with the expected states for each node type.
    // This operation can be slow, so it is only advised using it for debug
    // validation and internal assertions. If an inconsistency is found,
    // the function throws with MOON_INTERNAL_EXCEPTION().
    void validateNodes() const;

    //
    // Allocate and construct a new node, incrementing the tree's node count.
    // The new node is still not linked to the tree. After allocation, a node
    // is normally passed to another 'newNode*()' call to link it as a child|leaf.
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

    static void checkNodeRecursive(const SyntaxTreeNode * const node);

    // Also a reference to a node in the pool.
    const SyntaxTreeNode * root = nullptr;

    // All nodes are sourced from this pool.
    Pool<SyntaxTreeNode, MOON_AST_NODE_POOL_GRANULARITY> nodePool;
};

inline std::ostream & operator << (std::ostream & os, const SyntaxTree & syntaxTree)
{
    syntaxTree.print(os);
    return os;
}

} // namespace moon {}

#endif // MOON_SYNTAX_TREE_HPP
