
// ================================================================================================
// -*- C++ -*-
// File: semantic_check.hpp
// Author: Guilherme R. Lampert
// Created on: 17/04/16
// Brief: Code related to semantic and type checking as well as other parsing utilities.
// ================================================================================================

#ifndef MOON_SEMANTIC_CHECK_HPP
#define MOON_SEMANTIC_CHECK_HPP

#include "symbol_table.hpp"
#include "syntax_tree.hpp"
#include "runtime.hpp"
#include <vector>

namespace moon
{

// ========================================================
// Parse-time variable type info:
// ========================================================

struct VarInfo final
{
    const TypeId   *     typeId;    // Runtime TypeId for UDTs and native types alike.
    const Function *     funcPtr;   // If the var is a pointer-to-function, we can resolve it at compile-time.
    Variant::Type        varType;   // Final Variant type. Matches stEval.
    SyntaxTreeNode::Eval arrayEval; // Only relevant if varType=Array. Data type stored in an array var.
    SyntaxTreeNode::Eval stEval;    // Matches varType; Cached conversion for frequent use.
};

class VarInfoTable final
{
public:

    explicit VarInfoTable(ParseContext & parseCtx)
        : ctx{ parseCtx }
    { }

    VarInfoTable(const VarInfoTable &) = delete;
    VarInfoTable & operator = (const VarInfoTable &) = delete;

    void beginFunc(const Symbol * funcNameSymbol);
    void endFunc();

    void beginUDT(const Symbol * typeNameSymbol, const char * elementName);
    void endUDT();

    void beginForLoop(const Symbol * iteratorVarSymbol, const SyntaxTreeNode * iterableNode);
    void endForLoop();

    const VarInfo * findVar(const Symbol * symbol) const;
    const VarInfo * findGlobalVar(const Symbol * symbol) const;
    const VarInfo * findFuncLocalVar(const Symbol * symbol) const;

    void addGlobalVar(const Symbol * varNameSymbol, const VarInfo & vi);
    void addFuncLocalVar(const Symbol * varNameSymbol, const VarInfo & vi);

    bool removeVar(const Symbol * symbol);
    bool removeGlobalVar(const Symbol * symbol);
    bool removeFuncLocalVar(const Symbol * symbol);

    bool isParsingFunction() const noexcept { return currentFuncSymbol != nullptr; }
    bool isParsingUDT()      const noexcept { return currentUDTSymbol  != nullptr; }

    // Transient states:
    const TypeId * lastRhsTypeId     = nullptr;
    const Symbol * currentFuncSymbol = nullptr;
    const Symbol * currentUDTSymbol  = nullptr;

    // Stuff use only inside loops/functions:
    bool                   insideLoopStmt      = false;
    bool                   foundReturnStmt     = false;
    SyntaxTreeNode::Eval   expectedReturnType  = SyntaxTreeNode::Eval::Void;
    std::uint32_t          forLoopIdCounter    = 0;
    const SyntaxTreeNode * forLoopIteratorNode = nullptr;

private:

    // To call the error handlers.
    ParseContext & ctx;

    // [var_name_symbol, var_info_record]
    using VarTable = HashTable<const Symbol *, VarInfo>;

    // [func_name_symbol, table_of_local_vars]
    using FuncRecord = std::pair<const Symbol *, VarTable>;

    // All globals in a translation unit or program.
    VarTable globalVars;

    // All functions in a translation unit or program. Each function
    // has a HashTable with a record for every local var + parameters.
    std::vector<FuncRecord> localVars;
    VarTable * currentFuncLocals = nullptr;

    // Small stack for nested for-loops:
    static constexpr int MaxNestedForLoops = 64;
    const SyntaxTreeNode * forLoopIterStack[MaxNestedForLoops];
    int forLoopIterStackTop = 0;
};

// ========================================================
// Imported file registry:
// ========================================================

class ImportTable final
{
public:
    //TODO
private:
};

// ========================================================
// Parsing and semantic check utilities:
// ========================================================

void beginTranslationUnit(ParseContext & ctx, const SyntaxTreeNode * statements);
void requireGlobalScope(ParseContext & ctx, const char * elementName);

//
// Miscellaneous:
//
SyntaxTreeNode * newLiteralNode(ParseContext & ctx, const SyntaxTreeNode::Eval stEval, const Symbol * symbol);
SyntaxTreeNode * newTypeIdNode(ParseContext & ctx, const SyntaxTreeNode::Eval stEval, const Symbol * symbol = nullptr);
SyntaxTreeNode * newIdentNode(ParseContext & ctx, const Symbol * identifierSymbol);
SyntaxTreeNode * newTypecastNode(ParseContext & ctx, const SyntaxTreeNode * lhs, const SyntaxTreeNode * rhs);
SyntaxTreeNode * newTypeofNode(ParseContext & ctx, SyntaxTreeNode * exprOrTypeNode);
SyntaxTreeNode * newStatementNode(ParseContext & ctx, const SyntaxTreeNode * left, const SyntaxTreeNode * right);
SyntaxTreeNode * newUDTNode(ParseContext & ctx, const Symbol * typeNameSymbol);
SyntaxTreeNode * handleImportDirective(ParseContext & ctx, const Symbol * importFileSymbol);

//
// Variable declaration:
//
SyntaxTreeNode * newVarDeclNode(ParseContext & ctx, const Symbol * varNameSymbol,
                                SyntaxTreeNode * initNode, const SyntaxTreeNode * typeNode,
                                SyntaxTreeNode::Eval stEval);

//
// Arrays and ranges:
//
SyntaxTreeNode * newArraySubscriptNode(ParseContext & ctx, const SyntaxTreeNode * arrayExpr,
                                       const SyntaxTreeNode * indexExpr);

SyntaxTreeNode * newArrayLiteralNode(ParseContext & ctx, const SyntaxTreeNode * arrayInitializers);
SyntaxTreeNode * newArrayTypeNode(ParseContext & ctx, const SyntaxTreeNode * arrayInitializers);
SyntaxTreeNode * newRangeParamNode(ParseContext & ctx, const Symbol * literalSymbol);

//
// Binary/unary/ifs:
//
SyntaxTreeNode * newBinaryOpNode(ParseContext & ctx, const SyntaxTreeNode::Type type,
                                 SyntaxTreeNode * lhs, SyntaxTreeNode * rhs);

SyntaxTreeNode * newUnaryOpNode(ParseContext & ctx, const SyntaxTreeNode::Type type,
                                const OpCode unaryOp, const SyntaxTreeNode * operand);

SyntaxTreeNode * newCompareNode(ParseContext & ctx, const SyntaxTreeNode::Type type, SyntaxTreeNode * child0,
                                const SyntaxTreeNode * child1, const SyntaxTreeNode * child2);

//
// Loops:
//
SyntaxTreeNode * newLoopNode(ParseContext & ctx, SyntaxTreeNode::Type nodeType, SyntaxTreeNode * loopCondNode,
                             const SyntaxTreeNode * bodyStatementListNode);

SyntaxTreeNode * newForLoopIteratorNode(ParseContext & ctx, const SyntaxTreeNode * iterableNode,
                                        const Symbol * iteratorVarSymbol);

SyntaxTreeNode * newLoopJumpNode(ParseContext & ctx, SyntaxTreeNode::Type nodeType);

//
// Function-related stuff:
//
SyntaxTreeNode * newFunctionDeclNode(ParseContext & ctx,
                                     const Symbol * funcNameSymbol,
                                     const SyntaxTreeNode * paramListNode,
                                     const SyntaxTreeNode * returnTypeNode,
                                     const SyntaxTreeNode * bodyStatementListNode);

SyntaxTreeNode * newFunctionCallNode(ParseContext & ctx, const Symbol * funcNameSymbol,
                                     SyntaxTreeNode * argListNode);

SyntaxTreeNode * newVarArgsNode(ParseContext & ctx, const Symbol * varArgsSymbol);
SyntaxTreeNode * newReturnNode(ParseContext & ctx, const SyntaxTreeNode * optExprNode);

//
// Structs, objects type-aliases:
//
SyntaxTreeNode * newStructDeclNode(ParseContext & ctx, const Symbol * structTypeSymbol,
                                   const SyntaxTreeNode * structMembersListNode);

SyntaxTreeNode * newTypeAliasNode(ParseContext & ctx, const Symbol * aliasNameSymbol,
                                  const SyntaxTreeNode * aliasedTypeIdNode);

SyntaxTreeNode * newObjConstructorCallNode(ParseContext & ctx, const SyntaxTreeNode * typeIdNode,
                                           const SyntaxTreeNode * constructorArgsNode);

//
// Enums:
//
SyntaxTreeNode * newEnumDeclNode(ParseContext & ctx,
                                 const Symbol * enumNameSymbol,
                                 SyntaxTreeNode * enumConstantsListNode);

SyntaxTreeNode * newEnumConstNode(ParseContext & ctx, const Symbol * enumConstNameSymbol,
                                  const SyntaxTreeNode * initializerNode,
                                  const SyntaxTreeNode::Eval constEval);

SyntaxTreeNode::Eval enumMemberConstantReference(ParseContext & ctx,
                                                 SyntaxTreeNode * eTypeNode,
                                                 SyntaxTreeNode * eConstNode);

// ========================================================
// Internal type conversion utilities:
// ========================================================

SyntaxTreeNode::Eval varType2Eval(const Variant::Type varType);
Variant::Type eval2VarType(const SyntaxTreeNode::Eval stEval);

const TypeId * varType2TypeId(const TypeTable & typeTable, const Variant::Type varType);
const TypeId * eval2TypeId(const TypeTable & typeTable, const SyntaxTreeNode::Eval stEval);

SyntaxTreeNode::Eval symbol2Eval(const Symbol & symbol);
const Symbol * eval2Symbol(const SymbolTable & symTable, SyntaxTreeNode::Eval stEval);

Variant symbol2Variant(VM & vm, const Symbol & sym);
Variant::Type typeId2VarType(const TypeId * tid);

// ========================================================
// Additional error/warning handlers:
// ========================================================

[[noreturn]] void parserError(ParseContext & ctx, const std::string & message) noexcept(false);
void parserWarn(ParseContext & ctx, const std::string & message); // --> Doesn't explicit throw any exceptions.

} // namespace moon {}

#endif // MOON_SEMANTIC_CHECK_HPP
