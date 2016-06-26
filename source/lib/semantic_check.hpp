
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
    const TypeId   *     typeId;     // Runtime TypeId for UDTs and native types alike.
    const Function *     funcPtr;    // If the var is a pointer-to-function, we can resolve it at compile-time.
    Variant::Type        varType;    // Final Variant type. Matches stEval.
    SyntaxTreeNode::Eval arrayEval;  // Only relevant if varType=Array. Data type stored in an array var.
    SyntaxTreeNode::Eval stEval;     // Matches varType; Cached conversion for frequent use.
    UInt16               stackIndex; // Used for local variables, InvalidStackIndex otherwise.
};

class VarInfoTable final
{
public:

    VarInfoTable() = default;
    VarInfoTable(const VarInfoTable &) = delete;
    VarInfoTable & operator = (const VarInfoTable &) = delete;

    void beginFunc(ParseContext & ctx, const Symbol * funcNameSymbol);
    void endFunc(ParseContext & ctx);

    void beginUDT(ParseContext & ctx, const Symbol * typeNameSymbol, const char * elementName);
    void endUDT(ParseContext & ctx);

    void beginForLoop(ParseContext & ctx, const Symbol * iteratorVarSymbol, const SyntaxTreeNode * iterableNode);
    void endForLoop(ParseContext & ctx);

    const VarInfo * findVar(const Symbol * symbol) const;
    const VarInfo * findGlobalVar(const Symbol * symbol) const;
    const VarInfo * findFuncLocalVar(const Symbol * symbol) const;
    const VarInfo * findNamedFuncLocalVar(const Symbol * varSymbol, const Symbol * funcSymbol);

    void addGlobalVar(const Symbol * varNameSymbol, VarInfo vi);
    void addFuncLocalVar(const Symbol * varNameSymbol, VarInfo vi);
    int  getFuncStackSize(const Symbol * funcSymbol);

    bool removeVar(const Symbol * symbol);
    bool removeGlobalVar(const Symbol * symbol);
    bool removeFuncLocalVar(const Symbol * symbol);

    bool isParsingFunction() const noexcept { return currentFuncSymbol != nullptr; }
    bool isParsingUDT()      const noexcept { return currentUDTSymbol  != nullptr; }

    // Transient states:
    const TypeId *          lastRhsTypeId       = nullptr;
    const TypeId *          lastRhsParentTypeId = nullptr;
    const Symbol *          currentFuncSymbol   = nullptr;
    const Symbol *          currentUDTSymbol    = nullptr;

    // Stuff use only inside loops/functions:
    bool                    insideLoopStmt      = false;
    bool                    foundReturnStmt     = false;
    SyntaxTreeNode::Eval    expectedReturnType  = SyntaxTreeNode::Eval::Void;
    std::uint32_t           forLoopIdCounter    = 0;
    const SyntaxTreeNode *  forLoopIteratorNode = nullptr;

private:

    // [var_name_symbol, var_info_record]
    using VarTable = HashTable<const Symbol *, VarInfo>;

    struct FuncRecord
    {
        const Symbol *      funcName;
        int                 stackSize;
        VarTable            localVars;
    };

    VarTable                globalVars;
    std::vector<FuncRecord> localVars;
    VarTable *              currentFuncLocals   = nullptr;
    int *                   currentStackSize    = nullptr;
    int                     forLoopIterStackTop = 0;
    static constexpr int    MaxNestedForLoops   = 64;
    const SyntaxTreeNode *  forLoopIterStack[MaxNestedForLoops]; // Small stack of nested for-loops
};

// ========================================================
// Imported file registry:
// ========================================================

class ImportTable final
{
public:

    ImportTable() = default;
    ImportTable(const ImportTable &) = delete;
    ImportTable & operator = (const ImportTable &) = delete;

    ~ImportTable()
    {
        for (auto import : importedFileNames)
        {
            releaseRcString(import);
        }
    }

    bool isImported(const ConstRcString * const filename) const
    {
        for (const auto import : importedFileNames)
        {
            if (cmpRcStringsEqual(import, filename))
            {
                return true;
            }
        }
        return false;
    }

    void addImport(ConstRcString * filename)
    {
        MOON_ASSERT(isRcStringValid(filename));
        importedFileNames.push_back(filename);
        addRcStringRef(filename);
    }

private:

    std::vector<ConstRcString *> importedFileNames;
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
void registerNewFunction(ParseContext & ctx,
                         const Symbol * funcNameSymbol,
                         const SyntaxTreeNode * paramListNode,
                         const SyntaxTreeNode * returnTypeNode);

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
const Symbol * mangleEnumTypeName(SymbolTable & symTable, const Symbol * enumNameSymbol, int declLineNum);
const Symbol * demangleEnumTypeName(SymbolTable & symTable, const Symbol * enumNameSymbol);

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
