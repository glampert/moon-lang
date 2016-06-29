
// ================================================================================================
// -*- C++ -*-
// File: semantic_check.cpp
// Author: Guilherme R. Lampert
// Created on: 17/04/16
// Brief: Code related to semantic and type checking as well as other parsing utilities.
// ================================================================================================

#include "semantic_check.hpp"
#include "compiler.hpp"
#include "vm.hpp"

namespace moon
{

// Slightly shorter names for local usage:
using STNode = SyntaxTreeNode::Type;
using STEval = SyntaxTreeNode::Eval;

// ========================================================
// Parse-time variable type info:
// ========================================================

void VarInfoTable::beginFunc(ParseContext & ctx, const Symbol * funcNameSymbol)
{
    if (isParsingFunction())
    {
        parserError(ctx, "nested functions are currently unsupported");
    }
    if (isParsingUDT())
    {
        parserError(ctx, "member functions are currently unsupported");
    }

    localVars.emplace_back(FuncRecord{ funcNameSymbol, 0, VarTable{} });
    currentFuncLocals = &localVars.back().localVars;
    currentStackSize  = &localVars.back().stackSize;
    currentFuncSymbol = funcNameSymbol;
}

void VarInfoTable::endFunc(ParseContext & ctx)
{
    if (expectedReturnType != STEval::Void && !foundReturnStmt)
    {
        const std::string fname = color::white() + toString(currentFuncSymbol->name) + color::restore();
        parserError(ctx, "missing return statement in non-void function '" + fname + "'");
    }

    currentFuncLocals  = nullptr;
    currentFuncSymbol  = nullptr;
    currentStackSize   = nullptr;
    foundReturnStmt    = false;
    expectedReturnType = STEval::Void;
}

void VarInfoTable::beginUDT(ParseContext & ctx, const Symbol * typeNameSymbol, const char * elementName)
{
    requireGlobalScope(ctx, elementName);
    currentUDTSymbol = typeNameSymbol;
}

void VarInfoTable::endUDT(ParseContext & /* ctx */)
{
    currentUDTSymbol = nullptr;
}

void VarInfoTable::beginForLoop(ParseContext & ctx, const Symbol * iteratorVarSymbol, const SyntaxTreeNode * iterableNode)
{
    if (forLoopIteratorNode != nullptr)
    {
        if (forLoopIterStackTop >= MaxNestedForLoops)
        {
            MOON_INTERNAL_EXCEPTION("too many nested for loops!"); // Some nasty loops :P
        }
        forLoopIterStack[forLoopIterStackTop++] = forLoopIteratorNode;
    }

    insideLoopStmt = true;
    forLoopIteratorNode = newForLoopIteratorNode(ctx, iterableNode, iteratorVarSymbol);
}

void VarInfoTable::endForLoop(ParseContext & /* ctx */)
{
    insideLoopStmt = false;
    if (forLoopIterStackTop > 0)
    {
        forLoopIteratorNode = forLoopIterStack[--forLoopIterStackTop];
    }
    else
    {
        forLoopIteratorNode = nullptr;
    }
}

const VarInfo * VarInfoTable::findVar(const Symbol * symbol) const
{
    const VarInfo * vi = nullptr;

    // It stands to reason that locals are referenced more often
    // than globals inside functions, so check the local vars first.
    if (isParsingFunction())
    {
        vi = findFuncLocalVar(symbol);
        if (vi == nullptr) // Okay, try the globals now
        {
            vi = findGlobalVar(symbol);
        }
    }
    else // Must be a global
    {
        vi = findGlobalVar(symbol);
    }

    return vi;
}

const VarInfo * VarInfoTable::findFuncLocalVar(const Symbol * symbol) const
{
    MOON_ASSERT(symbol != nullptr);
    MOON_ASSERT(currentFuncLocals != nullptr);

    auto && iter = currentFuncLocals->find(symbol);
    if (iter != currentFuncLocals->end())
    {
        return &(iter->second);
    }
    return nullptr;
}

const VarInfo * VarInfoTable::findNamedFuncLocalVar(const Symbol * varSymbol, const Symbol * funcSymbol)
{
    if (currentFuncSymbol == funcSymbol)
    {
        MOON_ASSERT(currentFuncLocals != nullptr);
        auto && iter = currentFuncLocals->find(varSymbol);
        if (iter != currentFuncLocals->end())
        {
            return &(iter->second);
        }
    }
    else
    {
        for (FuncRecord & funcRecord : localVars)
        {
            if (funcRecord.funcName == funcSymbol)
            {
                // Cache for subsequent calls.
                currentFuncSymbol = funcRecord.funcName;
                currentFuncLocals = &funcRecord.localVars;
                currentStackSize  = &funcRecord.stackSize;

                auto && iter = currentFuncLocals->find(varSymbol);
                if (iter != currentFuncLocals->end())
                {
                    return &(iter->second);
                }

                break;
            }
        }
    }

    return nullptr;
}

const VarInfo * VarInfoTable::findGlobalVar(const Symbol * symbol) const
{
    MOON_ASSERT(symbol != nullptr);

    auto && iter = globalVars.find(symbol);
    if (iter != globalVars.end())
    {
        return &(iter->second);
    }
    return nullptr;
}

void VarInfoTable::addGlobalVar(const Symbol * varNameSymbol, VarInfo vi)
{
    MOON_ASSERT(varNameSymbol != nullptr);

    vi.stackIndex = InvalidStackIndex;
    globalVars.insert(std::make_pair(varNameSymbol, vi));
}

void VarInfoTable::addFuncLocalVar(const Symbol * varNameSymbol, VarInfo vi)
{
    MOON_ASSERT(varNameSymbol     != nullptr);
    MOON_ASSERT(currentFuncLocals != nullptr);
    MOON_ASSERT(currentStackSize  != nullptr);

    // Up to 64K function parameters + local vars.
    if (*currentStackSize >= UINT16_MAX)
    {
        MOON_INTERNAL_EXCEPTION("maximum number of local variables and function parameters exceeded in '" +
                                toString(currentFuncSymbol->name) + "'");
    }

    vi.stackIndex = static_cast<UInt16>(*currentStackSize);
    (*currentStackSize)++; // Only grows. removeFuncLocalVar() does not decrement it.

    currentFuncLocals->insert(std::make_pair(varNameSymbol, vi));
}

int VarInfoTable::getFuncStackSize(const Symbol * funcSymbol)
{
    MOON_ASSERT(funcSymbol != nullptr);

    if (currentFuncSymbol == funcSymbol)
    {
        MOON_ASSERT(currentStackSize != nullptr);
        return static_cast<int>(*currentStackSize);
    }
    else
    {
        for (FuncRecord & funcRecord : localVars)
        {
            if (funcRecord.funcName == funcSymbol)
            {
                // Cache for subsequent calls.
                currentFuncSymbol = funcRecord.funcName;
                currentFuncLocals = &funcRecord.localVars;
                currentStackSize  = &funcRecord.stackSize;
                return static_cast<int>(*currentStackSize);
            }
        }
    }

    return 0;
}

bool VarInfoTable::removeVar(const Symbol * symbol)
{
    bool removed;
    if (isParsingFunction())
    {
        removed = removeFuncLocalVar(symbol);
        if (!removed) // Okay, try the globals now
        {
            removed = removeGlobalVar(symbol);
        }
    }
    else // Must be a global
    {
        removed = removeGlobalVar(symbol);
    }
    return removed;
}

bool VarInfoTable::removeGlobalVar(const Symbol * symbol)
{
    MOON_ASSERT(symbol != nullptr);
    return globalVars.erase(symbol) != 0;
}

bool VarInfoTable::removeFuncLocalVar(const Symbol * symbol)
{
    MOON_ASSERT(symbol != nullptr);
    MOON_ASSERT(currentFuncLocals != nullptr);
    return currentFuncLocals->erase(symbol) != 0;
}

// ========================================================
// Parsing and semantic check utilities:
// ========================================================

void beginTranslationUnit(ParseContext & ctx, const SyntaxTreeNode * statements)
{
    const Symbol * tuFileName = ctx.symTable->findOrDefineStrLiteral(ctx.srcFile != nullptr ? ctx.srcFile->c_str() : "???");
    SyntaxTreeNode * tuNode   = ctx.syntTree->newNodeWithSymbol(STNode::TranslationUnit, tuFileName,
                                                                statements, nullptr, nullptr, STEval::Void);

    if (ctx.importDepth == 0) // First script
    {
        ctx.syntTree->setRoot(tuNode);
        ctx.treeRoot = tuNode;
    }
    else // An import
    {
        ctx.treeRoot = tuNode;
    }
}

void requireGlobalScope(ParseContext & ctx, const char * elementName)
{
    if (ctx.varInfo->isParsingFunction() || ctx.varInfo->isParsingUDT())
    {
        parserError(ctx, toString(elementName) + " can only be declared at the global scope");
    }
}

SyntaxTreeNode * handleImportDirective(ParseContext & ctx, const Symbol * importFileSymbol)
{
    MOON_ASSERT(ctx.compiler    != nullptr);
    MOON_ASSERT(ctx.importTable != nullptr);

    if (ctx.importTable->isImported(importFileSymbol->value.asString))
    {
        // Script already imported before. Ignore this import directive.
        return ctx.syntTree->newNode(STNode::NoOp, nullptr, nullptr, nullptr, STEval::Void);
    }

    const std::string filename{ importFileSymbol->value.asString->chars };
    SyntaxTreeNode * importRoot = nullptr;

    ctx.importDepth++;
    ctx.compiler->parseScriptImport(ctx.vm, filename, &importRoot);
    ctx.importDepth--;

    ctx.importTable->addImport(importFileSymbol->value.asString);
    return importRoot; // This is the root of the newly imported script.
}

static void registerVariable(ParseContext & ctx, const Symbol * varNameSymbol,
                             const SyntaxTreeNode * initNode, const SyntaxTreeNode * typeNode,
                             const STEval stEval, void (VarInfoTable::*pAddVarMethod)(const Symbol *, VarInfo))
{
    const SyntaxTreeNode * tidNode = nullptr;

    if (typeNode != nullptr)
    {
        tidNode = typeNode;
    }
    else if (initNode != nullptr)
    {
        tidNode = initNode;
        if (tidNode->evalType == STEval::Void)
        {
            parserError(ctx, "trying to initialize variable from return value of void function");
        }
    }
    else
    {
        parserError(ctx, "variable declaration requires an initializer");
    }

    const TypeId * typeId;
    if (stEval == STEval::UDT)
    {
        MOON_ASSERT(tidNode != nullptr && tidNode->symbol != nullptr);
        typeId = ctx.vm->types.findTypeId(tidNode->symbol->name);
    }
    else
    {
        typeId = eval2TypeId(ctx.vm->types, stEval);
    }

    if (typeId == nullptr)
    {
        MOON_ASSERT(tidNode != nullptr && tidNode->symbol != nullptr);
        const std::string tname = color::white() + toString(tidNode->symbol->name) + color::restore();
        parserError(ctx, "cannot declare variable of undefined type '" + tname + "'");
    }

    const Variant::Type varType = eval2VarType(stEval);
    const STEval arrayEval = ((varType == Variant::Type::Array && initNode != nullptr) ?
                               initNode->getChild(2)->evalType : STEval::Any);

    const Function * funcPtr = nullptr;
    if (varType == Variant::Type::Function && initNode != nullptr &&
        initNode->symbol != nullptr && initNode->nodeType != STNode::ExprFuncCall)
    {
        funcPtr = ctx.vm->functions.findFunction(initNode->symbol->name);
        MOON_ASSERT(funcPtr != nullptr); // Already syntax-checked, so this suffices.
    }

    (ctx.varInfo->*pAddVarMethod)(varNameSymbol, VarInfo{ typeId, funcPtr, varType, arrayEval, stEval, InvalidStackIndex });
}

static STEval registerGlobalOrLocal(ParseContext & ctx, const Symbol * varNameSymbol, SyntaxTreeNode * initNode,
                                    const SyntaxTreeNode * typeNode, STEval stEval)
{
    if (!ctx.varInfo->isParsingFunction()) // Global scope or parsing a function declaration?
    {
        if (stEval == STEval::Undefined)
        {
            if (initNode == nullptr || initNode->symbol == nullptr)
            {
                parserError(ctx, "variable declaration requires an initializer");
            }

            // A global can be initialized from other globals
            const VarInfo * globVar = ctx.varInfo->findGlobalVar(initNode->symbol);
            if (globVar == nullptr)
            {
                const std::string srcName = color::white() + toString(varNameSymbol->name) + color::restore();
                const std::string dstName = color::white() + toString(initNode->symbol->name) + color::restore();
                parserError(ctx, "initializing global variable '" + srcName + "' from unknown identifier '" + dstName + "'");
            }

            stEval = globVar->stEval;
            initNode->evalType = stEval;
        }

        if (ctx.varInfo->findGlobalVar(varNameSymbol))
        {
            const std::string vname = color::white() + toString(varNameSymbol->name) + color::restore();
            parserError(ctx, "redefinition of global variable '" + vname + "'");
        }

        registerVariable(ctx, varNameSymbol, initNode, typeNode, stEval, &VarInfoTable::addGlobalVar);
    }
    else // Function scope
    {
        if (stEval == STEval::Undefined)
        {
            if (initNode == nullptr || initNode->symbol == nullptr)
            {
                parserError(ctx, "variable declaration requires an initializer");
            }

            // A local can be initialized from other locals or globals
            const VarInfo * globVar = ctx.varInfo->findGlobalVar(initNode->symbol);
            if (globVar == nullptr)
            {
                // Try from a previous local variable
                const VarInfo * localVar = ctx.varInfo->findFuncLocalVar(initNode->symbol);
                if (localVar == nullptr)
                {
                    const std::string srcName = color::white() + toString(varNameSymbol->name) + color::restore();
                    const std::string dstName = color::white() + toString(initNode->symbol->name) + color::restore();
                    parserError(ctx, "initializing local variable '" + srcName + "' from unknown identifier '" + dstName + "'");
                }
                stEval = localVar->stEval;
            }
            else
            {
                stEval = globVar->stEval;
            }
            initNode->evalType = stEval;
        }

        if (ctx.varInfo->findFuncLocalVar(varNameSymbol))
        {
            const std::string vname = color::white() + toString(varNameSymbol->name) + color::restore();
            parserError(ctx, "redefinition of local variable '" + vname + "'");
        }
        else if (ctx.varInfo->findGlobalVar(varNameSymbol))
        {
            const std::string vname = color::white() + toString(varNameSymbol->name) + color::restore();
            parserError(ctx, "redefinition of global variable '" + vname + "' as a local variable");
        }

        registerVariable(ctx, varNameSymbol, initNode, typeNode, stEval, &VarInfoTable::addFuncLocalVar);
    }

    return stEval;
}

SyntaxTreeNode * newVarDeclNode(ParseContext & ctx, const Symbol * varNameSymbol, SyntaxTreeNode * initNode,
                                const SyntaxTreeNode * typeNode, STEval stEval)
{
    // Member vars get collected elsewhere.
    if (!ctx.varInfo->isParsingUDT())
    {
        stEval = registerGlobalOrLocal(ctx, varNameSymbol, initNode, typeNode, stEval);
    }

    return ctx.syntTree->newNodeWithSymbol(STNode::VarDeclStatement,
                                           varNameSymbol, initNode, typeNode,
                                           nullptr, stEval);
}

SyntaxTreeNode * newLiteralNode(ParseContext & ctx, const STEval stEval, const Symbol * symbol)
{
    return ctx.syntTree->newNodeWithEval(STNode::ExprLiteralConst, symbol, stEval);
}

SyntaxTreeNode * newTypeIdNode(ParseContext & ctx, const STEval stEval, const Symbol * symbol)
{
    if (symbol == nullptr)
    {
        symbol = eval2Symbol(*ctx.symTable, stEval);
    }
    return ctx.syntTree->newNodeWithEval(STNode::ExprTypeIdent, symbol, stEval);
}

SyntaxTreeNode * newIdentNode(ParseContext & ctx, const Symbol * identifierSymbol)
{
    // If you'd like to use different names for the source-location constants, just change these.
    static constexpr auto fileHash = ct::hashCString("SRC_FILE_NAME");
    static constexpr auto lineHash = ct::hashCString("SRC_LINE_NUM");
    static constexpr auto funcHash = ct::hashCString("SRC_FUNC_NAME");

    // Check for the source location built-ins that translate directly to a constant:
    if (identifierSymbol->cmpEqual(fileHash))
    {
        const char * srcFileName = ((ctx.srcFile != nullptr) ?
                                     ctx.srcFile->c_str() :
                                     "<unknown file>");
        return ctx.syntTree->newNodeWithEval(STNode::ExprLiteralConst,
                                             ctx.symTable->findOrDefineStrLiteral(srcFileName),
                                             STEval::Str);
    }
    else if (identifierSymbol->cmpEqual(lineHash))
    {
        const int srcLineNum = ctx.lexer->lineno();
        return ctx.syntTree->newNodeWithEval(STNode::ExprLiteralConst,
                                             ctx.symTable->findOrDefineIntLiteral(srcLineNum),
                                             STEval::Long);
    }
    else if (identifierSymbol->cmpEqual(funcHash))
    {
        const char * funcName = ((ctx.varInfo->currentFuncSymbol != nullptr)  ?
                                  ctx.varInfo->currentFuncSymbol->name->chars :
                                  "<global scope>");
        return ctx.syntTree->newNodeWithEval(STNode::ExprLiteralConst,
                                             ctx.symTable->findOrDefineStrLiteral(funcName),
                                             STEval::Str);
    }

    // A user-defined identifier:
    SyntaxTreeNode * identNode = ctx.syntTree->newNodeWithSymbol(STNode::ExprNameIdent, identifierSymbol);
    if (!ctx.varInfo->isParsingUDT())
    {
        // Try figuring out the type of this identifier reference
        const VarInfo * vi = ctx.varInfo->findVar(identifierSymbol);
        if (vi != nullptr)
        {
            identNode->evalType = vi->stEval;

            // VarArgs get converted to array on runtime, so we need to pretend references
            // to a VarArgs are actually references to an untyped array (array of Any).
            if (identNode->evalType == STEval::VarArgs)
            {
                identNode->evalType = STEval::Array;
            }
        }
        else
        {
            // Could be a function (in a 'var = function' kind of expression)
            if (ctx.vm->functions.findFunction(identifierSymbol->name))
            {
                identNode->evalType = STEval::Func;
            }
        }
        // Leave undefined. Will trigger an error later.
    }
    return identNode;
}

SyntaxTreeNode * newArraySubscriptNode(ParseContext & ctx, const SyntaxTreeNode * arrayExpr,
                                       const SyntaxTreeNode * indexExpr)
{
    // Multi-dimensional array subscripting or reference to member array
    if (arrayExpr->nodeType == STNode::ExprArraySubscript ||
        arrayExpr->nodeType == STNode::ExprMemberRef)
    {
        // NOTE that we hardcode eval to 'Any'. See the comment below
        // on the ExprFuncCall case for the explanation. This is a current
        // limitation of our implementation and should be improved in the future.
        return ctx.syntTree->newNode(STNode::ExprArraySubscript, nullptr, arrayExpr, indexExpr, STEval::Any);
    }

    // 1D array subscripting, possibly from the return value of a function:
    MOON_ASSERT(arrayExpr->symbol != nullptr);
    if (arrayExpr->evalType != STEval::Array)
    {
        const std::string name = color::white() + toString(arrayExpr->symbol->name) + color::restore();
        if (arrayExpr->nodeType == STNode::ExprFuncCall)
        {
            parserError(ctx, "function '" + name + "' does not return an array");
        }
        else
        {
            parserError(ctx, "'" + name + "' is not an array ");
        }
    }

    if (indexExpr->evalType != STEval::Int &&
        indexExpr->evalType != STEval::Long)
    {
        parserError(ctx, "array subscript index must be an integer type");
    }

    STEval subscriptEval;
    if (arrayExpr->nodeType == STNode::ExprFuncCall) // Function that returns Array?
    {
        // NOTE:
        // No simple way of figuring out the underlaying type of the
        // array returned by a function/member-ref/multi-dim-array, so
        // in this case we pretend the array type is 'Any' and delegate
        // type-checking to runtime. If there's a type mismatch, it won't
        // show up during compilation, but it will trigger a runtime error.
        // This should be fixed in a future, more robust, implementation.
        subscriptEval = STEval::Any;
    }
    else
    {
        const VarInfo * vi = ctx.varInfo->findVar(arrayExpr->symbol);
        if (vi == nullptr)
        {
            const std::string name = color::white() + toString(arrayExpr->symbol->name) + color::restore();
            parserError(ctx, "reference to undefined variable '" + name + "'");
        }
        MOON_ASSERT(vi->arrayEval != STEval::Undefined);
        MOON_ASSERT(vi->varType == Variant::Type::Array);
        subscriptEval = vi->arrayEval;
    }

    return ctx.syntTree->newNode(STNode::ExprArraySubscript, nullptr, arrayExpr, indexExpr, subscriptEval);
}

SyntaxTreeNode * newArrayLiteralNode(ParseContext & ctx, const SyntaxTreeNode * arrayInitializers)
{
    // First child reserved for parameter_list chains
    // Third child has the type of the array storage
    const SyntaxTreeNode * arrayTypeNode = newArrayTypeNode(ctx, arrayInitializers);
    return ctx.syntTree->newNode(STNode::ExprArrayLiteral, nullptr,
                                 arrayInitializers, arrayTypeNode,
                                 STEval::Array);
}

SyntaxTreeNode * newArrayTypeNode(ParseContext & ctx, const SyntaxTreeNode * arrayInitializers)
{
    // We can't allow empty [] array initializers because our arrays are not polymorphic.
    // We need at least one initializer to dictate the underlaying type.
    if (arrayInitializers == nullptr)
    {
        parserError(ctx, "empty array literals are not allowed");
    }

    const STEval firstType = arrayInitializers->evalType;

    // All following array elements must match the first type.
    for (auto node = arrayInitializers->getChild(0); node != nullptr; node = node->getChild(0))
    {
        if (node->evalType != firstType)
        {
            parserError(ctx, "array initializers must all be of the same type");
        }
    }

    const Symbol * typeSymbol = ctx.symTable->findSymbol(eval2TypeId(ctx.vm->types, firstType)->name);
    return ctx.syntTree->newNodeWithEval(STNode::ExprTypeIdent, typeSymbol, firstType);
}

SyntaxTreeNode * newRangeParamNode(ParseContext & ctx, const Symbol * literalSymbol)
{
    if (literalSymbol->type == Symbol::Type::StrLiteral) // One-char string literals are allowed
    {
        if (literalSymbol->value.asString->length != 1)
        {
            parserError(ctx, "only single character literals are allowed in range expressions");
        }

        // Convert to its ASCII value.
        const int c = literalSymbol->value.asString->chars[0];
        literalSymbol = ctx.symTable->findOrDefineIntLiteral(c);
    }

    return ctx.syntTree->newNodeWithEval(STNode::ExprLiteralConst, literalSymbol,
                                         symbol2Eval(*literalSymbol));
}

static Variant::Type binaryOpResultType(const OpCode op, const Variant::Type typeA, const Variant::Type typeB)
{
    // If one term is Float or Any, we have an implicit conversion.
    auto typeConv = [](const Variant::Type lhs, const Variant::Type rhs) -> Variant::Type
    {
        if (lhs == Variant::Type::Float ||
            rhs == Variant::Type::Float)
        {
            return Variant::Type::Float;
        }
        if (lhs == Variant::Type::Any ||
            rhs == Variant::Type::Any)
        {
            return Variant::Type::Any;
        }
        MOON_ASSERT(lhs == rhs);
        return lhs;
    };

    switch (op)
    {
    // Boolean (integral) result:
    case OpCode::CmpNotEqual     : return Variant::Type::Integer;
    case OpCode::CmpEqual        : return Variant::Type::Integer;
    case OpCode::CmpGreaterEqual : return Variant::Type::Integer;
    case OpCode::CmpGreater      : return Variant::Type::Integer;
    case OpCode::CmpLessEqual    : return Variant::Type::Integer;
    case OpCode::CmpLess         : return Variant::Type::Integer;
    case OpCode::LogicOr         : return Variant::Type::Integer;
    case OpCode::LogicAnd        : return Variant::Type::Integer;
    // Preserves one of the input types:
    case OpCode::Sub             : return typeConv(typeA, typeB);
    case OpCode::Add             : return typeConv(typeA, typeB);
    case OpCode::Mod             : return typeConv(typeA, typeB);
    case OpCode::Div             : return typeConv(typeA, typeB);
    case OpCode::Mul             : return typeConv(typeA, typeB);
    default : MOON_UNREACHABLE();
    } // switch (op)
}

static Variant::Type unaryOpResultType(const OpCode op, const Variant::Type type)
{
    switch (op)
    {
    case OpCode::Plus     : return type;
    case OpCode::Negate   : return type;
    case OpCode::LogicNot : return Variant::Type::Integer; // boolean result
    default : MOON_UNREACHABLE();
    } // switch (op)
}

SyntaxTreeNode * newBinaryOpNode(ParseContext & ctx, const STNode type,
                                 SyntaxTreeNode * lhs, SyntaxTreeNode * rhs)
{
    OpCode binOp;
    Variant::Type resultType;

    switch (type)
    {
    case STNode::ExprAddAssign       : binOp = OpCode::Add;             break;
    case STNode::ExprSubAssign       : binOp = OpCode::Sub;             break;
    case STNode::ExprDivAssign       : binOp = OpCode::Div;             break;
    case STNode::ExprModAssign       : binOp = OpCode::Mod;             break;
    case STNode::ExprMulAssign       : binOp = OpCode::Mul;             break;
    case STNode::ExprAdd             : binOp = OpCode::Add;             break;
    case STNode::ExprSubtract        : binOp = OpCode::Sub;             break;
    case STNode::ExprDivide          : binOp = OpCode::Div;             break;
    case STNode::ExprModulo          : binOp = OpCode::Mod;             break;
    case STNode::ExprMultiply        : binOp = OpCode::Mul;             break;
    case STNode::ExprLogicOr         : binOp = OpCode::LogicOr;         break;
    case STNode::ExprLogicAnd        : binOp = OpCode::LogicAnd;        break;
    case STNode::ExprCmpNotEqual     : binOp = OpCode::CmpNotEqual;     break;
    case STNode::ExprCmpEqual        : binOp = OpCode::CmpEqual;        break;
    case STNode::ExprCmpGreaterEqual : binOp = OpCode::CmpGreaterEqual; break;
    case STNode::ExprCmpGreaterThan  : binOp = OpCode::CmpGreater;      break;
    case STNode::ExprCmpLessEqual    : binOp = OpCode::CmpLessEqual;    break;
    case STNode::ExprCmpLessThan     : binOp = OpCode::CmpLess;         break;
    case STNode::ExprAssign          : binOp = OpCode::StoreGlobal;     break;
    case STNode::ExprMemberRef       : binOp = OpCode::MemberRef;       break;
    default : MOON_INTERNAL_EXCEPTION("unhandled STNode type " + toString(type) + " in binary op!");
    } // switch (type)

    if (binOp == OpCode::MemberRef) // Validate the object and member types
    {
        const TypeId * lhsTypeId = nullptr;
        if (lhs->nodeType != STNode::ExprMemberRef)
        {
            MOON_ASSERT(lhs->symbol != nullptr);
            const VarInfo * vi = ctx.varInfo->findVar(lhs->symbol);
            if (vi == nullptr)
            {
                const std::string oname = color::white() + toString(lhs->symbol->name) + color::restore();
                parserError(ctx, "reference to member of undefined object '" + oname + "'");
            }
            else
            {
                lhsTypeId = vi->typeId;
            }
            ctx.varInfo->lastRhsParentTypeId = lhsTypeId;
        }
        else // Going down a recursive MemberRef chain
        {
            lhsTypeId = ctx.varInfo->lastRhsTypeId;
            MOON_ASSERT(lhsTypeId != nullptr);
        }

        // With the type of the left-hand-side object we can figure
        // out the type of the right-hand-side member reference:
        MOON_ASSERT(rhs->symbol != nullptr);
        const auto memberName = rhs->symbol->name;
        const Variant member  = ((lhsTypeId->templateObject != nullptr) ?
                                  lhsTypeId->templateObject->findMemberVar(memberName) :
                                  Variant{});

        if (member.isNull())
        {
            const std::string tname = color::white() + toString(lhsTypeId->name) + color::restore();
            const std::string mname = color::white() + toString(memberName) + color::restore();
            parserError(ctx, "Type " + tname + " has no member named '" + mname + "'");
        }

        // Fix the member node eval if needed:
        if (rhs->evalType == STEval::Undefined)
        {
            rhs->evalType = varType2Eval(member.type);
        }

        if (member.type == Variant::Type::Object)
        {
            MOON_ASSERT(member.getAsObject() != nullptr);
            ctx.varInfo->lastRhsTypeId = member.getAsObject()->getTypeId();
        }
        else
        {
            ctx.varInfo->lastRhsTypeId = varType2TypeId(ctx.vm->types, member.type);
        }

        resultType = member.type;
    }
    else
    {
        if (lhs->evalType == STEval::Undefined)
        {
            const std::string name = color::white() + toString(lhs->symbol->name) + color::restore();
            parserError(ctx, "undefined identifier '" + name + "' on left-hand-side of expression");
        }
        if (rhs->evalType == STEval::Undefined)
        {
            const std::string name = color::white() + toString(rhs->symbol->name) + color::restore();
            parserError(ctx, "undefined identifier '" + name + "' on right-hand-side of expression");
        }

        if (lhs->evalType == STEval::Void || rhs->evalType == STEval::Void)
        {
            if (binOp == OpCode::StoreGlobal)
            {
                parserError(ctx, "void function cannot be used on assignment");
            }
            else
            {
                parserError(ctx, "void function cannot be used with operator " + binaryOpToString(binOp));
            }
        }

        const Variant::Type lhsVarType = eval2VarType(lhs->evalType);
        const Variant::Type rhsVarType = eval2VarType(rhs->evalType);

        if (binOp == OpCode::StoreGlobal) // Generic assignment tag
        {
            if (ctx.varInfo->lastRhsParentTypeId != nullptr && ctx.varInfo->lastRhsParentTypeId->templateObject != nullptr)
            {
                if (ctx.varInfo->lastRhsParentTypeId->templateObject->isEnumType)
                {
                    parserError(ctx, "assigning to enum constant");
                }
            }

            if (!isAssignmentValid(lhsVarType, rhsVarType))
            {
                parserError(ctx, "cannot assign " + toString(rhsVarType) + " to " + toString(lhsVarType));
            }

            resultType = lhsVarType; // Assignment doesn't change the left-hand-side type.
        }
        else // Comparison, sum, division, etc...
        {
            if (!isBinaryOpValid(binOp, lhsVarType, rhsVarType))
            {
                parserError(ctx, "cannot apply operator " + binaryOpToString(binOp) + " on " +
                            toString(lhsVarType) + " and " + toString(rhsVarType));
            }
            resultType = binaryOpResultType(binOp, lhsVarType, rhsVarType);
        }
    }

    // child 0 = reserved for parameter_list
    return ctx.syntTree->newNode(type, nullptr, lhs, rhs, varType2Eval(resultType));
}

SyntaxTreeNode * newUnaryOpNode(ParseContext & ctx, const STNode type,
                                const OpCode unaryOp, const SyntaxTreeNode * operand)
{
    if (operand->evalType == STEval::Undefined)
    {
        const std::string name = color::white() + toString(operand->symbol->name) + color::restore();
        parserError(ctx, "undefined identifier '" + name + "' on unary expression");
    }

    const Variant::Type varType = eval2VarType(operand->evalType);
    if (!isUnaryOpValid(unaryOp, varType))
    {
        parserError(ctx, "cannot apply unary operator " + unaryOpToString(unaryOp) +
                         " on type " + toString(varType));
    }

    const Variant::Type resultType = unaryOpResultType(unaryOp, varType);
    // child 0 = reserved for parameter_list
    return ctx.syntTree->newNode(type, nullptr, operand, nullptr, varType2Eval(resultType));
}

SyntaxTreeNode * newCompareNode(ParseContext & ctx, const STNode type, SyntaxTreeNode * child0,
                                const SyntaxTreeNode * child1, const SyntaxTreeNode * child2)
{
    // Special 'if X then' case:
    if (child0->nodeType == STNode::ExprNameIdent)
    {
        // Turn it into a compare against zero/null/false expression:
        SyntaxTreeNode * zeroNode = newLiteralNode(ctx, STEval::Long, ctx.symTable->findOrDefineIntLiteral(0));
        SyntaxTreeNode * cmpNode  = newBinaryOpNode(ctx, STNode::ExprCmpNotEqual, child0, zeroNode);
        return ctx.syntTree->newNode(type, cmpNode, child1, child2, STEval::Void);
    }

    // Normal 'if X comp Y then' case:
    return ctx.syntTree->newNode(type, child0, child1, child2, STEval::Void);
}

SyntaxTreeNode * newTypecastNode(ParseContext & ctx, const SyntaxTreeNode * lhs, const SyntaxTreeNode * rhs)
{
    // It is a syntax error to pass anything else, so an assert suffices.
    MOON_ASSERT(rhs->nodeType == STNode::ExprTypeIdent);

    // Left-hand-side validation:
    if (lhs->evalType == STEval::Undefined)
    {
        const std::string name = color::white() + toString(lhs->symbol->name) + color::restore();
        parserError(ctx, "referencing undefined identifier '" + name + "' in cast expression");
    }
    else if (lhs->nodeType != STNode::ExprFuncCall &&
             ctx.vm->functions.findFunction(lhs->symbol->name))
    {
        parserError(ctx, "referencing function name on left-hand side of cast "
                         "expression. Did you mean to call it instead?");
    }
    else if (ctx.vm->types.findTypeId(lhs->symbol->name))
    {
        parserError(ctx, "referencing type name in the wrong side of a cast expression");
    }

    // See if the typecast is possible:
    // We can pretend this is an assignment, so no need to define a new typecast operator for now.
    // Right-hand-side type is the target of our fake assignment.
    // Any => T casts are always allowed and only checked at runtime.
    if (lhs->evalType != STEval::Any && !isAssignmentValid(eval2VarType(rhs->evalType), eval2VarType(lhs->evalType)))
    {
        const std::string srcType = color::white() + toString(eval2TypeId(ctx.vm->types, lhs->evalType)->name) + color::restore();
        const std::string dstType = color::white() + toString(eval2TypeId(ctx.vm->types, rhs->evalType)->name) + color::restore();
        parserError(ctx, "cannot cast from " + srcType + " to " + dstType);
    }

    // Optional superfluous cast warning:
    if (lhs->evalType == rhs->evalType)
    {
        const std::string tname = color::white() + toString(eval2TypeId(ctx.vm->types, lhs->evalType)->name) + color::restore();
        parserWarn(ctx, "unnecessary typecast from " + tname + " to " + tname);
    }

    // child 0 = reserved for parameter_list
    return ctx.syntTree->newNode(STNode::ExprTypecast, nullptr, lhs, rhs, rhs->evalType);
}

SyntaxTreeNode * newTypeofNode(ParseContext & ctx, SyntaxTreeNode * exprOrTypeNode)
{
    STEval stEval = exprOrTypeNode->evalType;
    if (stEval == STEval::Undefined)
    {
        // Could be a function name of a UDT name.
        MOON_ASSERT(exprOrTypeNode->symbol != nullptr);
        const auto name = exprOrTypeNode->symbol->name;

        if (ctx.vm->functions.findFunction(name))
        {
            stEval = STEval::Func;
        }
        else if (ctx.vm->types.findTypeId(name))
        {
            stEval = STEval::UDT;
        }
        else
        {
            parserError(ctx, "undefined type or expression as type_of() argument");
        }
        exprOrTypeNode->evalType = stEval; // Fix the node.
    }

    // child 0 = reserved for parameter_list
    return ctx.syntTree->newNode(STNode::ExprTypeof, nullptr, exprOrTypeNode, nullptr, STEval::Tid);
}

SyntaxTreeNode * newForLoopIteratorNode(ParseContext & ctx, const SyntaxTreeNode * iterableNode,
                                        const Symbol * iteratorVarSymbol)
{
    STEval stEval;
    const STEval evalType = iterableNode->evalType;
    const STNode nodeType = iterableNode->nodeType;

    if (evalType == STEval::Range)
    {
        // Ranges are just a pair of integer numbers.
        stEval = STEval::Long;
    }
    else if (evalType == STEval::Array)
    {
        if (nodeType == STNode::ExprArrayLiteral)
        {
            // Second child is a dummy node that caries the underlaying array type.
            stEval = iterableNode->getChild(2)->evalType;
        }
        else if (nodeType == STNode::ExprArraySubscript || nodeType == STNode::ExprFuncCall)
        {
            // Array of arrays or function returning array.
            // We won't backtrack to the declaration to find out the exact type stored
            // in the array, so the var will be 'Any' and we will rely on runtime checking.
            stEval = STEval::Any;
        }
        else // Variable of type array:
        {
            // Lookup the underlaying data type stored in this array:
            const VarInfo * vi = ctx.varInfo->findVar(iterableNode->symbol);
            MOON_ASSERT(vi != nullptr);
            stEval = vi->arrayEval;
        }
    }
    else // Error:
    {
        const std::string errorStr = ((evalType == STEval::Undefined) ? "is undefined" : "is not an iterable type");
        if (iterableNode->symbol != nullptr)
        {
            const std::string name = color::white() + toString(iterableNode->symbol->name) + color::restore();
            parserError(ctx, "for loop argument '" + name + "' " + errorStr);
        }
        else
        {
            parserError(ctx, "for loop argument " + errorStr);
        }
    }

    MOON_ASSERT(ctx.lexer   != nullptr);
    MOON_ASSERT(ctx.varInfo != nullptr);

    // These variables are implicitly declared inside every for loop to serve as auxiliary loop counters.
    char tempName1[512];
    char tempName2[512];
    std::snprintf(tempName1, arrayLength(tempName1), "__for_idx_%i_%u__", ctx.lexer->lineno(), ctx.varInfo->forLoopIdCounter++);
    std::snprintf(tempName2, arrayLength(tempName2), "__for_prm_%i_%u__", ctx.lexer->lineno(), ctx.varInfo->forLoopIdCounter++);

    const Symbol * internalCounterSymbol = ctx.symTable->findOrDefineIdentifier(tempName1, ctx.lexer->lineno());
    const Symbol * internalParamSymbol   = ctx.symTable->findOrDefineIdentifier(tempName2, ctx.lexer->lineno());

    const SyntaxTreeNode * iteratorTypeNode = newTypeIdNode(ctx, stEval);
    SyntaxTreeNode * iteratorNode = newVarDeclNode(ctx, iteratorVarSymbol, nullptr,
                                                   iteratorTypeNode, stEval);

    const SyntaxTreeNode * internalCounterTypeNode = newTypeIdNode(ctx, STEval::Long);
    SyntaxTreeNode * internalCounterNode = newVarDeclNode(ctx, internalCounterSymbol, nullptr,
                                                          internalCounterTypeNode, STEval::Long);

    const SyntaxTreeNode * internalParamTypeNode = newTypeIdNode(ctx, evalType);
    SyntaxTreeNode * internalParamNode = newVarDeclNode(ctx, internalParamSymbol, nullptr,
                                                        internalParamTypeNode, evalType);

    iteratorNode->setChild(2, internalCounterNode);
    internalCounterNode->setChild(2, internalParamNode);
    return iteratorNode;
}

SyntaxTreeNode * newLoopNode(ParseContext & ctx, const STNode nodeType, SyntaxTreeNode * loopCondNode,
                             const SyntaxTreeNode * bodyStatementListNode)
{
    if (nodeType == STNode::LoopStatement) // 'loop'
    {
        return ctx.syntTree->newNode(STNode::LoopStatement, bodyStatementListNode,
                                     nullptr, nullptr, STEval::Void);
    }
    else if (nodeType == STNode::WhileStatement) // 'while cond do'
    {
        return newCompareNode(ctx, STNode::WhileStatement, loopCondNode,
                              bodyStatementListNode, nullptr);
    }
    else if (nodeType == STNode::ForStatement) // 'for i in range do'
    {
        // Once the for loop is complete we must delete the iterator name
        // to prevent a redefinition error on subsequent loops.
        MOON_ASSERT(ctx.varInfo->forLoopIteratorNode != nullptr);
        ctx.varInfo->removeVar(ctx.varInfo->forLoopIteratorNode->symbol);
        return ctx.syntTree->newNodeWithSymbol(STNode::ForStatement, nullptr,
                                               ctx.varInfo->forLoopIteratorNode,
                                               loopCondNode, bodyStatementListNode,
                                               STEval::Void);
    }
    else
    {
        MOON_INTERNAL_EXCEPTION("bad loop node type!");
    }
}

SyntaxTreeNode * newLoopJumpNode(ParseContext & ctx, const STNode nodeType)
{
    if (!ctx.varInfo->insideLoopStmt)
    {
        if (nodeType == STNode::ContinueStatement)
        {
            parserError(ctx, "continue statement found outside of a loop");
        }
        else
        {
            parserError(ctx, "break statement found outside of a loop");
        }
    }
    return ctx.syntTree->newNode(nodeType, nullptr, nullptr, nullptr, STEval::Void);
}

static bool checkUnusedExprResult(ParseContext & ctx, const SyntaxTreeNode * root)
{
    const STNode type = root->nodeType;

    switch (type)
    {
    case STNode::ExprNameIdent :
    case STNode::ExprLiteralConst :
    case STNode::ExprRange :
    case STNode::ExprArrayLiteral :
    case STNode::ExprArraySubscript :
    case STNode::ExprMemberRef :
    case STNode::ExprObjectConstructor :
    case STNode::ExprTypecast :
    case STNode::ExprTypeof :
    case STNode::ExprCmpNotEqual :
    case STNode::ExprCmpEqual :
    case STNode::ExprCmpGreaterEqual :
    case STNode::ExprCmpGreaterThan :
    case STNode::ExprCmpLessEqual :
    case STNode::ExprCmpLessThan :
    case STNode::ExprLogicOr :
    case STNode::ExprLogicAnd :
    case STNode::ExprLogicNot :
    case STNode::ExprSubtract :
    case STNode::ExprAdd :
    case STNode::ExprModulo :
    case STNode::ExprDivide :
    case STNode::ExprMultiply :
    case STNode::ExprUnaryMinus :
    case STNode::ExprUnaryPlus :
        parserWarn(ctx, "result of expression is not used. Dead code will be removed");
        return true;

    default :
        return false;
    } // switch (type)
}

SyntaxTreeNode * newStatementNode(ParseContext & ctx, const SyntaxTreeNode * left, const SyntaxTreeNode * right)
{
    if (right != nullptr)
    {
        const bool isUnused = checkUnusedExprResult(ctx, right);
        if (isUnused)
        {
            // Just ignore this subtree, since it has no side-effects, no code should be generated.
            right = ctx.syntTree->newNode(STNode::NoOp, nullptr, nullptr, nullptr, STEval::Void);
        }
    }

    return ctx.syntTree->newNode(STNode::Statement, left, right, nullptr, STEval::Void);
}

static const Symbol * isEnumType(ParseContext & ctx, const Symbol * typeNameSymbol)
{
    const Symbol * mangleNameSymbol = mangleEnumTypeName(*ctx.symTable, typeNameSymbol, ctx.lexer->lineno());
    if (!ctx.vm->types.findTypeId(mangleNameSymbol->name))
    {
        return nullptr;
    }
    return mangleNameSymbol;
}

SyntaxTreeNode * newUDTNode(ParseContext & ctx, const Symbol * typeNameSymbol)
{
    if (!ctx.vm->types.findTypeId(typeNameSymbol->name))
    {
        const std::string tname = color::white() + toString(typeNameSymbol->name) + color::restore();
        if (ctx.vm->functions.findFunction(typeNameSymbol->name))
        {
            parserError(ctx, "expected type identifier, not function '" + tname + "'");
        }
        else if (const Symbol * mangleNameSymbol = isEnumType(ctx, typeNameSymbol)) // Possible enum name?
        {
            typeNameSymbol = mangleNameSymbol;
        }
        else
        {
            parserError(ctx, "referencing undefined type identifier '" + tname + "'");
        }
    }

    return newTypeIdNode(ctx, STEval::UDT, typeNameSymbol);
}

static void collectFunctionCallArgs(ParseContext & ctx, const SyntaxTreeNode * root,
                                    Variant * argVarsOut, UInt32 & argCountOut)
{
    if (root == nullptr)
    {
        return;
    }
    if (argCountOut == Function::MaxArguments)
    {
        parserError(ctx, "max function argument count (" + toString(Function::MaxArguments) + ") exceeded!");
    }

    const STEval stEval = root->evalType;
    if (stEval == STEval::Undefined)
    {
        parserError(ctx, "passing undefined variable as argument " + toString(argCountOut + 1) + " of function call");
    }

    argVarsOut[argCountOut++] = Variant{ eval2VarType(stEval) };
    collectFunctionCallArgs(ctx, root->getChild(0), argVarsOut, argCountOut);
}

SyntaxTreeNode * newFunctionCallNode(ParseContext & ctx, const Symbol * funcNameSymbol, SyntaxTreeNode * argListNode)
{
    MOON_ASSERT(funcNameSymbol != nullptr);

    ConstRcString * const funcNameStr = funcNameSymbol->name;
    const Function * func = ctx.vm->functions.findFunction(funcNameStr);

    if (func == nullptr)
    {
        // Could be attempting to indirectly call via a function pointer
        const VarInfo * vi = ctx.varInfo->findVar(funcNameSymbol);
        if (vi != nullptr)
        {
            if (vi->stEval != STEval::Func)
            {
                const std::string fname = color::white() + toString(funcNameStr) + color::restore();
                parserError(ctx, "'" + fname + "' is not a function!");
            }

            func = vi->funcPtr;
            if (func == nullptr)
            {
                // Delegate all error checking to the runtime.
                // This could be an attempt to call an uninitialized function pointer
                // but it might also be a reference we can't backtrack to its initialization
                // (a func parameter, obj member, any var, etc).
                return ctx.syntTree->newNodeWithSymbol(STNode::ExprFuncCall, funcNameSymbol,
                                                       nullptr, argListNode, nullptr, STEval::Any);
            }
        }
        else
        {
            bool inMemberRef = false;

            // Hack to guess if this was an attempt at calling a function like
            // a member method. We have a custom error message for that.
            if (ctx.currText != nullptr && !ctx.currText->empty())
            {
                // Check if the name was preceded by a '.'
                const auto index = ctx.currText->rfind(funcNameStr->chars);
                if (index != std::string::npos && index != 0)
                {
                    if (ctx.currText->at(index - 1) == '.')
                    {
                        inMemberRef = true;
                    }
                }
            }

            if (inMemberRef)
            {
                parserError(ctx, "object member methods are not supported. Assign member function variable to a temp before calling it.");
            }
            else
            {
                const std::string fname = color::white() + toString(funcNameStr) + color::restore();
                parserError(ctx, "attempting to call undefined function '" + fname + "'");
            }
        }
    }

    // Use the return type of the function for this node's eval type.
    const STEval stEval = func->hasReturnVal() ? varType2Eval(*func->returnType) : STEval::Void;

    // Gather the args passed in (we only care about the types now):
    UInt32 argCount = 0;
    Variant argVars[Function::MaxArguments];
    collectFunctionCallArgs(ctx, argListNode, argVars, argCount);

    // Check them against the expected. We reuse the runtime stack code for that.
    std::string errorMessage;
    const Stack::Slice args{ argVars, static_cast<int>(argCount) };
    if (!func->validateArguments(args, errorMessage))
    {
        parserError(ctx, errorMessage);
    }

    // Check if function call should be stripped on debug builds (becomes a no-op).
    if (!ctx.debugMode && func->isDebugOnly())
    {
        return ctx.syntTree->newNode(STNode::NoOp, nullptr, nullptr, nullptr, STEval::Void);
    }

    // Functions like assert() and panic() prepend source location to the call.
    if (func->hasCallerInfo())
    {
        const int srcLineNum = ctx.lexer->lineno();

        const char * srcFileName = ((ctx.srcFile != nullptr) ?
                                     ctx.srcFile->c_str() :
                                     "<unknown file>");

        const char * callerName = ((ctx.varInfo->currentFuncSymbol != nullptr)  ?
                                    ctx.varInfo->currentFuncSymbol->name->chars :
                                    "<global scope>");

        auto fileNode = ctx.syntTree->newNodeWithSymbol(STNode::ExprLiteralConst, ctx.symTable->findOrDefineStrLiteral(srcFileName));
        auto lineNode = ctx.syntTree->newNodeWithSymbol(STNode::ExprLiteralConst, ctx.symTable->findOrDefineIntLiteral(srcLineNum));
        auto funcNode = ctx.syntTree->newNodeWithSymbol(STNode::ExprLiteralConst, ctx.symTable->findOrDefineStrLiteral(callerName));

        fileNode->evalType = STEval::Str;
        lineNode->evalType = STEval::Long;
        funcNode->evalType = STEval::Str;

        fileNode->setChild(0, lineNode);
        lineNode->setChild(0, funcNode);
        funcNode->setChild(0, argListNode);

        // file, line and function will precede the argument list.
        argListNode = fileNode;
    }

    // First (0) child is reserved for chaining the call itself to another parameter_list.
    // Second (1) child is the parameter_list chain.
    return ctx.syntTree->newNodeWithSymbol(STNode::ExprFuncCall, funcNameSymbol, nullptr, argListNode, nullptr, stEval);
}

static void collectFunctionArgTypes(ParseContext & ctx, const SyntaxTreeNode * root,
                                    Variant::Type * argTypesOut, UInt32 & argCountOut,
                                    UInt32 & extraFlagsOut)
{
    if (root == nullptr)
    {
        return;
    }
    if (argCountOut == Function::MaxArguments)
    {
        parserError(ctx, "max function argument count (" + toString(Function::MaxArguments) + ") exceeded!");
    }

    const SyntaxTreeNode * argNode = root->getChild(1);
    const STEval stEval = argNode->evalType;

    if (stEval == STEval::VarArgs)
    {
        extraFlagsOut |= Function::VarArgs;
        return;
    }

    argTypesOut[argCountOut++] = eval2VarType(stEval);
    collectFunctionArgTypes(ctx, root->getChild(0), argTypesOut, argCountOut, extraFlagsOut);
}

void registerNewFunction(ParseContext & ctx,
                         const Symbol * funcNameSymbol,
                         const SyntaxTreeNode * paramListNode,
                         const SyntaxTreeNode * returnTypeNode)
{
    // Redefinition not allowed. We also don't currently support function overloads.
    const auto funcName = funcNameSymbol->name;
    if (ctx.vm->functions.findFunction(funcName))
    {
        const std::string fname = color::white() + toString(funcName) + color::restore();
        parserError(ctx, "function '" + fname + "' was defined more than once");
    }

    UInt32 extraFlags = 0;
    UInt32 argCount   = 0;
    Variant::Type argTypes[Function::MaxArguments];

    // nullptr indicates no return type (void).
    const Variant::Type * pRetType = nullptr;
    Variant::Type retTypeTemp;

    if (returnTypeNode->evalType != STEval::Void)
    {
        retTypeTemp = eval2VarType(returnTypeNode->evalType);
        pRetType = &retTypeTemp;
    }

    collectFunctionArgTypes(ctx, paramListNode, argTypes, argCount, extraFlags);
    if (extraFlags & Function::VarArgs)
    {
        argCount = 0;
    }

    // Register the new function with the runtime:
    ctx.vm->functions.addFunction(funcName, pRetType,
                                  (argCount != 0 ? argTypes : nullptr),
                                  argCount, 0, extraFlags, nullptr);

    ctx.varInfo->expectedReturnType = returnTypeNode->evalType;
}

SyntaxTreeNode * newFunctionDeclNode(ParseContext & ctx,
                                     const Symbol * funcNameSymbol,
                                     const SyntaxTreeNode * paramListNode,
                                     const SyntaxTreeNode * returnTypeNode,
                                     const SyntaxTreeNode * bodyStatementListNode)
{
    // child[0] = parameter list
    // child[1] = function body
    // child[2] = return type
    return ctx.syntTree->newNodeWithSymbol(STNode::FuncDeclStatement, funcNameSymbol,
                                           paramListNode, bodyStatementListNode,
                                           returnTypeNode, returnTypeNode->evalType);
}

SyntaxTreeNode * newVarArgsNode(ParseContext & ctx, const Symbol * varArgsSymbol)
{
    // The varargs stack slice will be converted to an array at runtime.
    const SyntaxTreeNode * varArgsTypeNode = newTypeIdNode(ctx, STEval::VarArgs);
    return newVarDeclNode(ctx, varArgsSymbol, nullptr, varArgsTypeNode, STEval::VarArgs);
}

SyntaxTreeNode * newReturnNode(ParseContext & ctx, const SyntaxTreeNode * optExprNode)
{
    ctx.varInfo->foundReturnStmt = true;
    if (!ctx.varInfo->isParsingFunction()) // Stray "return" statement?
    {
        parserError(ctx, "return statement found outside of a function");
    }

    //
    // Void return: "return;"
    //
    if (optExprNode == nullptr)
    {
        if (ctx.varInfo->expectedReturnType != STEval::Void)
        {
            const std::string fname = color::white() + toString(ctx.varInfo->currentFuncSymbol->name) + color::restore();
            parserError(ctx, "function '" + fname + "' was expected to return " +
                        toString(eval2VarType(ctx.varInfo->expectedReturnType)));
        }
        return ctx.syntTree->newNodeWithEval(STNode::ReturnStatement, nullptr, STEval::Void);
    }

    //
    // Non-void return: "return expression;"
    //
    if (ctx.varInfo->expectedReturnType == STEval::Void)
    {
        const std::string fname = color::white() + toString(ctx.varInfo->currentFuncSymbol->name) + color::restore();
        parserError(ctx, "void function '" + fname + "' is not supposed to return a value");
    }

    if ((optExprNode->symbol   != nullptr) &&
        (optExprNode->nodeType != STNode::ExprFuncCall) &&
        (optExprNode->nodeType != STNode::ExprObjectConstructor) &&
        (optExprNode->nodeType != STNode::ExprLiteralConst))
    {
        // Not a var neither a function, then error out.
        if (!ctx.varInfo->findVar(optExprNode->symbol) &&
            !ctx.vm->functions.findFunction(optExprNode->symbol->name))
        {
            parserError(ctx, "undefined return expression '" + toString(optExprNode->symbol->name) + "'");
        }
    }

    if (!isAssignmentValid(eval2VarType(ctx.varInfo->expectedReturnType), eval2VarType(optExprNode->evalType)))
    {
        const std::string fname = color::white() + toString(ctx.varInfo->currentFuncSymbol->name) + color::restore();
        parserError(ctx, "function '" + fname + "' was expected to return " +
                    toString(eval2VarType(ctx.varInfo->expectedReturnType)));
    }

    return ctx.syntTree->newNode(STNode::ReturnStatement, optExprNode,
                                 nullptr, nullptr, optExprNode->evalType);
}

static void collectObjMembers(ParseContext & ctx, const SyntaxTreeNode * root, Object * objOut)
{
    if (root == nullptr)
    {
        return;
    }

    const STEval stEval = root->evalType;
    const auto varName  = root->symbol->name;

    if (objOut->hasMember(varName))
    {
        const std::string mname = color::white() + toString(varName) + color::restore();
        parserError(ctx, objOut->getTypeName() + " member '" + mname + "' was defined more than once");
    }

    Variant data{ eval2VarType(stEval) };

    // Members that reference other User Defined Types require special handling.
    if (data.type == Variant::Type::Object)
    {
        MOON_ASSERT(root->getChild(1) != nullptr);

        const auto typeName = root->getChildSymbol(1)->name;
        const TypeId * typeId = ctx.vm->types.findTypeId(typeName);

        if (typeId == nullptr)
        {
            const std::string tname = color::white() + toString(typeName) + color::restore();
            parserError(ctx, "cannot declare member variable of undefined type '" + tname + "'");
        }

        data.value.asObject = const_cast<Object *>(typeId->templateObject);
    }

    objOut->addMember(varName, data);
    collectObjMembers(ctx, root->getChild(0), objOut);
}

SyntaxTreeNode * newStructDeclNode(ParseContext & ctx, const Symbol * structTypeSymbol,
                                   const SyntaxTreeNode * structMembersListNode)
{
    // Redefinition not allowed!
    const auto typeName = structTypeSymbol->name;
    if (ctx.vm->types.findTypeId(typeName))
    {
        const std::string tname = color::white() + toString(typeName) + color::restore();
        parserError(ctx, "struct '" + tname + "' was defined more than once");
    }

    Object * templateObj = Struct::newInstance(*ctx.vm, nullptr);
    const TypeId * newTypeId = ctx.vm->types.addTypeId(typeName, &Struct::newInstance, templateObj, false);
    templateObj->setTypeId(newTypeId);
    templateObj->markTypeTemplate();

    collectObjMembers(ctx, structMembersListNode, templateObj);
    return ctx.syntTree->newNodeWithSymbol(STNode::StructDeclStatement, structTypeSymbol,
                                           structMembersListNode, nullptr, nullptr, STEval::UDT);
}

SyntaxTreeNode * newTypeAliasNode(ParseContext & ctx, const Symbol * aliasNameSymbol,
                                  const SyntaxTreeNode * aliasedTypeIdNode)
{
    const auto aliasName = aliasNameSymbol->name;
    const auto typeName  = aliasedTypeIdNode->symbol->name;

    // Redefinition not allowed.
    if (ctx.vm->types.findTypeId(aliasName))
    {
        const std::string tname = color::white() + toString(aliasName) + color::restore();
        parserError(ctx, "type alias '" + tname + "' was defined more than once");
    }

    // New definition:
    const TypeId * existingType = ctx.vm->types.findTypeId(typeName);
    if (existingType == nullptr)
    {
        const std::string tname = color::white() + toString(typeName) + color::restore();
        parserError(ctx, "trying to alias undefined type '" + tname + "'");
    }

    ctx.vm->types.addTypeAlias(existingType, aliasName);
    return ctx.syntTree->newNodeWithSymbol(STNode::TypeAliasDeclStatement, aliasNameSymbol,
                                           aliasedTypeIdNode, nullptr, nullptr, STEval::UDT);
}

SyntaxTreeNode * newObjConstructorCallNode(ParseContext & ctx, const SyntaxTreeNode * typeIdNode,
                                           const SyntaxTreeNode * constructorArgsNode)
{
    // First child reserved for parameter_list chains
    return ctx.syntTree->newNodeWithSymbol(STNode::ExprObjectConstructor, typeIdNode->symbol,
                                           nullptr, constructorArgsNode, typeIdNode,
                                           typeIdNode->evalType);
}

static void initEnumMembers(ParseContext & ctx, const SyntaxTreeNode * root,
                            Object * objOut, Int64 & nextEnumIntVal)
{
    if (root == nullptr)
    {
        return;
    }

    MOON_ASSERT(objOut != nullptr);
    MOON_ASSERT(root->symbol != nullptr);
    MOON_ASSERT(root->symbol->name != nullptr);

    const STEval stEval = root->evalType;
    const auto varName  = root->symbol->name;

    if (objOut->hasMember(varName))
    {
        const std::string mname = color::white() + toString(varName) + color::restore();
        parserError(ctx, objOut->getTypeName() + " member '" + mname + "' was defined more than once");
    }

    Variant data{ eval2VarType(stEval) };
    const SyntaxTreeNode * initExprNode = root->getChild(1);

    if (initExprNode != nullptr)
    {
        if (initExprNode->nodeType == STNode::ExprLiteralConst)
        {
            MOON_ASSERT(initExprNode->symbol != nullptr);

            if (initExprNode->evalType == STEval::Int ||
                initExprNode->evalType == STEval::Long)
            {
                MOON_ASSERT(data.type == Variant::Type::Integer);
                data = symbol2Variant(*ctx.vm, *initExprNode->symbol);
            }
            else // string, float, bool
            {
                const Variant initVal = symbol2Variant(*ctx.vm, *initExprNode->symbol);
                MOON_ASSERT(data.type == initVal.type);
                data = initVal;
                // Enum strings have program lifetime.
                if (data.type == Variant::Type::Str)
                {
                    data.getAsString()->isPersistent = true;
                }
            }
        }
        else // identifier / other enum member ref
        {
            if (initExprNode->nodeType == STNode::ExprNameIdent)
            {
                MOON_ASSERT(initExprNode->symbol != nullptr);
                const Variant initVal = objOut->findMemberVar(initExprNode->symbol->name);
                MOON_ASSERT(data.type == initVal.type);
                data = initVal;
            }
            else
            {
                MOON_ASSERT(initExprNode->nodeType == STNode::ExprMemberRef);

                // Already checked when parsing, so an assert suffices.
                MOON_ASSERT(initExprNode->getChildSymbol(1) != nullptr);
                const Symbol * mangled = mangleEnumTypeName(*ctx.symTable, initExprNode->getChildSymbol(1), ctx.lexer->lineno());
                const TypeId * otherEnum = ctx.vm->types.findTypeId(mangled->name);
                MOON_ASSERT(otherEnum != nullptr);

                MOON_ASSERT(initExprNode->getChildSymbol(2) != nullptr);
                const Variant initVal = otherEnum->templateObject->findMemberVar(initExprNode->getChildSymbol(2)->name);
                MOON_ASSERT(data.type == initVal.type);
                data = initVal;
            }
        }
    }
    else // sequential integer list
    {
        MOON_ASSERT(data.type == Variant::Type::Integer);
        data.value.asInteger = nextEnumIntVal++;
    }

    if (data.type == Variant::Type::Integer)
    {
        nextEnumIntVal = data.value.asInteger + 1;
    }

    objOut->addMember(varName, data);
    initEnumMembers(ctx, root->getChild(0), objOut, nextEnumIntVal);
}

static void checkEnumInitializers(ParseContext & ctx, const SyntaxTreeNode * root,
                                  const SyntaxTreeNode * iterNode, int & numOfNonIntegralConstants)
{
    if (iterNode == nullptr)
    {
        return;
    }

    if (iterNode->evalType == STEval::Undefined)
    {
        // Trying to init the constant with itself?
        if (iterNode->symbol == iterNode->getChildSymbol(1))
        {
            const std::string cname = color::white() + toString(iterNode->symbol->name) + color::restore();
            parserError(ctx, "assigning enum constant '" + cname + "' to itself");
        }

        // Reference to a previously defined constant in this enum?
        bool foundIt = false;
        for (auto node = root; node != iterNode; node = node->getChild(0))
        {
            if (node->symbol == iterNode->getChildSymbol(1))
            {
                const_cast<SyntaxTreeNode *>(iterNode)->evalType = node->evalType;
                const_cast<SyntaxTreeNode *>(iterNode->getChild(1))->evalType = node->evalType;
                foundIt = true;
                break;
            }
        }

        if (!foundIt)
        {
            const std::string cname = color::white() + toString(iterNode->getChildSymbol(1)->name) + color::restore();
            parserError(ctx, "assigning undefined identifier '" + cname + "' to enum constant");
        }
    }

    // If not explicitly initialized, we assume sequential integer constants.
    // But in that case, previous constants must also be integers.
    if (iterNode->getChild(1) == nullptr)
    {
        if (numOfNonIntegralConstants != 0)
        {
            parserError(ctx, "unable to deduce next constant value for enum with non-integer constants");
        }
    }

    if (iterNode->evalType != STEval::Int &&
        iterNode->evalType != STEval::Long)
    {
        ++numOfNonIntegralConstants;
    }

    checkEnumInitializers(ctx, root, iterNode->getChild(0), numOfNonIntegralConstants);
}

const Symbol * mangleEnumTypeName(SymbolTable & symTable, const Symbol * enumNameSymbol, int declLineNum)
{
    char enumTypeName[256];
    std::snprintf(enumTypeName, arrayLength(enumTypeName), "__enum__%s", enumNameSymbol->name->chars);
    return symTable.findOrDefineIdentifier(enumTypeName, declLineNum);
}

const Symbol * demangleEnumTypeName(SymbolTable & symTable, const Symbol * enumNameSymbol)
{
    const char * name    = enumNameSymbol->name->chars;
    const char * prefix  = "__enum__";
    const auto prefixLen = std::strlen(prefix);

    if (std::strncmp(name, prefix, prefixLen) != 0)
    {
        return enumNameSymbol;
    }

    char outputName[256];
    std::snprintf(outputName, arrayLength(outputName), "%s", name + prefixLen);
    return symTable.findOrDefineIdentifier(outputName, Symbol::LineNumBuiltIn);
}

SyntaxTreeNode * newEnumDeclNode(ParseContext & ctx, const Symbol * enumNameSymbol, SyntaxTreeNode * enumConstantsListNode)
{
    // The enum type name is mangled, so we can declare a global variable with the name
    // given in the source code. Enums will then be handled just as global variables would.
    const Symbol * enumTypeNameSymbol = mangleEnumTypeName(*ctx.symTable, enumNameSymbol, ctx.lexer->lineno());

    // Redefinition not allowed.
    const auto typeName = enumTypeNameSymbol->name;
    if (ctx.vm->types.findTypeId(typeName))
    {
        const std::string tname = color::white() + toString(typeName) + color::restore();
        parserError(ctx, "enum '" + tname + "' was defined more than once");
    }

    Object * templateObj = Enum::newInstance(*ctx.vm, nullptr);
    const TypeId * newTypeId = ctx.vm->types.addTypeId(typeName, &Enum::newInstance, templateObj, false);
    templateObj->setTypeId(newTypeId);
    templateObj->markTypeTemplate();

    // The enum instance becomes a global object, while the type is stored with a mangled name.
    SyntaxTreeNode * initNode = nullptr;
    SyntaxTreeNode * typeNode = ctx.syntTree->newNodeWithSymbol(STNode::ExprTypeIdent,
                          enumTypeNameSymbol, nullptr, nullptr, nullptr, STEval::UDT);
    registerGlobalOrLocal(ctx, enumNameSymbol, initNode, typeNode, STEval::UDT);

    // This will search for undefined STEvals in the initializers and update
    // them if the enum const references a global constant or a previously
    // defined sibling constant in this enum. It also checks that uninitialized
    // constant are not used on an enum with non-integral values.
    int numOfNonIntegralConstants = 0;
    checkEnumInitializers(ctx, enumConstantsListNode, enumConstantsListNode, numOfNonIntegralConstants);

    Int64 nextEnumIntVal = 0;
    initEnumMembers(ctx, enumConstantsListNode, templateObj, nextEnumIntVal);

    return ctx.syntTree->newNodeWithSymbol(STNode::EnumDeclStatement, enumTypeNameSymbol,
                                           enumConstantsListNode, nullptr, nullptr, STEval::UDT);
}

SyntaxTreeNode * newEnumConstNode(ParseContext & ctx, const Symbol * enumConstNameSymbol,
                                  const SyntaxTreeNode * initializerNode, const STEval constEval)
{
    return ctx.syntTree->newNodeWithSymbol(STNode::ExprNameIdent, enumConstNameSymbol,
                                           nullptr, initializerNode, nullptr, constEval);
}

STEval enumMemberConstantReference(ParseContext & ctx, SyntaxTreeNode * eTypeNode, SyntaxTreeNode * eConstNode)
{
    MOON_ASSERT(eTypeNode->symbol  != nullptr);
    MOON_ASSERT(eConstNode->symbol != nullptr);

    const Symbol * typeName = mangleEnumTypeName(*ctx.symTable, eTypeNode->symbol, ctx.lexer->lineno());
    const TypeId * enumType = ctx.vm->types.findTypeId(typeName->name);

    if (enumType == nullptr)
    {
        const std::string ename = color::white() + toString(eTypeNode->symbol->name) + color::restore();
        parserError(ctx, "reference to undefined enum type '" + ename + "'");
    }
    MOON_ASSERT(enumType->templateObject != nullptr);

    const auto constName = eConstNode->symbol->name;
    const int  enumConst = enumType->templateObject->findMemberIndex(constName);

    if (enumConst < 0)
    {
        const std::string cname = color::white() + toString(eTypeNode->symbol->name) + "." + toString(constName) + color::restore();
        parserError(ctx, "reference to undefined enum constant '" + cname + "'");
    }

    // Okay
    const STEval stEvalConst = varType2Eval(enumType->templateObject->getMemberAt(enumConst).data.type);
    eTypeNode->evalType  = STEval::UDT;
    eConstNode->evalType = stEvalConst;
    return stEvalConst;
}

// ========================================================
// Internal type conversion utilities:
// ========================================================

STEval varType2Eval(const Variant::Type varType)
{
    switch (varType)
    {
    case Variant::Type::Null     : return STEval::Null;
    case Variant::Type::Integer  : return STEval::Long;
    case Variant::Type::Float    : return STEval::Double;
    case Variant::Type::Function : return STEval::Func;
    case Variant::Type::Tid      : return STEval::Tid;
    case Variant::Type::Str      : return STEval::Str;
    case Variant::Type::Object   : return STEval::UDT;
    case Variant::Type::Array    : return STEval::Array;
    case Variant::Type::Range    : return STEval::Range;
    case Variant::Type::Any      : return STEval::Any;
    default : MOON_INTERNAL_EXCEPTION("can't convert Variant type '" + toString(varType) + "' to STEval!");
    } // switch (varType)
}

Variant::Type eval2VarType(const STEval stEval)
{
    switch (stEval)
    {
    case STEval::Null    : return Variant::Type::Null;
    case STEval::VarArgs : return Variant::Type::Array;
    case STEval::Int     : return Variant::Type::Integer;
    case STEval::Long    : return Variant::Type::Integer;
    case STEval::Float   : return Variant::Type::Float;
    case STEval::Double  : return Variant::Type::Float;
    case STEval::Bool    : return Variant::Type::Integer;
    case STEval::Str     : return Variant::Type::Str;
    case STEval::Array   : return Variant::Type::Array;
    case STEval::Range   : return Variant::Type::Range;
    case STEval::Any     : return Variant::Type::Any;
    case STEval::Func    : return Variant::Type::Function;
    case STEval::Tid     : return Variant::Type::Tid;
    case STEval::UDT     : return Variant::Type::Object;
    default : MOON_INTERNAL_EXCEPTION("can't convert STEval '" + toString(stEval) + "' to Variant type!");
    } // switch (stEval)
}

const TypeId * varType2TypeId(const TypeTable & typeTable, const Variant::Type varType)
{
    switch (varType)
    {
    case Variant::Type::Null     : return typeTable.nullTypeId;
    case Variant::Type::Integer  : return typeTable.longTypeId;
    case Variant::Type::Float    : return typeTable.doubleTypeId;
    case Variant::Type::Function : return typeTable.functionTypeId;
    case Variant::Type::Tid      : return typeTable.tidTypeId;
    case Variant::Type::Str      : return typeTable.strTypeId;
    case Variant::Type::Object   : return typeTable.objectTypeId;
    case Variant::Type::Array    : return typeTable.arrayTypeId;
    case Variant::Type::Range    : return typeTable.rangeTypeId;
    case Variant::Type::Any      : return typeTable.anyTypeId;
    default : MOON_INTERNAL_EXCEPTION("can't convert Variant type '" + toString(varType) + "' to STEval!");
    } // switch (varType)
}

const TypeId * eval2TypeId(const TypeTable & typeTable, const STEval stEval)
{
    switch (stEval)
    {
    case STEval::Null    : return typeTable.nullTypeId;
    case STEval::VarArgs : return typeTable.arrayTypeId;
    case STEval::Int     : return typeTable.intTypeId;
    case STEval::Long    : return typeTable.longTypeId;
    case STEval::Float   : return typeTable.floatTypeId;
    case STEval::Double  : return typeTable.doubleTypeId;
    case STEval::Bool    : return typeTable.boolTypeId;
    case STEval::Str     : return typeTable.strTypeId;
    case STEval::Array   : return typeTable.arrayTypeId;
    case STEval::Range   : return typeTable.rangeTypeId;
    case STEval::Any     : return typeTable.anyTypeId;
    case STEval::Func    : return typeTable.functionTypeId;
    case STEval::Tid     : return typeTable.tidTypeId;
    case STEval::UDT     : return typeTable.objectTypeId;
    default : MOON_INTERNAL_EXCEPTION("can't convert STEval '" + toString(stEval) + "' to TypeId!");
    } // switch (stEval)
}

STEval symbol2Eval(const Symbol & symbol)
{
    switch (symbol.type)
    {
    case Symbol::Type::IntLiteral   : return STEval::Long;
    case Symbol::Type::FloatLiteral : return STEval::Double;
    case Symbol::Type::BoolLiteral  : return STEval::Bool;
    case Symbol::Type::StrLiteral   : return STEval::Str;
    case Symbol::Type::Identifier   : return STEval::UDT;
    default : MOON_INTERNAL_EXCEPTION("can't convert symbol '" + toString(symbol.type) + "' to STEval!");
    } // switch (symbol.type)
}

const Symbol * eval2Symbol(const SymbolTable & symTable, const STEval stEval)
{
    // These are built-ins always present in the symbol table. Check symbol_table.cpp.
    switch (stEval)
    {
    case STEval::Null    : return symTable.findSymbol("null");
    case STEval::Void    : return symTable.findSymbol("void");
    case STEval::VarArgs : return symTable.findSymbol("varargs");
    case STEval::Int     : return symTable.findSymbol("int");
    case STEval::Long    : return symTable.findSymbol("long");
    case STEval::Float   : return symTable.findSymbol("float");
    case STEval::Double  : return symTable.findSymbol("double");
    case STEval::Bool    : return symTable.findSymbol("bool");
    case STEval::Str     : return symTable.findSymbol("str");
    case STEval::Array   : return symTable.findSymbol("array");
    case STEval::Range   : return symTable.findSymbol("range");
    case STEval::Any     : return symTable.findSymbol("any");
    case STEval::Func    : return symTable.findSymbol("function");
    case STEval::Tid     : return symTable.findSymbol("tid");
    case STEval::UDT     : return symTable.findSymbol("object");
    default : MOON_INTERNAL_EXCEPTION("can't convert STEval '" + toString(stEval) + "' to a symbol!");
    } // switch (stEval)
}

Variant symbol2Variant(VM & vm, const Symbol & symbol)
{
    Variant var;
    switch (symbol.type)
    {
    case Symbol::Type::IntLiteral :
        var.type = Variant::Type::Integer;
        var.value.asInteger = symbol.value.asInteger;
        break;
    case Symbol::Type::FloatLiteral :
        var.type = Variant::Type::Float;
        var.value.asFloat = symbol.value.asFloat;
        break;
    case Symbol::Type::BoolLiteral :
        var.type = Variant::Type::Integer;
        var.value.asInteger = symbol.value.asBoolean;
        break;
    case Symbol::Type::StrLiteral :
        var.type = Variant::Type::Str;
        var.value.asString = Str::newFromString(vm, symbol.value.asString);
        break;
    default :
        MOON_RUNTIME_EXCEPTION("symbol '" + toString(symbol) + "' doesn't map directly to a Variant!");
    } // switch (symbol.type)
    return var;
}

Variant::Type typeId2VarType(const TypeId * tid)
{
    MOON_ASSERT(tid != nullptr && isRcStringValid(tid->name));

    const auto hashId = tid->name->hashVal;
    if (ct::hashCString( "int"      ) == hashId) { return Variant::Type::Integer;  }
    if (ct::hashCString( "long"     ) == hashId) { return Variant::Type::Integer;  }
    if (ct::hashCString( "float"    ) == hashId) { return Variant::Type::Float;    }
    if (ct::hashCString( "double"   ) == hashId) { return Variant::Type::Float;    }
    if (ct::hashCString( "bool"     ) == hashId) { return Variant::Type::Integer;  }
    if (ct::hashCString( "range"    ) == hashId) { return Variant::Type::Range;    }
    if (ct::hashCString( "any"      ) == hashId) { return Variant::Type::Any;      }
    if (ct::hashCString( "object"   ) == hashId) { return Variant::Type::Object;   }
    if (ct::hashCString( "function" ) == hashId) { return Variant::Type::Function; }
    if (ct::hashCString( "str"      ) == hashId) { return Variant::Type::Str;      }
    if (ct::hashCString( "array"    ) == hashId) { return Variant::Type::Array;    }
    if (ct::hashCString( "tid"      ) == hashId) { return Variant::Type::Tid;      }

    MOON_RUNTIME_EXCEPTION("bad variable type id '" + toString(tid->name) +
                           "' (" + strPrintF("0x%08X", hashId) + ")");
}

// ========================================================
// Additional error/warning handlers:
// ========================================================

[[noreturn]] void parserError(ParseContext & ctx, const std::string & message) noexcept(false)
{
    MOON_ASSERT(ctx.lexer != nullptr);
    const std::string filename = (ctx.srcFile != nullptr) ? *ctx.srcFile : "???";
    parserError(message, filename, ctx.lexer->lineno(), ctx.currText);
}

void parserWarn(ParseContext & ctx, const std::string & message)
{
    MOON_ASSERT(ctx.lexer != nullptr);

    if (!ctx.enableWarnings)
    {
        return;
    }

    const std::string filename = (ctx.srcFile != nullptr) ? *ctx.srcFile : "???";
    warning(message, filename, ctx.lexer->lineno(), ctx.currText);
}

} // namespace moon {}
