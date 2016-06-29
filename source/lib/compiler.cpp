
// ================================================================================================
// -*- C++ -*-
// File: compiler.cpp
// Author: Guilherme R. Lampert
// Created on: 06/07/15
// Brief: The Compiler class transforms a Syntax Tree representation into VM bytecode.
// ================================================================================================

#include "compiler.hpp"
#include <algorithm>
#include <iomanip> // For std::setw used in the debug printers
#include <fstream> // Used by the DefaultFileIOCallbacks

namespace moon
{

// ========================================================
// Debugging helpers:
// ========================================================

#if MOON_ENABLE_ASSERT

#define EXPECT_CHILD_AT_INDEX(node, childIndex)                              \
    do                                                                       \
    {                                                                        \
        MOON_ASSERT((node) != nullptr);                                      \
        if ((node)->getChild((childIndex)) == nullptr)                       \
        {                                                                    \
            MOON_INTERNAL_EXCEPTION("expected node for child index " +       \
                                    toString((childIndex)));                 \
        }                                                                    \
    } while (0)

#define EXPECT_NUM_CHILDREN(node, expected)                                  \
    do                                                                       \
    {                                                                        \
        MOON_ASSERT((node) != nullptr);                                      \
        for (int n = 0; n < (expected); ++n)                                 \
        {                                                                    \
            if ((node)->getChild(n) == nullptr)                              \
            {                                                                \
                MOON_INTERNAL_EXCEPTION("expected " + toString((expected)) + \
                                        " child nodes, got " + toString(n)); \
            }                                                                \
        }                                                                    \
    } while (0)

#define EXPECT_SYMBOL(node)                                                  \
    do                                                                       \
    {                                                                        \
        MOON_ASSERT((node) != nullptr);                                      \
        if ((node)->symbol == nullptr)                                       \
        {                                                                    \
            MOON_INTERNAL_EXCEPTION("expected symbol for node " +            \
                                    toString((node)->nodeType));             \
        }                                                                    \
    } while (0)

#else // !MOON_ENABLE_ASSERT

// Extra error checking is disabled.
#define EXPECT_CHILD_AT_INDEX(node, childIndex)
#define EXPECT_NUM_CHILDREN(node, expected)
#define EXPECT_SYMBOL(node)

#endif // MOON_ENABLE_ASSERT

// ========================================================
// Intermediate Instruction node allocation:
// ========================================================

static IntermediateInstr * newInstruction(Compiler & compiler, const OpCode op)
{
    IntermediateInstr * instr = compiler.instrPool.allocate();
    instr->next               = nullptr;
    instr->operand.symbol     = nullptr;
    instr->uid                = compiler.instrCount++;
    instr->stackIndex         = InvalidStackIndex;
    instr->dataType           = Variant::Type::Null;
    instr->op                 = op;
    return instr;
}

static IntermediateInstr * newInstruction(Compiler & compiler, const OpCode op, const Symbol * symbol,
                                          const Variant::Type dataType, const UInt16 stackIndex)
{
    IntermediateInstr * instr = compiler.instrPool.allocate();
    instr->next               = nullptr;
    instr->operand.symbol     = symbol;
    instr->uid                = compiler.instrCount++;
    instr->stackIndex         = stackIndex;
    instr->dataType           = dataType;
    instr->op                 = op;
    return instr;
}

static IntermediateInstr * newInstruction(Compiler & compiler, const OpCode op, const IntermediateInstr * jumpTarget)
{
    IntermediateInstr * instr = compiler.instrPool.allocate();
    instr->next               = nullptr;
    instr->operand.jumpTarget = jumpTarget;
    instr->uid                = compiler.instrCount++;
    instr->stackIndex         = InvalidStackIndex;
    instr->dataType           = Variant::Type::Null;
    instr->op                 = op;
    return instr;
}

static IntermediateInstr * newInstruction(Compiler & compiler, const IntermediateInstr * copy)
{
    MOON_ASSERT(copy != nullptr);
    IntermediateInstr * instr = compiler.instrPool.allocate();
    *instr                    = *copy;
    instr->next               = nullptr;
    instr->uid                = compiler.instrCount++;
    return instr;
}

static IntermediateInstr * linkInstructions(IntermediateInstr * head, IntermediateInstr * newTail)
{
    MOON_ASSERT(head != nullptr);

    // Instruction chains where we use this function are short.
    // The longest would be from a big if/elseif construct, which
    // shouldn't be longer than 20 or so nodes on any sane piece
    // of code. This is unlikely to ever be a performance issue.
    IntermediateInstr * search = head;
    for (; search->next != nullptr; search = search->next) { }
    search->next = newTail;
    return head;
}

template<typename... Instructions>
static IntermediateInstr * linkInstructions(IntermediateInstr * head, Instructions... args)
{
    MOON_ASSERT(head != nullptr);

    IntermediateInstr * search = head;
    for (; search->next != nullptr; search = search->next) { }
    search->next = linkInstructions(args...);
    return head;
}

// ========================================================
// Intermediate code generation:
// ========================================================

using DataMap  = HashTable<const Symbol *, UInt32>;
using InstrMap = HashTable<const IntermediateInstr *, UInt32>;

struct CodeGenContext final
{
    CodeGenContext() = default;
    CodeGenContext(const CodeGenContext &) = delete;
    CodeGenContext & operator = (const CodeGenContext &) = delete;

    // VM is needed mainly for the runtime type info tables.
    VM * vm = nullptr;

    // These are used to combine repeated program data/symbols
    // and to optimize away noops in the VM Bytecode generation step.
    DataMap  dataMapping;
    InstrMap instrMapping;

    // Somewhat hackish way of keeping track of the
    // head and tail of a loop for break/continue jumps,
    // plus the cached name of the iterator var in a for-loop.
    const IntermediateInstr * loopStartAnchor     = nullptr;
    const IntermediateInstr * loopEndAnchor       = nullptr;
    const SyntaxTreeNode    * forLoopIteratorNode = nullptr;

    // Necessary for object member access code gen.
    const TypeId * lastMemberTypeId = nullptr;

    // For resolving 'object.member.member...' chains.
    std::vector<const Symbol *> memberRefList;

    //
    // Visited nodes (used for parameter lists/constructors code gen):
    //

    // For some expressions we need to keep track of the visited nodes
    // (e.g.: counting function parameter lists). This temporary array
    // is used for such cases.
    std::vector<const SyntaxTreeNode *> visitedNodes;

    bool nodeWasVisited(const SyntaxTreeNode * node) const
    {
        if (node == nullptr)
        {
            return false;
        }
        const auto iter = std::find(std::begin(visitedNodes), std::end(visitedNodes), node);
        return iter != std::end(visitedNodes);
    }

    //
    // Function definition code gen:
    //

    const IntermediateInstr * returnAnchor = nullptr;
    const Symbol * currFuncNameSymbol      = nullptr;
    int currentStackSize                   = 0;

    bool isInsideFunctionBody() const noexcept
    {
        return currFuncNameSymbol != nullptr;
    }

    void beginFunction(const IntermediateInstr * endLabel, const SyntaxTreeNode * root, VarInfoTable & varInfo)
    {
        if (isInsideFunctionBody())
        {
            MOON_INTERNAL_EXCEPTION("nested functions not supported yet");
        }

        returnAnchor       = endLabel;
        currFuncNameSymbol = root->symbol;
        currentStackSize   = std::max(0, varInfo.getFuncStackSize(currFuncNameSymbol) - 1);
    }

    void endFunction()
    {
        returnAnchor       = nullptr;
        currFuncNameSymbol = nullptr;
        currentStackSize   = 0;
    }
};

// ========================================================

using EmitInstrForNodeCB = IntermediateInstr * (*)(Compiler &, CodeGenContext &, const SyntaxTreeNode *);
static IntermediateInstr * traverseTreeRecursive(Compiler & compiler, CodeGenContext & codeGen, const SyntaxTreeNode * root);

static void findSymbolInfo(Compiler & compiler, CodeGenContext & codeGen, const Symbol * objSymbol,
                           Variant::Type * outDataType, UInt16 * outStackIndex, bool * outIsFuncLocal)
{
    const VarInfo * vi;
    if (codeGen.isInsideFunctionBody())
    {
        vi = compiler.varInfo.findNamedFuncLocalVar(objSymbol, codeGen.currFuncNameSymbol);
        if (vi != nullptr)
        {
            (*outIsFuncLocal) = true;
            (*outStackIndex)  = vi->stackIndex;
        }
        else // Try the globals
        {
            (*outIsFuncLocal) = false;
            (*outStackIndex)  = InvalidStackIndex;
            if (codeGen.forLoopIteratorNode == nullptr) // Only if not doing a for loop
            {
                vi = compiler.varInfo.findGlobalVar(objSymbol);
            }
        }
    }
    else // Generating global code
    {
        (*outIsFuncLocal) = false;
        (*outStackIndex)  = InvalidStackIndex;
        vi = compiler.varInfo.findGlobalVar(objSymbol);
    }

    if (vi != nullptr)
    {
        (*outDataType) = vi->varType;
    }
    else
    {
        if (codeGen.forLoopIteratorNode != nullptr && codeGen.forLoopIteratorNode->symbol == objSymbol)
        {
            (*outDataType) = eval2VarType(codeGen.forLoopIteratorNode->evalType);
            if (codeGen.isInsideFunctionBody())
            {
                (*outIsFuncLocal) = true;
                (*outStackIndex)  = codeGen.currentStackSize;
            }
        }
        else
        {
            (*outDataType) = Variant::Type::Null;
        }
    }
}

static const TypeId * findTypeNameTypeId(CodeGenContext & codeGen, const Symbol * typeSymbol)
{
    // Type-Id of a type name.
    MOON_ASSERT(typeSymbol != nullptr);
    MOON_ASSERT(codeGen.vm != nullptr);
    return codeGen.vm->types.findTypeId(typeSymbol->name);
}

static const TypeId * findObjectTypeId(Compiler & compiler, CodeGenContext & codeGen, const Symbol * objSymbol)
{
    // Type-Id of a variable/object.
    const VarInfo * vi;
    if (codeGen.isInsideFunctionBody())
    {
        vi = compiler.varInfo.findNamedFuncLocalVar(objSymbol, codeGen.currFuncNameSymbol);
        if (vi == nullptr) // Okay, try the globals now
        {
            vi = compiler.varInfo.findGlobalVar(objSymbol);
        }
    }
    else
    {
        vi = compiler.varInfo.findGlobalVar(objSymbol);
    }

    return (vi != nullptr ? vi->typeId : nullptr);
}

static IntermediateInstr * makeLoadIntLiteral(Compiler & compiler, const int value)
{
    const Symbol * literalValueSym = compiler.symTable.findOrDefineIntLiteral(value);
    return newInstruction(compiler, OpCode::LoadGlobal, literalValueSym, Variant::Type::Integer, InvalidStackIndex);
}

static IntermediateInstr * makeLoadMemberOffsetInstr(Compiler & compiler, CodeGenContext & codeGen, const Symbol * objSymbol,
                                                     const Symbol * memberSymbol, const TypeId ** outTypeId, int * outMemOffset)
{
    const TypeId * typeId = findObjectTypeId(compiler, codeGen, objSymbol);
    if (typeId == nullptr || typeId->templateObject == nullptr)
    {
        MOON_INTERNAL_EXCEPTION("'" + toString(objSymbol->name) + "' is not a type or has no dynamic members table!");
    }

    const int memberOffset = typeId->templateObject->findMemberIndex(memberSymbol->name);
    if (memberOffset < 0)
    {
        MOON_INTERNAL_EXCEPTION("object has no member '" + toString(memberSymbol->name) + "'");
    }

    MOON_ASSERT(outTypeId    != nullptr);
    MOON_ASSERT(outMemOffset != nullptr);

    (*outTypeId)    = typeId;
    (*outMemOffset) = memberOffset;

    return makeLoadIntLiteral(compiler, memberOffset);
}

static IntermediateInstr * appendTrailingLoadOpIfNeeded(Compiler & compiler, IntermediateInstr * argument, const SyntaxTreeNode * root)
{
    if (root->nodeType == SyntaxTreeNode::Type::ExprAssign    ||
        root->nodeType == SyntaxTreeNode::Type::ExprSubAssign ||
        root->nodeType == SyntaxTreeNode::Type::ExprAddAssign ||
        root->nodeType == SyntaxTreeNode::Type::ExprModAssign ||
        root->nodeType == SyntaxTreeNode::Type::ExprDivAssign ||
        root->nodeType == SyntaxTreeNode::Type::ExprMulAssign)
    {
        const IntermediateInstr * last = argument;
        for (; last->next != nullptr; last = last->next) { }

        // Stupid "c = a += b" kid of code.
        // Need to load the result of the rhs expression back into the stack.
        // Reusing the last store op from the resolved argument for that.
        IntermediateInstr * tailLoad = newInstruction(compiler, OpCode::NoOp);
        tailLoad->op         = (last->op == OpCode::StoreGlobal ? OpCode::LoadGlobal : OpCode::LoadLocal);
        tailLoad->dataType   = last->dataType;
        tailLoad->stackIndex = last->stackIndex;
        tailLoad->operand    = last->operand;
        argument = linkInstructions(argument, tailLoad);
    }

    return argument;
}

static IntermediateInstr * getParamListChain(Compiler & compiler, CodeGenContext & codeGen, const SyntaxTreeNode * root)
{
    return (root->getChild(0) ? traverseTreeRecursive(compiler, codeGen, root->getChild(0)) : nullptr);
}

static IntermediateInstr * emitNOOP(Compiler & compiler, CodeGenContext &, const SyntaxTreeNode *)
{
    // For the node types that generate no code.
    return newInstruction(compiler, OpCode::NoOp);
}

static IntermediateInstr * emitProgStart(Compiler & compiler, CodeGenContext & codeGen, const SyntaxTreeNode * root)
{
    IntermediateInstr * programRoot = newInstruction(compiler, OpCode::ProgStart);
    if (root->getChild(0) != nullptr) // A translation unit might be empty.
    {
        IntermediateInstr * translationUnit = traverseTreeRecursive(compiler, codeGen, root->getChild(0));
        programRoot = linkInstructions(programRoot, translationUnit);
    }
    return programRoot;
}

static IntermediateInstr * emitStatement(Compiler & compiler, CodeGenContext & codeGen, const SyntaxTreeNode * root)
{
    EXPECT_CHILD_AT_INDEX(root, 1);
    if (root->getChild(0) != nullptr)
    {
        IntermediateInstr * lhs = traverseTreeRecursive(compiler, codeGen, root->getChild(0));
        IntermediateInstr * rhs = traverseTreeRecursive(compiler, codeGen, root->getChild(1));
        return linkInstructions(lhs, rhs);
    }
    return traverseTreeRecursive(compiler, codeGen, root->getChild(1));
}

static IntermediateInstr * emitIfThen(Compiler & compiler, CodeGenContext & codeGen, const SyntaxTreeNode * root)
{
    // If the then-statement ('if' body) is not empty:
    if (root->getChild(1) != nullptr)
    {
        EXPECT_NUM_CHILDREN(root, 2);
        IntermediateInstr * condition     = traverseTreeRecursive(compiler, codeGen, root->getChild(0));
        IntermediateInstr * thenStatement = traverseTreeRecursive(compiler, codeGen, root->getChild(1));
        IntermediateInstr * noopEnd       = newInstruction(compiler, OpCode::NoOp);
        IntermediateInstr * jumpIfFalse   = newInstruction(compiler, OpCode::JmpIfFalse, noopEnd);

        if (root->getChild(0)->nodeType == SyntaxTreeNode::Type::ExprFuncCall)
        {
            condition = linkInstructions(condition, newInstruction(compiler, OpCode::LoadRVR));
        }

        // if-cond => jump-end-if-false => if-body => noop[jump-target/end]
        return linkInstructions(condition, jumpIfFalse, thenStatement, noopEnd);
    }
    else // Empty then-statement/body:
    {
        MOON_ASSERT(root->getChild(0) != nullptr);
        IntermediateInstr * condition   = traverseTreeRecursive(compiler, codeGen, root->getChild(0));
        IntermediateInstr * noopEnd     = newInstruction(compiler, OpCode::NoOp);
        IntermediateInstr * jumpIfFalse = newInstruction(compiler, OpCode::JmpIfFalse, noopEnd);

        if (root->getChild(0)->nodeType == SyntaxTreeNode::Type::ExprFuncCall)
        {
            condition = linkInstructions(condition, newInstruction(compiler, OpCode::LoadRVR));
        }

        // if-cond => jump-end-if-false => noop[jump-target/end]
        return linkInstructions(condition, jumpIfFalse, noopEnd);
    }
}

static IntermediateInstr * emitIfThenElse(Compiler & compiler, CodeGenContext & codeGen, const SyntaxTreeNode * root)
{
    // child[0] = condition/expression   (must have)
    // child[1] = if-body/then-statement (may be empty)
    // child[2] = else-body/statement    (may be empty)

    // Must have a condition part:
    EXPECT_CHILD_AT_INDEX(root, 0);
    IntermediateInstr * condition = traverseTreeRecursive(compiler, codeGen, root->getChild(0));

    if (root->getChild(0)->nodeType == SyntaxTreeNode::Type::ExprFuncCall)
    {
        condition = linkInstructions(condition, newInstruction(compiler, OpCode::LoadRVR));
    }

    // If the then-statement body and else-statement body are not empty, resolve them:
    IntermediateInstr * thenStatement = (root->getChild(1) ? traverseTreeRecursive(compiler, codeGen, root->getChild(1)) : nullptr);
    IntermediateInstr * elseStatement = (root->getChild(2) ? traverseTreeRecursive(compiler, codeGen, root->getChild(2)) : nullptr);

    // Jump labels:
    IntermediateInstr * noopEnd     = newInstruction(compiler, OpCode::NoOp);
    IntermediateInstr * jumpToEnd   = newInstruction(compiler, OpCode::Jmp, noopEnd);
    IntermediateInstr * jumpIfFalse = newInstruction(compiler, OpCode::JmpIfFalse, (elseStatement != nullptr) ? elseStatement : jumpToEnd);

    IntermediateInstr * block0 = linkInstructions(condition, jumpIfFalse);
    IntermediateInstr * block1 = nullptr;

    // Handle the instruction sequence differently if the if or else bodies are empty:
    if (thenStatement != nullptr)
    {
        block1 = linkInstructions(block0, thenStatement, jumpToEnd);
    }
    else
    {
        block1 = linkInstructions(block0, jumpToEnd);
    }

    if (elseStatement != nullptr)
    {
        return linkInstructions(block1, elseStatement, noopEnd);
    }
    else
    {
        return linkInstructions(block1, noopEnd);
    }

    // if-cond => jump-else-if-false => if-body =>
    // jump-end => else-body => noop[jump-target/end]
}

static IntermediateInstr * emitIfThenElseIf(Compiler & compiler, CodeGenContext & codeGen, const SyntaxTreeNode * root)
{
    // When entering the if/elseif chain for the first time:
    //
    // child[0] = if condition/expression    (must have)
    // child[1] = if-body/then-statement     (may be empty)
    // child[2] = elseif-body/then-statement (may be empty)
    //
    // After the initial `if`:
    //
    // child[0] = elseif condition/expression (must have)
    // child[1] = elseif-body/then-statement  (may be empty)
    // child[2] = terminal else-statement or another elseif (which may have empty bodies)

    // Must have a condition part:
    EXPECT_CHILD_AT_INDEX(root, 0);
    IntermediateInstr * condition = traverseTreeRecursive(compiler, codeGen, root->getChild(0));

    if (root->getChild(0)->nodeType == SyntaxTreeNode::Type::ExprFuncCall)
    {
        condition = linkInstructions(condition, newInstruction(compiler, OpCode::LoadRVR));
    }

    // If the then-statement body and elseif body are not empty, resolve them:
    IntermediateInstr * thenStatement = (root->getChild(1) ? traverseTreeRecursive(compiler, codeGen, root->getChild(1)) : nullptr);
    IntermediateInstr * elseStatement = (root->getChild(2) ? traverseTreeRecursive(compiler, codeGen, root->getChild(2)) : nullptr);

    // NOTE: This will produce a noop at the end for each of the
    // if/elseif conditions in the chain. Those can be consolidated into
    // a single noop/jump-target by the final bytecode compilation stage.
    IntermediateInstr * noopEnd     = newInstruction(compiler, OpCode::NoOp);
    IntermediateInstr * jumpToEnd   = newInstruction(compiler, OpCode::Jmp, noopEnd);
    IntermediateInstr * jumpIfFalse = newInstruction(compiler, OpCode::JmpIfFalse, (elseStatement != nullptr) ? elseStatement : jumpToEnd);

    // if|elseif-cond => jump-next-if-false => if|elseif-body =>
    // jump-end => [repeat till an else] => noop[jump-target/end]
    if (elseStatement != nullptr)
    {
        return linkInstructions(condition, jumpIfFalse, thenStatement, jumpToEnd, elseStatement, noopEnd);
    }
    else
    {
        return linkInstructions(condition, jumpIfFalse, thenStatement, jumpToEnd, noopEnd);
    }
}

static IntermediateInstr * emitLoop(Compiler & compiler, CodeGenContext & codeGen, const SyntaxTreeNode * root)
{
    // Anchor the start and end of the loop with noops we can jump to.
    IntermediateInstr * noopLabelStart = newInstruction(compiler, OpCode::NoOp);
    IntermediateInstr * noopLabelEnd   = newInstruction(compiler, OpCode::NoOp);
    IntermediateInstr * loopContinue   = newInstruction(compiler, OpCode::Jmp, noopLabelStart);

    // If there's any code in the loop body:
    if (root->getChild(0) != nullptr)
    {
        // Jump targets for possible 'continue/break' inside the loop.
        codeGen.loopStartAnchor = noopLabelStart;
        codeGen.loopEndAnchor   = noopLabelEnd;

        IntermediateInstr * loopBody = traverseTreeRecursive(compiler, codeGen, root->getChild(0));

        codeGen.loopStartAnchor = nullptr;
        codeGen.loopEndAnchor   = nullptr;

        return linkInstructions(noopLabelStart, loopBody, loopContinue, noopLabelEnd);
    }

    // Empty unconditional loop (suspicious, but legal code).
    return linkInstructions(noopLabelStart, loopContinue, noopLabelEnd);
}

static IntermediateInstr * emitWhileLoop(Compiler & compiler, CodeGenContext & codeGen, const SyntaxTreeNode * root)
{
    // The conditional expression must be present.
    EXPECT_CHILD_AT_INDEX(root, 0);
    IntermediateInstr * loopCond     = traverseTreeRecursive(compiler, codeGen, root->getChild(0));
    IntermediateInstr * noopLabelEnd = newInstruction(compiler, OpCode::NoOp);
    IntermediateInstr * loopContinue = newInstruction(compiler, OpCode::Jmp, loopCond);

    // The jump to bail the loop when cond == false:
    IntermediateInstr * jumpIfFalse = newInstruction(compiler, OpCode::JmpIfFalse, noopLabelEnd);
    loopCond = linkInstructions(loopCond, jumpIfFalse);

    // If there's any code in the loop body:
    if (root->getChild(1) != nullptr)
    {
        // Jump targets for possible 'continue/break' inside the loop.
        codeGen.loopStartAnchor = loopCond;
        codeGen.loopEndAnchor   = noopLabelEnd;

        IntermediateInstr * loopBody = traverseTreeRecursive(compiler, codeGen, root->getChild(1));

        codeGen.loopStartAnchor = nullptr;
        codeGen.loopEndAnchor   = nullptr;

        return linkInstructions(loopCond, loopBody, loopContinue, noopLabelEnd);
    }

    // Empty while loop:
    return linkInstructions(loopCond, loopContinue, noopLabelEnd);
}

static IntermediateInstr * emitForLoop(Compiler & compiler, CodeGenContext & codeGen, const SyntaxTreeNode * root)
{
    auto makeLoadInstr = [](Compiler & comp, const IntermediateInstr * instr)
    {
        IntermediateInstr * newLoadInstr = newInstruction(comp, instr);
        const bool isGlobal = (instr->op == OpCode::StoreSetTypeGlobal);
        newLoadInstr->op = (isGlobal ? OpCode::LoadGlobal : OpCode::LoadLocal);
        return newLoadInstr;
    };

    auto makeCommonStoreInst = [](Compiler & comp, const IntermediateInstr * instr)
    {
        IntermediateInstr * newStoreInstr = newInstruction(comp, instr);
        newStoreInstr->op = (instr->op == OpCode::StoreSetTypeLocal ? OpCode::StoreLocal : OpCode::StoreGlobal);
        return newStoreInstr;
    };

    // ------------------------------------------

    // Must have the counter (i) and a for-loop parameter/condition.
    EXPECT_CHILD_AT_INDEX(root, 0);
    EXPECT_CHILD_AT_INDEX(root, 1);

    // The termination condition or collection to iterate over (for-loop parameter):
    IntermediateInstr * forParam = traverseTreeRecursive(compiler, codeGen, root->getChild(1));
    if (root->getChild(1)->nodeType == SyntaxTreeNode::Type::ExprFuncCall)
    {
        forParam = linkInstructions(forParam, newInstruction(compiler, OpCode::LoadRVR));
    }

    //
    // Iterator, internal index and stored parameter:
    //

    codeGen.forLoopIteratorNode = root->getChild(0);
    if (codeGen.isInsideFunctionBody())
    {
        codeGen.currentStackSize++;
    }

    // 'i' iterator:
    IntermediateInstr * forVars   = traverseTreeRecursive(compiler, codeGen, root->getChild(0));
    IntermediateInstr * storeIter = forVars;
    for (; storeIter->next != nullptr; storeIter = storeIter->next) { }
    MOON_ASSERT(storeIter->op == OpCode::StoreSetTypeLocal || storeIter->op == OpCode::StoreSetTypeGlobal);

    // The internal index '__for_idx__' is linked to the 'i' iterator:
    forVars = linkInstructions(forVars, traverseTreeRecursive(compiler, codeGen, root->getChild(0)->getChild(2)));
    IntermediateInstr * storeIdx = forVars;
    for (; storeIdx->next != nullptr; storeIdx = storeIdx->next) { }
    MOON_ASSERT(storeIdx->op == OpCode::StoreSetTypeLocal || storeIdx->op == OpCode::StoreSetTypeGlobal);

    // The copy of the loop parameter/condition '__for_prm__' is linked to '__for_idx__':
    forVars = linkInstructions(forVars, traverseTreeRecursive(compiler, codeGen, root->getChild(0)->getChild(2)->getChild(2)));
    IntermediateInstr * storeParam = forVars;
    for (; storeParam->next != nullptr; storeParam = storeParam->next) { }
    MOON_ASSERT(storeParam->op == OpCode::StoreSetTypeLocal || storeParam->op == OpCode::StoreSetTypeGlobal);

    //
    // ForLoopPrep:
    //

    // ForLoopPrep is preceded by loading the param, iterator and internal index:
    IntermediateInstr * forPrep = newInstruction(compiler, OpCode::ForLoopPrep);
    forPrep = linkInstructions(makeLoadInstr(compiler, storeParam), forPrep);
    forPrep = linkInstructions(makeLoadInstr(compiler, storeIter),  forPrep);
    forPrep = linkInstructions(makeLoadInstr(compiler, storeIdx),   forPrep);

    // After prepping, the updated values get written back to memory:
    forPrep = linkInstructions(forPrep, makeCommonStoreInst(compiler, storeIdx));
    forPrep = linkInstructions(forPrep, makeCommonStoreInst(compiler, storeIter));

    //
    // ForLoopTest:
    //

    // ForLoopTest requires loading the internal index and the loop param/termination cond:
    IntermediateInstr * forTest = newInstruction(compiler, OpCode::ForLoopTest);
    forTest = linkInstructions(makeLoadInstr(compiler, storeParam), forTest);
    forTest = linkInstructions(makeLoadInstr(compiler, storeIdx),   forTest);

    //
    // ForLoopStep:
    //

    // Similarly to ForLoopPrep, we need the param, iterator and internal index.
    IntermediateInstr * forStep = newInstruction(compiler, OpCode::ForLoopStep);
    forStep = linkInstructions(makeLoadInstr(compiler, storeParam), forStep);
    forStep = linkInstructions(makeLoadInstr(compiler, storeIter),  forStep);
    forStep = linkInstructions(makeLoadInstr(compiler, storeIdx),   forStep);

    // After stepping, the updated values get written back to memory:
    forStep = linkInstructions(forStep, makeCommonStoreInst(compiler, storeIdx));
    forStep = linkInstructions(forStep, makeCommonStoreInst(compiler, storeIter));

    //
    // Wrap it up:
    //
    IntermediateInstr * forCont = newInstruction(compiler, OpCode::Jmp, forStep);
    IntermediateInstr * forEnd  = newInstruction(compiler, OpCode::NoOp);

    // Goes after the ForLoopTest to break if the result was false.
    IntermediateInstr * jumpIfFalse = newInstruction(compiler, OpCode::JmpIfFalse, forEnd);

    // We have to jump over the first step of the loop on initialization (ForLoopPrep).
    IntermediateInstr * postPrepJump = newInstruction(compiler, OpCode::Jmp, forTest);

    // If there's any code in the loop body:
    IntermediateInstr * loopBody;
    if (root->getChild(2) != nullptr)
    {
        codeGen.loopStartAnchor = forStep; // Jump targets for 'continue/break'
        codeGen.loopEndAnchor   = forEnd;
        loopBody = traverseTreeRecursive(compiler, codeGen, root->getChild(2));
        codeGen.loopStartAnchor = nullptr;
        codeGen.loopEndAnchor   = nullptr;
    }
    else // Empty for-loop body:
    {
        loopBody = nullptr;
    }

    // Assign the '__for_prm__'s initial value from stack top.
    IntermediateInstr * forPrmInit = makeCommonStoreInst(compiler, storeParam);

    // Tie them up:
    IntermediateInstr * temp = linkInstructions(
                forVars, forParam, forPrmInit, forPrep, postPrepJump,
                forStep, forTest, jumpIfFalse, loopBody); // loopBody may be null, so must be linked at the tail.
    IntermediateInstr * forLoopInstrList = linkInstructions(temp, forCont, forEnd);

    codeGen.forLoopIteratorNode = nullptr;
    return forLoopInstrList;
}

static IntermediateInstr * emitBreak(Compiler & compiler, CodeGenContext & codeGen, const SyntaxTreeNode *)
{
    MOON_ASSERT(codeGen.loopEndAnchor != nullptr);
    return newInstruction(compiler, OpCode::Jmp, codeGen.loopEndAnchor);
}

static IntermediateInstr * emitContinue(Compiler & compiler, CodeGenContext & codeGen, const SyntaxTreeNode *)
{
    MOON_ASSERT(codeGen.loopStartAnchor != nullptr);
    return newInstruction(compiler, OpCode::Jmp, codeGen.loopStartAnchor);
}

static IntermediateInstr * emitMatchPrep(Compiler & compiler, CodeGenContext & codeGen, const SyntaxTreeNode * root)
{
    EXPECT_CHILD_AT_INDEX(root, 0); // The param to match with

    IntermediateInstr * matchParam = traverseTreeRecursive(compiler, codeGen, root->getChild(0));
    IntermediateInstr * matchEndOp = newInstruction(compiler, OpCode::MatchEnd);

    // Reusing the return anchor to mark the end of the match statement.
    codeGen.returnAnchor = matchEndOp;
    IntermediateInstr * matchBody = (root->getChild(1) ? traverseTreeRecursive(compiler, codeGen, root->getChild(1)) : nullptr);
    codeGen.returnAnchor = nullptr;

    return linkInstructions(matchParam, matchBody, matchEndOp);
}

static IntermediateInstr * emitMatchTest(Compiler & compiler, CodeGenContext & codeGen, const SyntaxTreeNode * root)
{
    MOON_ASSERT(codeGen.returnAnchor != nullptr);
    EXPECT_CHILD_AT_INDEX(root, 0); // The expression to match against.

    IntermediateInstr * caseExpr     = traverseTreeRecursive(compiler, codeGen, root->getChild(0));
    IntermediateInstr * caseBody     = (root->getChild(1) ? traverseTreeRecursive(compiler, codeGen, root->getChild(1)) : nullptr);
    IntermediateInstr * caseEnd      = newInstruction(compiler, OpCode::NoOp);
    IntermediateInstr * caseEndJump  = newInstruction(compiler, OpCode::JmpIfFalse, caseEnd);
    IntermediateInstr * matchEndJump = newInstruction(compiler, OpCode::Jmp, codeGen.returnAnchor);
    IntermediateInstr * matchTestOp  = newInstruction(compiler, OpCode::MatchTest);

    IntermediateInstr * matchStmt    = linkInstructions(caseExpr, matchTestOp, caseEndJump, caseBody, matchEndJump);
    IntermediateInstr * siblingCase  = (root->getChild(2) ? traverseTreeRecursive(compiler, codeGen, root->getChild(2)) : nullptr);

    return linkInstructions(matchStmt, caseEnd, siblingCase);
}

static IntermediateInstr * emitMatchDefault(Compiler & compiler, CodeGenContext & codeGen, const SyntaxTreeNode * root)
{
    // The default clause may be empty.
    return (root->getChild(1) ? traverseTreeRecursive(compiler, codeGen, root->getChild(1)) : newInstruction(compiler, OpCode::NoOp));
}

static IntermediateInstr * emitFunctionDecl(Compiler & compiler, CodeGenContext & codeGen, const SyntaxTreeNode * root)
{
    EXPECT_SYMBOL(root); // The function name.

    IntermediateInstr * funcStart = newInstruction(compiler, OpCode::FuncStart, root->symbol, Variant::Type::Function, InvalidStackIndex);
    IntermediateInstr * funcEnd   = newInstruction(compiler, OpCode::FuncEnd,   root->symbol, Variant::Type::Function, InvalidStackIndex);

    codeGen.beginFunction(funcEnd, root, compiler.varInfo);

    // Resolve the function body if any.
    // Parameter list and return type are not relevant here.
    IntermediateInstr * funcBody = (root->getChild(1) ?
            traverseTreeRecursive(compiler, codeGen, root->getChild(1)) :
            newInstruction(compiler, OpCode::NoOp));

    codeGen.endFunction();

    return linkInstructions(funcStart, funcBody, funcEnd);
}

static IntermediateInstr * emitReturn(Compiler & compiler, CodeGenContext & codeGen, const SyntaxTreeNode * root)
{
    MOON_ASSERT(codeGen.returnAnchor != nullptr);
    IntermediateInstr * retJump = newInstruction(compiler, OpCode::JmpReturn, codeGen.returnAnchor);

    // Return simply resolves its child expression if it has one.
    if (root->getChild(0) != nullptr)
    {
        IntermediateInstr * retExpr = traverseTreeRecursive(compiler, codeGen, root->getChild(0));

        // We only need to set the Return Value Register when the return
        // expression is not itself another call. If it is a call, the
        // leaf function in the call tree will set the register.
        if (root->getChild(0)->nodeType != SyntaxTreeNode::Type::ExprFuncCall)
        {
            IntermediateInstr * setRVR = newInstruction(compiler, OpCode::StoreRVR);
            return linkInstructions(retExpr, setRVR, retJump);
        }
        else
        {
            return linkInstructions(retExpr, retJump);
        }
    }
    else // Void return:
    {
        return retJump;
    }
}

static IntermediateInstr * emitEnumDecl(Compiler & compiler, CodeGenContext &, const SyntaxTreeNode * root)
{
    EXPECT_SYMBOL(root);

    const Symbol * enumObjNameSymbol  = demangleEnumTypeName(compiler.symTable, root->symbol);
    const Symbol * enumTypeNameSymbol = root->symbol;

    IntermediateInstr * loadTypeInsr = newInstruction(compiler, OpCode::LoadGlobal,  enumTypeNameSymbol, Variant::Type::Tid,    InvalidStackIndex);
    IntermediateInstr * initInstr    = newInstruction(compiler, OpCode::EnumDynInit, enumObjNameSymbol,  Variant::Type::Object, InvalidStackIndex);

    return linkInstructions(loadTypeInsr, initInstr);
}

static IntermediateInstr * emitNewRange(Compiler & compiler, CodeGenContext & codeGen, const SyntaxTreeNode * root)
{
    EXPECT_CHILD_AT_INDEX(root, 1); // Range start
    EXPECT_CHILD_AT_INDEX(root, 2); // Range end

    IntermediateInstr * startExpr  = traverseTreeRecursive(compiler, codeGen, root->getChild(1));
    IntermediateInstr * endExpr    = traverseTreeRecursive(compiler, codeGen, root->getChild(2));
    IntermediateInstr * newRangeOp = newInstruction(compiler, OpCode::NewRange);

    return linkInstructions(startExpr, endExpr, newRangeOp, getParamListChain(compiler, codeGen, root));
}

static IntermediateInstr * emitArraySubscript(Compiler & compiler, CodeGenContext & codeGen, const SyntaxTreeNode * root)
{
    // child 0 is reserved for parameter lists and may be null.
    EXPECT_CHILD_AT_INDEX(root, 1);
    EXPECT_CHILD_AT_INDEX(root, 2);

    // If either term is a function call, we must load from the Return Value Register.
    IntermediateInstr * arrayExpr = traverseTreeRecursive(compiler, codeGen, root->getChild(1));
    if (root->getChild(1)->nodeType == SyntaxTreeNode::Type::ExprFuncCall)
    {
        arrayExpr = linkInstructions(arrayExpr, newInstruction(compiler, OpCode::LoadRVR));
    }

    IntermediateInstr * subscriptExpr = traverseTreeRecursive(compiler, codeGen, root->getChild(2));
    if (root->getChild(2)->nodeType == SyntaxTreeNode::Type::ExprFuncCall)
    {
        subscriptExpr = linkInstructions(subscriptExpr, newInstruction(compiler, OpCode::LoadRVR));
    }

    return linkInstructions(arrayExpr, subscriptExpr, newInstruction(compiler, OpCode::ArraySubscript));
}

static IntermediateInstr * emitMemberRef(Compiler & compiler, CodeGenContext & codeGen, const SyntaxTreeNode * root)
{
    // child 0 is reserved for parameter list chains in function/constructor calls.
    EXPECT_CHILD_AT_INDEX(root, 1);
    EXPECT_CHILD_AT_INDEX(root, 2);
    EXPECT_SYMBOL(root->getChild(2));

    IntermediateInstr * objLoadInstr = traverseTreeRecursive(compiler, codeGen, root->getChild(1));
    const Symbol * objSymbol    = root->getChildSymbol(1);
    const Symbol * memberSymbol = root->getChildSymbol(2);

    IntermediateInstr * loadMemOffsInstr;
    if (objSymbol != nullptr) // First pass
    {
        int memberOffset = 0;
        loadMemOffsInstr = makeLoadMemberOffsetInstr(compiler, codeGen, objSymbol, memberSymbol,
                                                     &codeGen.lastMemberTypeId, &memberOffset);

        const Object::Member & member = codeGen.lastMemberTypeId->templateObject->getMemberAt(memberOffset);
        if (member.data.type == Variant::Type::Object)
        {
            codeGen.lastMemberTypeId = member.data.value.asObject->getTypeId();
        }
    }
    else // Going down a chain of obj.member.member.member ...
    {
        MOON_ASSERT(codeGen.lastMemberTypeId != nullptr);
        const int memberOffset = codeGen.lastMemberTypeId->templateObject->findMemberIndex(memberSymbol->name);

        if (memberOffset < 0)
        {
            MOON_INTERNAL_EXCEPTION("sub object of type '" + toString(codeGen.lastMemberTypeId->name) +
                                    "' has no member '" + toString(memberSymbol->name) + "'");
        }
        loadMemOffsInstr = makeLoadIntLiteral(compiler, memberOffset);

        const Object::Member & member = codeGen.lastMemberTypeId->templateObject->getMemberAt(memberOffset);
        if (member.data.type == Variant::Type::Object)
        {
            codeGen.lastMemberTypeId = member.data.value.asObject->getTypeId();
        }
    }

    return linkInstructions(objLoadInstr, loadMemOffsInstr,
                            newInstruction(compiler, OpCode::MemberRef),
                            getParamListChain(compiler, codeGen, root));
}

static IntermediateInstr * emitNewVar(Compiler & compiler, CodeGenContext & codeGen, const SyntaxTreeNode * root)
{
    EXPECT_SYMBOL(root); // The var name.

    const Symbol * typeSymbol    = nullptr;
    IntermediateInstr * argument = nullptr;

    if (root->getChild(1) != nullptr) // Type node
    {
        // We just want 'object' for the NEW_VAR instruction.
        // If it is a dynamic object, the NEW_OBJ instr handles the type.
        if (root->getChild(1)->evalType == SyntaxTreeNode::Eval::UDT)
        {
            typeSymbol = compiler.symTable.findSymbol("object");
        }
        else
        {
            typeSymbol = root->getChild(1)->symbol;
        }
    }

    if (typeSymbol == nullptr)
    {
        typeSymbol = eval2Symbol(compiler.symTable, root->evalType);
    }

    int           argumentCount = 0;
    bool          symbolIsFuncLocal;
    UInt16        stackIndex;
    Variant::Type symbolType;

    if (root->getChild(0) != nullptr) // Initializer expression node
    {
        argument = traverseTreeRecursive(compiler, codeGen, root->getChild(0));
        argument = appendTrailingLoadOpIfNeeded(compiler, argument, root->getChild(0));
        argumentCount = 1;
    }

    findSymbolInfo(compiler, codeGen, root->symbol, &symbolType, &stackIndex, &symbolIsFuncLocal);
    const OpCode op = (symbolIsFuncLocal ? OpCode::StoreSetTypeLocal : OpCode::StoreSetTypeGlobal);

    IntermediateInstr * newOp   = newInstruction(compiler, OpCode::NewVar, typeSymbol, Variant::Type::Tid, InvalidStackIndex);
    IntermediateInstr * storeOp = newInstruction(compiler, op, root->symbol, symbolType, stackIndex);

    // A load instruction is appended to indicate if we should pop one value
    // from the stack to initialize the new var or if it was left uninitialized.
    IntermediateInstr * loadArgCountInstr = makeLoadIntLiteral(compiler, argumentCount);

    if (argument != nullptr)
    {
        // Storing from the return value of a function references the Return Value Register (RVR).
        if (root->getChild(0)->nodeType == SyntaxTreeNode::Type::ExprFuncCall)
        {
            argument = linkInstructions(argument, newInstruction(compiler, OpCode::LoadRVR));
        }

        return linkInstructions(argument, loadArgCountInstr, newOp, storeOp);
    }
    else
    {
        return linkInstructions(loadArgCountInstr, newOp, storeOp);
    }
}

static IntermediateInstr * emitLoad(Compiler & compiler, CodeGenContext & codeGen, const SyntaxTreeNode * root)
{
    EXPECT_SYMBOL(root);
    const Symbol * symbol = root->symbol;

    OpCode op = OpCode::LoadGlobal;
    UInt16 stackIndex = InvalidStackIndex;

    // Booleans are converted to integer (0=false, 1=true).
    if (symbol->type == Symbol::Type::BoolLiteral)
    {
        symbol = compiler.symTable.findOrDefineIntLiteral(symbol->value.asBoolean);
    }
    else if (symbol->type == Symbol::Type::Identifier)
    {
        bool symbolIsFuncLocal;
        Variant::Type symbolType;
        findSymbolInfo(compiler, codeGen, symbol, &symbolType, &stackIndex, &symbolIsFuncLocal);

        // A local variable or function parameter?
        if (symbolIsFuncLocal)
        {
            op = OpCode::LoadLocal;
        }
    }

    IntermediateInstr * loadInstr = newInstruction(compiler, op, symbol, eval2VarType(root->evalType), stackIndex);

    // Function calls consist of a chain of load instructions followed by one CALL instruction.
    if (root->getChild(0) != nullptr)
    {
        IntermediateInstr * argList = traverseTreeRecursive(compiler, codeGen, root->getChild(0));
        return linkInstructions(loadInstr, argList); // <-- NOTE: Change this to alter func parameter passing order!
    }
    else
    {
        return loadInstr;
    }
}

static IntermediateInstr * memberArrayStore(Compiler & compiler, CodeGenContext & codeGen,
                                            const SyntaxTreeNode * firstMemberRefNode,
                                            const SyntaxTreeNode * arraySubNode,
                                            const SyntaxTreeNode * assignmentNode)
{
    MOON_ASSERT(firstMemberRefNode != nullptr);
    EXPECT_CHILD_AT_INDEX(arraySubNode,   2);
    EXPECT_CHILD_AT_INDEX(assignmentNode, 2);

    IntermediateInstr * memberRefInstrList = traverseTreeRecursive(compiler, codeGen, firstMemberRefNode);
    IntermediateInstr * assignValLoadInstr = traverseTreeRecursive(compiler, codeGen, assignmentNode->getChild(2));
    IntermediateInstr * arraySubLoadInstr  = traverseTreeRecursive(compiler, codeGen, arraySubNode->getChild(2));

    // If any of the terms is a function call, we need the return value.
    if (assignmentNode->getChild(2)->nodeType == SyntaxTreeNode::Type::ExprFuncCall)
    {
        assignValLoadInstr = linkInstructions(assignValLoadInstr, newInstruction(compiler, OpCode::LoadRVR));
    }

    if (arraySubNode->getChild(2)->nodeType == SyntaxTreeNode::Type::ExprFuncCall)
    {
        arraySubLoadInstr = linkInstructions(arraySubLoadInstr, newInstruction(compiler, OpCode::LoadRVR));
    }

    // Stack:
    //  Object/member
    //  Assign value
    //  Array subscript index
    return linkInstructions(memberRefInstrList, assignValLoadInstr, arraySubLoadInstr, newInstruction(compiler, OpCode::StoreArraySubStk));
}

static IntermediateInstr * setUpMemberOffsetLoadChain(Compiler & compiler, CodeGenContext & codeGen, const Symbol * objSymbol)
{
    MOON_ASSERT(objSymbol != nullptr);

    int lastMemberOffset = 0;
    const TypeId * lastMemberTypeId = nullptr;
    IntermediateInstr * memOffsetInstr = makeLoadMemberOffsetInstr(compiler, codeGen, objSymbol,
                                codeGen.memberRefList[1], &lastMemberTypeId, &lastMemberOffset);

    if (codeGen.memberRefList.size() > 2)
    {
        const Object::Member & member = lastMemberTypeId->templateObject->getMemberAt(lastMemberOffset);
        if (member.data.type == Variant::Type::Object)
        {
            lastMemberTypeId = member.data.value.asObject->getTypeId();
        }

        // [0:1] is the first obj.member pair. A chain might follow.
        const std::size_t numRefs = codeGen.memberRefList.size();
        for (std::size_t i = 2; i < numRefs; ++i)
        {
            MOON_ASSERT(lastMemberTypeId != nullptr);
            const Symbol * memberSymbol = codeGen.memberRefList[i];

            lastMemberOffset = lastMemberTypeId->templateObject->findMemberIndex(memberSymbol->name);
            if (lastMemberOffset < 0)
            {
                MOON_INTERNAL_EXCEPTION("sub object of type '" + toString(lastMemberTypeId->name) +
                                        "' has no member '" + toString(memberSymbol->name) + "'");
            }

            linkInstructions(memOffsetInstr, makeLoadIntLiteral(compiler, lastMemberOffset));

            const Object::Member & subMember = lastMemberTypeId->templateObject->getMemberAt(lastMemberOffset);
            if (subMember.data.type == Variant::Type::Object)
            {
                lastMemberTypeId = subMember.data.value.asObject->getTypeId();
            }
        }
    }

    return memOffsetInstr;
}

static void collectMemberRefsRecursive(CodeGenContext & codeGen, const SyntaxTreeNode * root)
{
    if (root == nullptr)
    {
        return;
    }

    collectMemberRefsRecursive(codeGen, root->getChild(1));
    collectMemberRefsRecursive(codeGen, root->getChild(2));

    if (root->symbol != nullptr)
    {
        codeGen.memberRefList.push_back(root->symbol);
    }
}

static IntermediateInstr * emitStore(Compiler & compiler, CodeGenContext & codeGen, const SyntaxTreeNode * root)
{
    EXPECT_CHILD_AT_INDEX(root, 1);
    EXPECT_CHILD_AT_INDEX(root, 2);

    int numMemberOffsets     = 0;
    const Symbol * objSymbol = nullptr;
    bool isArraySubscript    = false;

    if (root->getChild(1)->nodeType == SyntaxTreeNode::Type::ExprMemberRef)
    {
        codeGen.memberRefList.clear();
        collectMemberRefsRecursive(codeGen, root->getChild(1));
        MOON_ASSERT(codeGen.memberRefList.size() >= 2); // obj.member pair at least

        objSymbol = codeGen.memberRefList[0];
        numMemberOffsets = static_cast<int>(codeGen.memberRefList.size() - 1);
    }
    else if (root->getChild(1)->nodeType == SyntaxTreeNode::Type::ExprArraySubscript)
    {
        const SyntaxTreeNode * child11 = root->getChild(1)->getChild(1);
        MOON_ASSERT(child11 != nullptr);

        // Writing to a member array (obj.member[N] = ...)
        if (child11->nodeType == SyntaxTreeNode::Type::ExprMemberRef)
        {
            return memberArrayStore(compiler, codeGen, child11, root->getChild(1), root);
        }
        else
        {
            objSymbol = child11->symbol;
        }

        isArraySubscript = true;
    }
    else // Simple variable assignment
    {
        objSymbol = root->getChildSymbol(1);
    }

    OpCode        op;
    UInt16        stackIndex;
    Variant::Type symbolType;
    bool          symbolIsFuncLocal;

    MOON_ASSERT(objSymbol != nullptr);
    findSymbolInfo(compiler, codeGen, objSymbol, &symbolType, &stackIndex, &symbolIsFuncLocal);

    IntermediateInstr * memOffsetInstr = nullptr;
    IntermediateInstr * argument = traverseTreeRecursive(compiler, codeGen, root->getChild(2));

    // We might need to load the rhs argument back up if this store
    // is part of a fancy composite operation like "c = (a += b)".
    argument = appendTrailingLoadOpIfNeeded(compiler, argument, root->getChild(2));

    if (symbolIsFuncLocal) // A local variable or function parameter?
    {
        if (numMemberOffsets > 0)
        {
            memOffsetInstr = setUpMemberOffsetLoadChain(compiler, codeGen, objSymbol);
            op = OpCode::MemberStoreLocal;
        }
        else if (isArraySubscript)
        {
            op = OpCode::StoreArraySubLocal;
        }
        else
        {
            op = OpCode::StoreLocal;
        }
    }
    else // A global symbol
    {
        if (numMemberOffsets > 0)
        {
            memOffsetInstr = setUpMemberOffsetLoadChain(compiler, codeGen, objSymbol);
            op = OpCode::MemberStoreGlobal;
        }
        else if (isArraySubscript)
        {
            op = OpCode::StoreArraySubGlobal;
        }
        else
        {
            op = OpCode::StoreGlobal;
        }
    }

    // Load the number of member offsets to access the sub-object (if according object members):
    if (numMemberOffsets > 0)
    {
        MOON_ASSERT(memOffsetInstr != nullptr);
        memOffsetInstr = linkInstructions(memOffsetInstr, makeLoadIntLiteral(compiler, numMemberOffsets));
    }

    IntermediateInstr * storeInstr = newInstruction(compiler, op, objSymbol, symbolType, stackIndex);

    // Load the subscript right after if doing an array:
    if (isArraySubscript)
    {
        IntermediateInstr * subscriptInstr = traverseTreeRecursive(compiler, codeGen, root->getChild(1)->getChild(2));
        if (root->getChild(1)->getChild(2)->nodeType == SyntaxTreeNode::Type::ExprFuncCall)
        {
            // Subscript from function return value.
            IntermediateInstr * getRVR = newInstruction(compiler, OpCode::LoadRVR);
            subscriptInstr = linkInstructions(subscriptInstr, getRVR);
        }
        storeInstr = linkInstructions(subscriptInstr, storeInstr);
    }

    // Storing from the return value of a function references the Return Value Register (RVR).
    if (root->getChild(2)->nodeType == SyntaxTreeNode::Type::ExprFuncCall)
    {
        IntermediateInstr * getRVR = newInstruction(compiler, OpCode::LoadRVR);
        return linkInstructions(linkInstructions(argument, memOffsetInstr), getRVR, storeInstr);
    }
    else // Normal store:
    {
        return linkInstructions(linkInstructions(argument, memOffsetInstr), storeInstr);
    }
}

static IntermediateInstr * emitTypecast(Compiler & compiler, CodeGenContext & codeGen, const SyntaxTreeNode * root)
{
    EXPECT_CHILD_AT_INDEX(root, 1);
    EXPECT_CHILD_AT_INDEX(root, 2);

    // Second child must be a type identifier
    EXPECT_SYMBOL(root->getChild(2));
    IntermediateInstr * loadTypeIdOp = newInstruction(compiler, OpCode::LoadGlobal, root->getChild(2)->symbol,
                                                      Variant::Type::Tid, InvalidStackIndex);

    IntermediateInstr * argument   = traverseTreeRecursive(compiler, codeGen, root->getChild(1));
    IntermediateInstr * typecastOp = newInstruction(compiler, OpCode::Typecast);

    return linkInstructions(argument, loadTypeIdOp, typecastOp, getParamListChain(compiler, codeGen, root));
}

static IntermediateInstr * emitTypeof(Compiler & compiler, CodeGenContext & codeGen, const SyntaxTreeNode * root)
{
    EXPECT_CHILD_AT_INDEX(root, 1);
    const SyntaxTreeNode * child1 = root->getChild(1);

    const bool nodeIsTypeId  = (child1->nodeType == SyntaxTreeNode::Type::ExprTypeIdent);
    const bool nodeIsLiteral = (child1->nodeType == SyntaxTreeNode::Type::ExprLiteralConst);
    const bool symbIsTypeId  = (child1->symbol && findTypeNameTypeId(codeGen, child1->symbol));
    const bool isNullLiteral = (nodeIsLiteral && child1->evalType == SyntaxTreeNode::Eval::Null);

    // Simple case of 'type_of(TYPE_NAME)/type_of(123)' => just load the TypeId directly.
    // Note that 'null' is a special case we can't just load the TypeId directly.
    // 'null' behaves like an identifier and a literal constant at the same time, so
    // the easiest way to handle 'type_of(null)' is to just do the runtime resolution.
    IntermediateInstr * typeofInstr;
    if (!isNullLiteral && (nodeIsTypeId || nodeIsLiteral || symbIsTypeId))
    {
        const Symbol * typeSymbol = (nodeIsLiteral ? eval2Symbol(compiler.symTable, child1->evalType) : child1->symbol);
        typeofInstr = newInstruction(compiler, OpCode::LoadGlobal, typeSymbol, Variant::Type::Tid, InvalidStackIndex);
    }
    else // Resolve the argument expression first and emit a Typeof opcode:
    {
        IntermediateInstr * argument = traverseTreeRecursive(compiler, codeGen, child1);

        // If either term is a function call, we must load from the Return Value Register.
        if (child1->nodeType == SyntaxTreeNode::Type::ExprFuncCall)
        {
            argument = linkInstructions(argument, newInstruction(compiler, OpCode::LoadRVR));
        }
        typeofInstr = linkInstructions(argument, newInstruction(compiler, OpCode::Typeof));
    }

    return linkInstructions(typeofInstr, getParamListChain(compiler, codeGen, root));
}

static void countArgsRecursive(CodeGenContext & codeGen, const SyntaxTreeNode * root, int * outArgCount)
{
    if (root == nullptr)
    {
        return;
    }

    (*outArgCount)++;
    codeGen.visitedNodes.push_back(root);

    if (!codeGen.nodeWasVisited(root->getChild(0)))
    {
        countArgsRecursive(codeGen, root->getChild(0), outArgCount);
    }
}

template<OpCode OP, Variant::Type OperandType>
static IntermediateInstr * emitParamChain(Compiler & compiler, CodeGenContext & codeGen, const SyntaxTreeNode * root)
{
    // This function is used to resolve parameter chains for function calls,
    // constructor calls and array literals. The OP template parameter is the
    // instruction ending the list (i.e.: a Call, NewObj, NewArray, etc).

    OpCode op = OP;
    UInt16 stackIndex = InvalidStackIndex;

    // Check for the case where attempting an indirect function call
    // via a function parameter or local variable of type 'function'.
    if (op == OpCode::Call)
    {
        EXPECT_SYMBOL(root);

        bool symbolIsFuncLocal;
        Variant::Type symbolType;
        findSymbolInfo(compiler, codeGen, root->symbol, &symbolType, &stackIndex, &symbolIsFuncLocal);

        if (symbolIsFuncLocal)
        {
            op = OpCode::CallLocal;
        }
    }

    IntermediateInstr * headInstr = newInstruction(compiler, op, root->symbol, OperandType, stackIndex);

    // We have to keep track of the visited nodes to be able to properly
    // compute the argument counts. Some nodes will be visited more than
    // once due to the parameter list chains.
    codeGen.visitedNodes.clear();

    int argumentCount = 0;
    IntermediateInstr * myArgs = nullptr;
    IntermediateInstr * argListChain = nullptr;

    // We might be linked to an parameter list chain, so go down the child 0 hierarchy:
    if (root->getChild(0) != nullptr)
    {
        argListChain = traverseTreeRecursive(compiler, codeGen, root->getChild(0));
    }

    // Child 1 has the argument list of this function:
    if (root->getChild(1) != nullptr)
    {
        myArgs = traverseTreeRecursive(compiler, codeGen, root->getChild(1));
        countArgsRecursive(codeGen, root->getChild(1), &argumentCount);
    }

    // Argument count is appended as a literal integer value:
    IntermediateInstr * loadArgCountOp = makeLoadIntLiteral(compiler, argumentCount);

    // Wrap it up!
    if (myArgs != nullptr)
    {
        return linkInstructions(myArgs, loadArgCountOp, headInstr, argListChain);
    }
    else // No args for this function/constructor:
    {
        return linkInstructions(loadArgCountOp, headInstr, argListChain);
    }
}

template<OpCode OP>
static IntermediateInstr * emitUnaryOp(Compiler & compiler, CodeGenContext & codeGen, const SyntaxTreeNode * root)
{
    // child 0 is reserved for parameter list chains in function/constructor calls.
    EXPECT_CHILD_AT_INDEX(root, 1);
    IntermediateInstr * arg = traverseTreeRecursive(compiler, codeGen, root->getChild(1));

    // Function calls must load the Return Value Register.
    if (root->getChild(1)->nodeType == SyntaxTreeNode::Type::ExprFuncCall)
    {
        arg = linkInstructions(arg, newInstruction(compiler, OpCode::LoadRVR));
    }

    IntermediateInstr * unaryOp = newInstruction(compiler, OP);
    return linkInstructions(arg, unaryOp, getParamListChain(compiler, codeGen, root));
}

template<OpCode OP>
static IntermediateInstr * emitBinaryOp(Compiler & compiler, CodeGenContext & codeGen, const SyntaxTreeNode * root)
{
    // child 0 is reserved for parameter list chains in function/constructor calls.
    EXPECT_CHILD_AT_INDEX(root, 1);
    EXPECT_CHILD_AT_INDEX(root, 2);
    IntermediateInstr * lhs = traverseTreeRecursive(compiler, codeGen, root->getChild(1));
    IntermediateInstr * rhs = traverseTreeRecursive(compiler, codeGen, root->getChild(2));

    // If either term is a function call, we must load from the Return Value Register.
    if (root->getChild(1)->nodeType == SyntaxTreeNode::Type::ExprFuncCall)
    {
        lhs = linkInstructions(lhs, newInstruction(compiler, OpCode::LoadRVR));
    }

    if (root->getChild(2)->nodeType == SyntaxTreeNode::Type::ExprFuncCall)
    {
        rhs = linkInstructions(rhs, newInstruction(compiler, OpCode::LoadRVR));
    }

    IntermediateInstr * binOp = newInstruction(compiler, OP);
    return linkInstructions(lhs, rhs, binOp, getParamListChain(compiler, codeGen, root));
}

template<OpCode OP>
static IntermediateInstr * emitCompoundBinaryOp(Compiler & compiler, CodeGenContext & codeGen, const SyntaxTreeNode * root)
{
    // child 0 is reserved for parameter list chains in function/constructor calls.
    EXPECT_CHILD_AT_INDEX(root, 1);
    EXPECT_CHILD_AT_INDEX(root, 2);

    const Symbol * lhsSymbol;
    bool           lhsIsArraySubscript;
    bool           symbolIsFuncLocal;
    OpCode         storeOp;
    OpCode         loadOp;
    UInt16         stackIndex;
    Variant::Type  symbolType;
    int            numMemberOffsets = 0;

    if (root->getChild(1)->nodeType == SyntaxTreeNode::Type::ExprMemberRef)
    {
        codeGen.memberRefList.clear();
        collectMemberRefsRecursive(codeGen, root->getChild(1));
        MOON_ASSERT(codeGen.memberRefList.size() >= 2); // obj.member pair at least

        lhsSymbol = codeGen.memberRefList[0];
        numMemberOffsets = static_cast<int>(codeGen.memberRefList.size() - 1);
        lhsIsArraySubscript = false;
    }
    else if (root->getChild(1)->nodeType == SyntaxTreeNode::Type::ExprArraySubscript)
    {
        lhsSymbol = root->getChild(1)->getChildSymbol(1);
        lhsIsArraySubscript = true;
    }
    else
    {
        EXPECT_SYMBOL(root->getChild(1));
        lhsSymbol = root->getChild(1)->symbol;
        lhsIsArraySubscript = false;
    }

    findSymbolInfo(compiler, codeGen, lhsSymbol, &symbolType, &stackIndex, &symbolIsFuncLocal);

    IntermediateInstr * memOffsetInstr = nullptr;
    if (symbolIsFuncLocal)
    {
        if (numMemberOffsets > 0)
        {
            memOffsetInstr = setUpMemberOffsetLoadChain(compiler, codeGen, lhsSymbol);
            storeOp = OpCode::MemberStoreLocal;
        }
        else if (lhsIsArraySubscript)
        {
            storeOp = OpCode::StoreArraySubLocal;
        }
        else
        {
            storeOp = OpCode::StoreLocal;
        }
        loadOp = OpCode::LoadLocal;
    }
    else
    {
        if (numMemberOffsets > 0)
        {
            memOffsetInstr = setUpMemberOffsetLoadChain(compiler, codeGen, lhsSymbol);
            storeOp = OpCode::MemberStoreGlobal;
        }
        else if (lhsIsArraySubscript)
        {
            storeOp = OpCode::StoreArraySubGlobal;
        }
        else
        {
            storeOp = OpCode::StoreGlobal;
        }
        loadOp = OpCode::LoadGlobal;
    }

    IntermediateInstr * binOpInstr = newInstruction(compiler, OP);
    IntermediateInstr * lhs = traverseTreeRecursive(compiler, codeGen, root->getChild(1));
    IntermediateInstr * rhs = traverseTreeRecursive(compiler, codeGen, root->getChild(2));

    // If either term is a function call, we must load from the Return Value Register.
    if (root->getChild(1)->nodeType == SyntaxTreeNode::Type::ExprFuncCall)
    {
        lhs = linkInstructions(lhs, newInstruction(compiler, OpCode::LoadRVR));
    }

    if (root->getChild(2)->nodeType == SyntaxTreeNode::Type::ExprFuncCall)
    {
        rhs = linkInstructions(rhs, newInstruction(compiler, OpCode::LoadRVR));
    }

    if (root->getChild(1)->nodeType != SyntaxTreeNode::Type::ExprFuncCall)
    {
        // Load the number of member offsets to access the sub-object (if according object members):
        if (numMemberOffsets > 0)
        {
            MOON_ASSERT(memOffsetInstr != nullptr);
            memOffsetInstr = linkInstructions(memOffsetInstr, makeLoadIntLiteral(compiler, numMemberOffsets));
        }

        IntermediateInstr * storeOpInstr = newInstruction(compiler, storeOp, lhsSymbol, symbolType, stackIndex);

        // Load the subscript index right after if the left hand side is an array subscript.
        if (lhsIsArraySubscript)
        {
            IntermediateInstr * subscriptInstr = traverseTreeRecursive(compiler, codeGen, root->getChild(1)->getChild(2));
            if (root->getChild(1)->getChild(2)->nodeType == SyntaxTreeNode::Type::ExprFuncCall)
            {
                // Subscript from function return value.
                IntermediateInstr * getRVR = newInstruction(compiler, OpCode::LoadRVR);
                subscriptInstr = linkInstructions(subscriptInstr, getRVR);
            }
            storeOpInstr = linkInstructions(subscriptInstr, storeOpInstr);
        }

        IntermediateInstr * paramList = getParamListChain(compiler, codeGen, root);
        if (paramList != nullptr)
        {
            // If the expression is being used on a parameter list we need to
            // load the symbol back into the stack after the write to memory.
            paramList = linkInstructions(newInstruction(compiler, loadOp, lhsSymbol, symbolType, stackIndex), paramList);
        }

        return linkInstructions(lhs, rhs, linkInstructions(binOpInstr, memOffsetInstr), storeOpInstr, paramList);
    }
    else
    {
        // Silly "func() += xyz" kind of code, but we need to handle it nonetheless.
        // Just push the result back into the stack and be done with it.
        return linkInstructions(lhs, rhs, binOpInstr, getParamListChain(compiler, codeGen, root));
    }
}

// ----------------------------------------------------------------------------
// emitInstrCallbacks[]:
//
// The handlers for each SyntaxTreeNode::Type (AKA STNode).
// Some will emit instructions and possibly recurse by calling
// traverseTreeRecursive() again with the child nodes of the subtree.
// ----------------------------------------------------------------------------
static const EmitInstrForNodeCB emitInstrCallbacks[]
{
    &emitNOOP,                                              // NoOp
    &emitProgStart,                                         // TranslationUnit
    &emitStatement,                                         // Statement
    &emitIfThen,                                            // IfThenStatement
    &emitIfThenElse,                                        // IfThenElseStatement
    &emitIfThenElseIf,                                      // IfThenElseIfStatement
    &emitLoop,                                              // LoopStatement
    &emitWhileLoop,                                         // WhileStatement
    &emitForLoop,                                           // ForStatement
    &emitMatchPrep,                                         // MatchStatement
    &emitMatchTest,                                         // MatchCaseStatement
    &emitMatchDefault,                                      // MatchDefaultStatement
    &emitFunctionDecl,                                      // FuncDeclStatement
    &emitEnumDecl,                                          // EnumDeclStatement
    &emitNOOP,                                              // StructDeclStatement
    &emitNOOP,                                              // TypeAliasDeclStatement
    &emitNewVar,                                            // VarDeclStatement
    &emitReturn,                                            // ReturnStatement
    &emitBreak,                                             // BreakStatement
    &emitContinue,                                          // ContinueStatement
    &emitNewRange,                                          // ExprRange
    &emitParamChain<OpCode::NewArray, Variant::Type::Tid>,  // ExprArrayLiteral
    &emitArraySubscript,                                    // ExprArraySubscript
    &emitParamChain<OpCode::Call, Variant::Type::Function>, // ExprFuncCall
    &emitMemberRef,                                         // ExprMemberRef
    &emitLoad,                                              // ExprNameIdent
    &emitNOOP,                                              // ExprTypeIdent
    &emitLoad,                                              // ExprLiteralConst
    &emitParamChain<OpCode::NewObj, Variant::Type::Tid>,    // ExprObjectConstructor
    &emitTypecast,                                          // ExprTypecast
    &emitTypeof,                                            // ExprTypeof
    &emitStore,                                             // ExprAssign
    &emitBinaryOp<OpCode::CmpNotEqual>,                     // ExprCmpNotEqual
    &emitBinaryOp<OpCode::CmpEqual>,                        // ExprCmpEqual
    &emitBinaryOp<OpCode::CmpGreaterEqual>,                 // ExprCmpGreaterEqual
    &emitBinaryOp<OpCode::CmpGreater>,                      // ExprCmpGreaterThan
    &emitBinaryOp<OpCode::CmpLessEqual>,                    // ExprCmpLessEqual
    &emitBinaryOp<OpCode::CmpLess>,                         // ExprCmpLessThan
    &emitBinaryOp<OpCode::LogicOr>,                         // ExprLogicOr
    &emitBinaryOp<OpCode::LogicAnd>,                        // ExprLogicAnd
    &emitUnaryOp<OpCode::LogicNot>,                         // ExprLogicNot
    &emitBinaryOp<OpCode::Sub>,                             // ExprSubtract
    &emitBinaryOp<OpCode::Add>,                             // ExprAdd
    &emitBinaryOp<OpCode::Mod>,                             // ExprModulo
    &emitBinaryOp<OpCode::Div>,                             // ExprDivide
    &emitBinaryOp<OpCode::Mul>,                             // ExprMultiply
    &emitCompoundBinaryOp<OpCode::Sub>,                     // ExprSubAssign
    &emitCompoundBinaryOp<OpCode::Add>,                     // ExprAddAssign
    &emitCompoundBinaryOp<OpCode::Mod>,                     // ExprModAssign
    &emitCompoundBinaryOp<OpCode::Div>,                     // ExprDivAssign
    &emitCompoundBinaryOp<OpCode::Mul>,                     // ExprMulAssign
    &emitUnaryOp<OpCode::Negate>,                           // ExprUnaryMinus
    &emitUnaryOp<OpCode::Plus>                              // ExprUnaryPlus
};
static_assert(arrayLength(emitInstrCallbacks) == UInt32(SyntaxTreeNode::Type::Count),
              "Keep this array in sync with the enum declaration!");

// Calls the appropriate handler according to the node type (which in turn might call this function again).
static IntermediateInstr * traverseTreeRecursive(Compiler & compiler, CodeGenContext & codeGen, const SyntaxTreeNode * root)
{
    MOON_ASSERT(root != nullptr);
    const auto handlerIndex  = static_cast<UInt32>(root->nodeType);
    MOON_ASSERT(handlerIndex < static_cast<UInt32>(SyntaxTreeNode::Type::Count));
    return emitInstrCallbacks[handlerIndex](compiler, codeGen, root);
}

// ========================================================
// VM Bytecode output:
// ========================================================

static UInt32 getProgCodeIndex(const InstrMap & instrMapping, const IntermediateInstr * const instr)
{
    MOON_ASSERT(instr != nullptr);

    auto && entry = instrMapping.find(instr);
    if (entry == std::end(instrMapping))
    {
        MOON_INTERNAL_EXCEPTION("instruction " + toString(instr->op) + " not found in InstructionMap");
    }
    return entry->second;
}

static UInt32 getProgDataIndex(const DataMap & dataMapping, const Symbol * const symbol)
{
    MOON_ASSERT(symbol != nullptr);

    auto && entry = dataMapping.find(symbol);
    if (entry == std::end(dataMapping))
    {
        MOON_INTERNAL_EXCEPTION("symbol '" + toString(*symbol) + "' not found in DataMap");
    }
    return entry->second;
}

static UInt32 addSymbolToProgData(VM & vm, const Symbol * const symbol, const Variant::Type type)
{
    Variant var{}; // Default initialized to null.

    // Identifies might be variables, function or user defined types.
    if (symbol->type == Symbol::Type::Identifier)
    {
        var.type = type;

        // Some of the types require special handing:
        if (type == Variant::Type::Function)
        {
            var.value.asFunction = vm.functions.findFunction(symbol->name);
        }
        else if (type == Variant::Type::Tid)
        {
            var.value.asTypeId = vm.types.findTypeId(symbol->name);
        }
    }
    else // Literal constants/strings:
    {
        var = symbol2Variant(vm, *symbol);
    }

    vm.data.push_back(var);
    const auto varIndex = static_cast<UInt32>(vm.data.size() - 1);

    #if MOON_GLOBALS_TABLE
    if (symbol->type == Symbol::Type::Identifier && !symbol->isBuiltInTypeId())
    {
        vm.globals.addGlobal(symbol->name, varIndex);
    }
    #endif // MOON_GLOBALS_TABLE

    return varIndex;
}

static void createMappings(Compiler & compiler, CodeGenContext & codeGen, VM & vm,
                           IntermediateInstr * listHead, const bool skipFunctions)
{
    IntermediateInstr * instr        = listHead;
    IntermediateInstr * prevInstr    = listHead;
    IntermediateInstr * funcListTail = nullptr;

    while (instr != nullptr)
    {
        // We skip the functions in this pass.
        // They get processed on the next one.
        if (skipFunctions && instr->op == OpCode::FuncStart)
        {
            if (funcListTail == nullptr)
            {
                compiler.funcCodeListHead = funcListTail = instr;
            }
            else
            {
                funcListTail->next = instr;
            }

            for (; instr != nullptr && instr->op != OpCode::FuncEnd; instr = instr->next) { }
            if (instr == nullptr)
            {
                break;
            }

            // Remove from main instruction chain and link to the functions chain:
            funcListTail       = instr;
            instr              = instr->next;
            prevInstr->next    = instr;
            funcListTail->next = nullptr;
            continue;
        }

        // On our setup, noops are just placeholders for
        // the jump targets. They can be eliminated now.
        if (instr->op != OpCode::NoOp)
        {
            // References any global program data? (Jumps reference other instructions)
            if (instr->operand.symbol != nullptr && instr->stackIndex == InvalidStackIndex && !isJumpOpCode(instr->op))
            {
                // Each symbol is added once.
                if (codeGen.dataMapping.find(instr->operand.symbol) == codeGen.dataMapping.end())
                {
                    const auto index = addSymbolToProgData(vm, instr->operand.symbol, instr->dataType);
                    codeGen.dataMapping.emplace(instr->operand.symbol, index);
                }
            }

            // Operand index is set later. For now this instruction is just a placeholder.
            vm.code.push_back(packInstruction(instr->op, 0));
            codeGen.instrMapping.emplace(instr, vm.code.size() - 1);
        }
        else
        {
            // The noop doesn't get inserted, so this instruction maps to the
            // index of the next instruction to be added to the vm.code vector.
            codeGen.instrMapping.emplace(instr, vm.code.size());
        }

        // Advance to the next on list:
        prevInstr = instr;
        instr = instr->next;
    }
}

static void fixReferences(const IntermediateInstr * const instr, const CodeGenContext & codeGen,
                          CodeVector & progCode, FunctionTable & funcTable)
{
    MOON_ASSERT(instr != nullptr);
    switch (instr->op)
    {
    // Function declarations:
    case OpCode::FuncStart :
    case OpCode::FuncEnd :
        {
            // Index of this instruction and its data operand (the function object in funcTable):
            const UInt32 selfCodeIdx = getProgCodeIndex(codeGen.instrMapping, instr);
            const UInt32 operandDataIndex = getProgDataIndex(codeGen.dataMapping, instr->operand.symbol);
            MOON_ASSERT(selfCodeIdx < progCode.size() && "Index out-of-bounds!");

            const auto funcName = instr->operand.symbol->name;
            const auto funcObj  = funcTable.findFunction(funcName);

            if (funcObj == nullptr)
            {
                MOON_INTERNAL_EXCEPTION("missing function table entry for '" + toString(funcName) + "'");
            }
            if (instr->op == OpCode::FuncStart)
            {
                funcTable.setJumpTargetFor(funcName, selfCodeIdx);
            }
            progCode[selfCodeIdx] = packInstruction(instr->op, operandDataIndex);
            break;
        }

    // Instructions that reference a code address:
    case OpCode::Jmp :
    case OpCode::JmpReturn :
    case OpCode::JmpIfTrue :
    case OpCode::JmpIfFalse :
        {
            // Index of this instruction and its jump target:
            const UInt32 selfCodeIdx = getProgCodeIndex(codeGen.instrMapping, instr);
            UInt32 operandCodeIdx = getProgCodeIndex(codeGen.instrMapping, instr->operand.jumpTarget);

            // Clamp if this instruction jumps to the end of the program.
            // (happens to removed noops that pointed to the end of a block/if-else-statement).
            operandCodeIdx = std::min(operandCodeIdx, static_cast<UInt32>(progCode.size() - 1));

            MOON_ASSERT(selfCodeIdx < progCode.size() && "Index out-of-bounds!");
            progCode[selfCodeIdx] = packInstruction(instr->op, operandCodeIdx);
            break;
        }

    // Call can reference global or local data (function pointers/refs)
    case OpCode::Call :
    case OpCode::CallLocal :
    // Instructions that reference some global data or constant:
    case OpCode::NewVar :
    case OpCode::NewObj :
    case OpCode::LoadGlobal :
    case OpCode::StoreGlobal :
    case OpCode::MemberStoreGlobal :
    case OpCode::StoreArraySubGlobal :
    case OpCode::StoreSetTypeGlobal :
    case OpCode::EnumDynInit :
    // Instructions referencing local function-level data:
    case OpCode::LoadLocal :
    case OpCode::StoreLocal :
    case OpCode::MemberStoreLocal :
    case OpCode::StoreArraySubLocal :
    case OpCode::StoreSetTypeLocal :
        {
            // Index of this instruction and its data operand:
            const UInt32 selfCodeIdx = getProgCodeIndex(codeGen.instrMapping, instr);

            // Either grab the data from global prog memory or function stack (stackIndex):
            const UInt32 operandDataIndex = ((instr->stackIndex == InvalidStackIndex) ?
                                             getProgDataIndex(codeGen.dataMapping, instr->operand.symbol) :
                                             instr->stackIndex);

            // Update the references:
            MOON_ASSERT(selfCodeIdx < progCode.size() && "Index out-of-bounds!");
            progCode[selfCodeIdx] = packInstruction(instr->op, operandDataIndex);
            break;
        }

    default :
        // No data or code referenced.
        break;
    } // switch (instr->op)
}

static void intermediateToVMBytecode(Compiler & compiler, CodeGenContext & codeGen, VM & vm)
{
    // We might still eliminate a few noops, but it should
    // be worthwhile reserving the memory beforehand anyway.
    vm.code.reserve(compiler.instrCount);

    // This step removes duplicate data and unneeded noops from the intermediate code.
    // It also serves to separate the function definition from the rest of the program code.
    createMappings(compiler, codeGen, vm, compiler.globCodeListHead, /* skipFunctions = */ true);

    // Pad the end with an instruction to anchor any potential jumps to the
    // end of the program, since we have removed all the other noop anchors.
    vm.code.push_back(packInstruction(OpCode::NoOp,    0));
    vm.code.push_back(packInstruction(OpCode::ProgEnd, 0));

    // Now we perform the same mapping for the function that got set aside in the previous step.
    createMappings(compiler, codeGen, vm, compiler.funcCodeListHead, /* skipFunctions = */ false);

    // And map back the instructions into the code vector:
    IntermediateInstr * instr;
    for (instr = compiler.globCodeListHead; instr != nullptr; instr = instr->next)
    {
        fixReferences(instr, codeGen, vm.code, vm.functions);
    }
    for (instr = compiler.funcCodeListHead; instr != nullptr; instr = instr->next)
    {
        fixReferences(instr, codeGen, vm.code, vm.functions);
    }
}

// ========================================================
// Debugging printing utilities:
// ========================================================

static void printIntermediateInstrListHelper(const IntermediateInstr * head, std::ostream & os)
{
    os << color::white() << "[[ begin intermediate instruction dump ]]" << color::restore() << "\n";

    int printedItems = 0;
    if (head != nullptr)
    {
        os << color::white() << "opcode count: " << static_cast<UInt32>(OpCode::Count)
           << color::restore() << "\n";

        for (const IntermediateInstr * instr = head; instr != nullptr; instr = instr->next)
        {
            os << color::cyan() << "[ " << std::setw(3) << instr->uid << " ] "
               << color::red() << toString(instr->op) << color::restore();

            if (!isJumpOpCode(instr->op))
            {
                if (instr->operand.symbol != nullptr)
                {
                    if (instr->dataType != Variant::Type::Null)
                    {
                        os << " \'" << color::magenta() << unescapeString(toString(*(instr->operand.symbol)).c_str())
                           << color::restore() << "\'" << " (" << toString(instr->dataType) << ")" << color::restore();
                    }
                    else
                    {
                        os << " \'" << color::magenta() << unescapeString(toString(*(instr->operand.symbol)).c_str())
                           << color::restore() << "\'";
                    }

                    if (instr->stackIndex != InvalidStackIndex)
                    {
                        os << " [stk=" << instr->stackIndex << "]";
                    }
                }
            }
            else // Jump instruction. Operand is another instruction.
            {
                if (instr->operand.jumpTarget != nullptr)
                {
                    os << " " << color::cyan() << instr->operand.jumpTarget->uid
                       << color::yellow() << " (" << toString(instr->operand.jumpTarget->op) << ")" << color::restore();
                }
                else // Null jump target. This would be an error in our compiler!
                {
                    os << color::magenta() << "ERROR_NULL_JUMP_TARGET" << color::restore();
                }
            }

            os << "\n";
            ++printedItems;
        }
    }
    else
    {
        os << "(empty)\n";
    }

    os << color::white() << "[[ listed " << printedItems << " instructions ]]"
       << color::restore() << "\n";
}

static void printInstructionMappingHelper(const InstrMap & instrMapping, std::ostream & os)
{
    os << color::white() << "[[ begin instruction map dump ]]" << color::restore() << "\n";

    for (auto && entry : instrMapping)
    {
        os << color::cyan() << "[ " << std::setw(3) << entry.second << " ] "
           << color::red() << toString(entry.first->op) << color::restore() << "\n";
    }

    os << color::white() << "[[ listed " << instrMapping.size() << " instructions ]]"
       << color::restore() << "\n";
}

// ========================================================
// FileIOCallbacks/DefaultFileIOCallbacks/OpenScriptRAII:
// ========================================================

// Automated way of opening and closing a script with the FileIOCallbacks using RAII.
struct OpenScriptRAII final
{
    using OpenMethod = bool (FileIOCallbacks::*)(const std::string &, std::istream **);

    FileIOCallbacks * callbacks;
    std::istream    * stream;

    OpenScriptRAII(const std::string & filename, FileIOCallbacks * iocb, OpenMethod openMethod)
        : callbacks{ iocb }
        , stream{ doOpen(filename, openMethod) }
    { }

    ~OpenScriptRAII()
    {
        if (stream != nullptr)
        {
            callbacks->closeScript(&stream);
        }
    }

    bool isOpen() const noexcept { return stream != nullptr; }

private:

    std::istream * doOpen(const std::string & filename, OpenMethod openMethod) const
    {
        MOON_ASSERT(callbacks != nullptr);
        std::istream * outStr = nullptr;
        const bool succeeded = (callbacks->*openMethod)(filename, &outStr);
        return (succeeded ? outStr : nullptr);
    }
};

// Relies on the Standard C++ file streams library. Accesses the local File System.
class DefaultFileIOCallbacks final
    : public FileIOCallbacks
{
public:
    DefaultFileIOCallbacks() = default;
    bool openScript(const std::string & scriptFile, std::istream ** streamOut) override;
    bool openScriptImport(const std::string & importFile, std::istream ** streamOut) override;
    void closeScript(std::istream ** stream) override;
};

bool DefaultFileIOCallbacks::openScriptImport(const std::string & importFile, std::istream ** streamOut)
{
    // No extra handling. We don't perform import path searching for the default callbacks.
    return openScript(importFile, streamOut);
}

bool DefaultFileIOCallbacks::openScript(const std::string & scriptFile, std::istream ** streamOut)
{
    MOON_ASSERT(!scriptFile.empty());
    MOON_ASSERT(streamOut != nullptr);

    std::ifstream inFile;
    inFile.exceptions(std::ifstream::goodbit); // Don't throw on error.
    inFile.open(scriptFile);

    const bool succeeded = (inFile.is_open() && inFile.good());
    if (!succeeded)
    {
        // Avoid allocating a new file object if we can't open.
        return false;
    }

    (*streamOut) = new std::ifstream{ std::move(inFile) };

    #if MOON_DEBUG
    logStream() << "Moon: DefaultFileIOCallbacks - opened script file \"" << scriptFile << "\"\n";
    #endif // MOON_DEBUG
    return succeeded;
}

void DefaultFileIOCallbacks::closeScript(std::istream ** stream)
{
    #if MOON_DEBUG
    logStream() << "Moon: DefaultFileIOCallbacks - closing script file.\n";
    #endif // MOON_DEBUG

    delete (*stream);
    stream = nullptr;
}

FileIOCallbacks::~FileIOCallbacks()
{
    // This is here to anchor the vtable to this file,
    // preventing the 'weak-vtables' warning on Clang.
}

// Shared instance used by compilers if none is provided.
static DefaultFileIOCallbacks g_defaultIOCallbacks{};

// ========================================================
// Compiler class methods:
// ========================================================

Compiler::Compiler()
    : Compiler{ &g_defaultIOCallbacks }
{ }

Compiler::Compiler(FileIOCallbacks * newIOCallbacks)
{
    setFileIOCallbacks(newIOCallbacks);
}

void Compiler::parseScript(VM * vm, const std::string & filename, SyntaxTreeNode ** outTreeRoot)
{
    MOON_ASSERT(vm != nullptr);
    MOON_ASSERT(!filename.empty());

    OpenScriptRAII script{ filename, fileIOCallbacks, &FileIOCallbacks::openScript };
    if (!script.isOpen())
    {
        MOON_INTERNAL_EXCEPTION("unable to open script file \"" + filename + "\"");
    }

    parseScript(vm, script.stream, filename, outTreeRoot);
}

void Compiler::parseScriptImport(VM * vm, const std::string & filename, SyntaxTreeNode ** outImportRoot)
{
    MOON_ASSERT(vm != nullptr);
    MOON_ASSERT(!filename.empty());

    #if MOON_DEBUG
    logStream() << "Moon: Importing script \"" << filename << "\"...\n";
    #endif // MOON_DEBUG

    OpenScriptRAII script{ filename, fileIOCallbacks, &FileIOCallbacks::openScriptImport };
    if (!script.isOpen())
    {
        MOON_INTERNAL_EXCEPTION("unable to open script import \"" + filename + "\"");
    }

    parseScript(vm, script.stream, filename, outImportRoot);
}

void Compiler::parseScript(VM * vm, std::istream * source, const std::string & filename, SyntaxTreeNode ** outTreeRoot)
{
    MOON_ASSERT(vm     != nullptr);
    MOON_ASSERT(source != nullptr);

    ParseContext parseCtx;
    std::string  currText;
    Lexer        lexer{ parseCtx, source };
    Parser       parser{ parseCtx };

    parseCtx.lexer          = &lexer;
    parseCtx.parser         = &parser;
    parseCtx.symTable       = &symTable;
    parseCtx.syntTree       = &syntTree;
    parseCtx.varInfo        = &varInfo;
    parseCtx.vm             = vm;
    parseCtx.compiler       = this;
    parseCtx.importTable    = &importTable;
    parseCtx.currText       = &currText;
    parseCtx.srcFile        = &filename;
    parseCtx.debugMode      = debugMode;
    parseCtx.enableWarnings = enableWarnings;

    const bool finishedOK = (parser.parse() == 0);
    if (finishedOK)
    {
        if (parseCtx.debugMode)
        {
            syntTree.validateNodes();
        }

        if (outTreeRoot != nullptr)
        {
            (*outTreeRoot) = parseCtx.treeRoot;
        }
    }
    else
    {
        MOON_INTERNAL_EXCEPTION("error while parsing the source code for \"" + filename + "\"");
    }
}

void Compiler::compile(VM * vm)
{
    MOON_ASSERT(vm != nullptr);

    CodeGenContext codeGen;
    codeGen.vm = vm;

    // Traverse the syntax tree to generate a list of intermediate instructions:
    globCodeListHead = traverseTreeRecursive(*this, codeGen, syntTree.getRoot());

    // Now generate the final VM bytecode from the intermediate representation:
    intermediateToVMBytecode(*this, codeGen, *vm);
}

void Compiler::setFileIOCallbacks(FileIOCallbacks * newIOCallbacks) noexcept
{
    if (newIOCallbacks == nullptr)
    {
        fileIOCallbacks = &g_defaultIOCallbacks;
    }
    else
    {
        fileIOCallbacks = newIOCallbacks;
    }
}

void Compiler::print(std::ostream & os) const
{
    os << "Intermediate instructions total: " << toString(instrCount) << "\n\n";

    os << "Global code:\n";
    printIntermediateInstrListHelper(globCodeListHead, os);

    os << "\nFunction code:\n";
    printIntermediateInstrListHelper(funcCodeListHead, os);
}

} // namespace moon {}
