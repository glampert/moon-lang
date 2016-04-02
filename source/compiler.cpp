
// ================================================================================================
// -*- C++ -*-
// File: compiler.cpp
// Author: Guilherme R. Lampert
// Created on: 06/07/15
// Brief: The Compiler class transforms a Syntax Tree representation into VM bytecode.
// ================================================================================================

#include "compiler.hpp"
#include <algorithm>
#include <iomanip> // For std::setw & friends

namespace moon
{

// ========================================================
// Local helpers:
// ========================================================

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

#define EXPECT_CHILD_AT_INDEX(node, childIndex)                        \
    do                                                                 \
    {                                                                  \
        MOON_ASSERT((node) != nullptr);                                \
        if ((node)->getChild((childIndex)) == nullptr)                 \
        {                                                              \
            MOON_INTERNAL_EXCEPTION("expected node for child index " + \
                                    toString((childIndex)));           \
        }                                                              \
    } while (0)

#define EXPECT_SYMBOL(node)                                       \
    do                                                            \
    {                                                             \
        MOON_ASSERT((node) != nullptr);                           \
        if ((node)->getSymbol() == nullptr)                       \
        {                                                         \
            MOON_INTERNAL_EXCEPTION("expected symbol for node " + \
                                    toString((node)->getType())); \
        }                                                         \
    } while (0)

// ========================================================

static std::uint32_t getProgCodeIndex(const InstrMap & instrMapping, const IntermediateInstr * instr)
{
    MOON_ASSERT(instr != nullptr);

    auto && entry = instrMapping.find(instr);
    if (entry == std::end(instrMapping))
    {
        MOON_INTERNAL_EXCEPTION("instruction " + toString(instr->op) + " not found in InstructionMap");
    }
    return entry->second;
}

static std::uint32_t getProgDataIndex(const DataMap & dataMapping, const Symbol * sym)
{
    MOON_ASSERT(sym != nullptr);

    auto && entry = dataMapping.find(sym);
    if (entry == std::end(dataMapping))
    {
        MOON_INTERNAL_EXCEPTION("symbol " + toString(*sym) + " not found in DataMap");
    }
    return entry->second;
}

static Variant::Type varTypeForNodeEval(const Compiler & compiler, const SyntaxTreeNode * node)
{
    const auto eval = node->getEval();
    switch (eval)
    {
    case SyntaxTreeNode::Eval::Int    : return Variant::Type::Integer;
    case SyntaxTreeNode::Eval::Long   : return Variant::Type::Integer;
    case SyntaxTreeNode::Eval::Float  : return Variant::Type::Float;
    case SyntaxTreeNode::Eval::Bool   : return Variant::Type::Integer;
    case SyntaxTreeNode::Eval::String : return Variant::Type::String;
    case SyntaxTreeNode::Eval::UDT    :
        {
            const Symbol * symbol = node->getSymbol();
            if (symbol == nullptr)
            {
                symbol = node->getChildSymbol(1);
            }
            return compiler.symbolToTypeId(symbol) ? Variant::Type::Tid : Variant::Type::Object;
        }
    default :
        MOON_INTERNAL_EXCEPTION("eval type '" + toString(eval) + "' doesn't map directly to a variant!");
    } // switch (eval)
}

static std::uint32_t addSymbolToProgData(VM::DataVector & progData,
                                         FunctionTable & funcTable,
                                         TypeTable & typeTable,
                                         const Symbol & sym,
                                         const Variant::Type type)
{
    Variant var{}; // Default initialized to null.

    // Identifies might be variables, function or user defined types.
    if (sym.type == Symbol::Type::Identifier)
    {
        var.type = type;

        // Some of the types require special handing:
        if (type == Variant::Type::Function)
        {
            var.value.asFunction = funcTable.findFunction(sym.name);
            if (var.value.asFunction == nullptr)
            {
                // We should not hit this error. The parser should have already validated undefined func calls.
                MOON_INTERNAL_EXCEPTION("referencing undefined function '" + toString(sym.name) + "'");
            }
        }
        else if (type == Variant::Type::Tid)
        {
            var.value.asTypeId = typeTable.findTypeId(sym.name);
            if (var.value.asTypeId == nullptr)
            {
                MOON_INTERNAL_EXCEPTION("referencing undefined type '" + toString(sym.name) + "'");
            }
        }
    }
    else // Literal constants/strings:
    {
        var = variantFromSymbol(sym);
    }

    progData.push_back(var);
    return static_cast<std::uint32_t>(progData.size() - 1);
}

static void printIntermediateInstrListHelper(const IntermediateInstr * head, std::ostream & os)
{
    os << color::white() << "[[ begin intermediate instruction dump ]]" << color::restore() << "\n";

    int printedItems = 0;
    if (head != nullptr)
    {
        os << color::white() << "opcode count: " << static_cast<unsigned>(OpCode::Count)
           << color::restore() << "\n";

        for (auto instr = head; instr != nullptr; instr = instr->next)
        {
            os << color::cyan() << "[ " << std::setw(3) << instr->uid << " ] "
               << color::red() << toString(instr->op) << color::restore();

            if (!isJumpOpCode(instr->op))
            {
                if (instr->operand.symbol != nullptr)
                {
                    if (instr->type != Variant::Type::Null)
                    {
                        os << " \'" << color::magenta() << unescapeString(toString(*(instr->operand.symbol)).c_str())
                           << color::restore() << "\'" << " (" << toString(instr->type) << ")" << color::restore();
                    }
                    else
                    {
                        os << " \'" << color::magenta() << unescapeString(toString(*(instr->operand.symbol)).c_str())
                           << color::restore() << "\'";
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

    for (const auto & entry : instrMapping)
    {
        os << color::cyan() << "[ " << std::setw(3) << entry.second << " ] "
           << color::red() << toString(entry.first->op) << color::restore() << "\n";
    }

    os << color::white() << "[[ listed " << instrMapping.size() << " instructions ]]"
       << color::restore() << "\n";
}

// ========================================================
// Intermediate Instruction generation:
// ========================================================

using EmitInstrForNodeCB = IntermediateInstr * (*)(Compiler & compiler, const SyntaxTreeNode * root);
static IntermediateInstr * traverseTreeRecursive(Compiler & compiler, const SyntaxTreeNode * root);

static IntermediateInstr * linkInstr(IntermediateInstr * head, IntermediateInstr * newTail)
{
    // Instruction chains where we use this function are short.
    // The longest would be from a big if/elseif construct, which
    // shouldn't be longer than 20 or so nodes on any sane piece
    // of code. This is unlikely to ever be a performance issue.
    auto search = head;
    for (; search->next != nullptr; search = search->next) { }
    search->next = newTail;
    return head;
}

static IntermediateInstr * emitNOOP(Compiler & compiler, const SyntaxTreeNode *)
{
    // For the node types that generate no code.
    return compiler.newInstruction(OpCode::NoOp);
}

static IntermediateInstr * emitProgStart(Compiler & compiler, const SyntaxTreeNode * root)
{
    auto block0 = compiler.newInstruction(OpCode::ProgStart);
    if (root->getChild(0) != nullptr) // A translation unit might be empty.
    {
        auto block1 = traverseTreeRecursive(compiler, root->getChild(0));
        return linkInstr(block0, block1);
    }
    return block0;
}

static IntermediateInstr * emitStatement(Compiler & compiler, const SyntaxTreeNode * root)
{
    EXPECT_CHILD_AT_INDEX(root, 1);
    if (root->getChild(0) != nullptr)
    {
        auto block0 = traverseTreeRecursive(compiler, root->getChild(0));
        auto block1 = traverseTreeRecursive(compiler, root->getChild(1));
        return linkInstr(block0, block1);
    }
    return traverseTreeRecursive(compiler, root->getChild(1));
}

static IntermediateInstr * emitIfThen(Compiler & compiler, const SyntaxTreeNode * root)
{
    // If the then-statement ('if' body) is not empty:
    if (root->getChild(1) != nullptr)
    {
        EXPECT_NUM_CHILDREN(root, 2);
        auto condition     = traverseTreeRecursive(compiler, root->getChild(0));
        auto thenStatement = traverseTreeRecursive(compiler, root->getChild(1));
        auto noop          = compiler.newInstruction(OpCode::NoOp);
        auto jumpIfFalse   = compiler.newInstruction(OpCode::JmpIfFalse, noop);

        // if-cond => jump-end-if-false => if-body => noop[jump-target/end]
        auto block0 = linkInstr(condition, jumpIfFalse);
        auto block1 = linkInstr(block0, thenStatement);
        return linkInstr(block1, noop);
    }
    else // Empty then-statement/body:
    {
        auto condition   = traverseTreeRecursive(compiler, root->getChild(0));
        auto noop        = compiler.newInstruction(OpCode::NoOp);
        auto jumpIfFalse = compiler.newInstruction(OpCode::JmpIfFalse, noop);

        // if-cond => jump-end-if-false => noop[jump-target/end]
        auto block0 = linkInstr(condition, jumpIfFalse);
        return linkInstr(block0, noop);
    }
}

static IntermediateInstr * emitIfThenElse(Compiler & compiler, const SyntaxTreeNode * root)
{
    // child[0] = condition/expression (must have)
    // child[1] = if-body/then-statement (may be empty)
    // child[2] = else-body/statement (may be empty)

    // Must have a if-condition part:
    EXPECT_CHILD_AT_INDEX(root, 0);
    auto condition = traverseTreeRecursive(compiler, root->getChild(0));

    // If the then-statement body and else-statement body are not empty, resolve them:
    auto thenStatement = (root->getChild(1) != nullptr) ? traverseTreeRecursive(compiler, root->getChild(1)) : nullptr;
    auto elseStatement = (root->getChild(2) != nullptr) ? traverseTreeRecursive(compiler, root->getChild(2)) : nullptr;

    // Jump labels:
    auto noop        = compiler.newInstruction(OpCode::NoOp);
    auto jumpToEnd   = compiler.newInstruction(OpCode::Jmp, noop);
    auto jumpIfFalse = compiler.newInstruction(OpCode::JmpIfFalse, (elseStatement != nullptr) ? elseStatement : jumpToEnd);

    IntermediateInstr * block0 = linkInstr(condition, jumpIfFalse);
    IntermediateInstr * block1 = nullptr;

    // Handle the instruction sequence differently if the if or else bodies are empty:
    if (thenStatement != nullptr)
    {
        auto temp = linkInstr(block0, thenStatement);
        block1 = linkInstr(temp, jumpToEnd);
    }
    else
    {
        block1 = linkInstr(block0, jumpToEnd);
    }

    if (elseStatement != nullptr)
    {
        return linkInstr(linkInstr(block1, elseStatement), noop);
    }
    else
    {
        return linkInstr(block1, noop);
    }

    // if-cond => jump-else-if-false => if-body =>
    //     jump-end => else-body => noop[jump-target/end]
}

static IntermediateInstr * emitIfThenElseIf(Compiler & compiler, const SyntaxTreeNode * root)
{
    // When entering the if/elseif chain for the first time:
    //
    // child[0] = if condition/expression (must have)
    // child[1] = if-body/then-statement (may be empty)
    // child[2] = elseif-body/then-statement (may be empty)
    //
    // After the initial 'if':
    //
    // child[0] = elseif condition/expression (must have)
    // child[1] = elseif-body/then-statement (may be empty)
    // child[2] = terminal else-statement or another elseif (which may have empty bodies)

    // Must have a if-condition part:
    EXPECT_CHILD_AT_INDEX(root, 0);
    auto condition = traverseTreeRecursive(compiler, root->getChild(0));

    // If the then-statement body and elseif body are not empty, resolve them:
    auto thenStatement = (root->getChild(1) != nullptr) ? traverseTreeRecursive(compiler, root->getChild(1)) : nullptr;
    auto elseStatement = (root->getChild(2) != nullptr) ? traverseTreeRecursive(compiler, root->getChild(2)) : nullptr;

    // NOTE: This will produce a noop at the end for each of the
    // if/elseif conditions in the chain. Those can be consolidated into
    // a single noop/jump-target by the final bytecode compilation stage.
    auto noop        = compiler.newInstruction(OpCode::NoOp);
    auto jumpToEnd   = compiler.newInstruction(OpCode::Jmp, noop);
    auto jumpIfFalse = compiler.newInstruction(OpCode::JmpIfFalse, (elseStatement != nullptr) ? elseStatement : jumpToEnd);

    auto block0 = linkInstr(condition, jumpIfFalse);
    auto block1 = linkInstr(block0, thenStatement);
    auto block2 = linkInstr(block1, jumpToEnd);
    return linkInstr(linkInstr(block2, elseStatement), noop);

    // if|elseif-cond => jump-next-if-false => if|elseif-body =>
    //     jump-end => [repeat till an else] => noop[jump-target/end]
}

static IntermediateInstr * emitLoop(Compiler & compiler, const SyntaxTreeNode * root)
{
    // Anchor the start and ends of the loop with noops we can jump to.
    auto noopLabelStart = compiler.newInstruction(OpCode::NoOp);
    auto noopLabelEnd   = compiler.newInstruction(OpCode::NoOp);
    auto loopContinue   = compiler.newInstruction(OpCode::Jmp, noopLabelStart);

    // If there's any code in the loop body:
    if (root->getChild(0) != nullptr)
    {
        compiler.setLoopAnchors(noopLabelStart, noopLabelEnd); // Jump targets for 'continue/break'
        auto loopBody = traverseTreeRecursive(compiler, root->getChild(0));
        compiler.clearLoopAnchors();

        return linkInstr(noopLabelStart, linkInstr(
               linkInstr(loopBody, loopContinue), noopLabelEnd));
    }

    // Empty unconditional loop (this warrants a warning in the parser, but legal)
    return linkInstr(noopLabelStart, linkInstr(loopContinue, noopLabelEnd));
}

static IntermediateInstr * emitWhileLoop(Compiler & compiler, const SyntaxTreeNode * root)
{
    // The conditional expression:
    EXPECT_CHILD_AT_INDEX(root, 0);
    auto loopCond     = traverseTreeRecursive(compiler, root->getChild(0));
    auto noopLabelEnd = compiler.newInstruction(OpCode::NoOp);
    auto loopContinue = compiler.newInstruction(OpCode::Jmp, loopCond);

    // The jump to bail the loop when cond == false:
    auto jumpIfFalse  = compiler.newInstruction(OpCode::JmpIfFalse, noopLabelEnd);
    loopCond = linkInstr(loopCond, jumpIfFalse);

    // If there's any code in the loop body:
    if (root->getChild(1) != nullptr)
    {
        compiler.setLoopAnchors(loopCond, noopLabelEnd); // Jump targets for 'continue/break'
        auto loopBody = traverseTreeRecursive(compiler, root->getChild(1));
        compiler.clearLoopAnchors();

        return linkInstr(loopCond, linkInstr(
               linkInstr(loopBody, loopContinue), noopLabelEnd));
    }

    // Empty while loop:
    return linkInstr(loopCond, linkInstr(loopContinue, noopLabelEnd));
}

static IntermediateInstr * emitForLoop(Compiler & compiler, const SyntaxTreeNode * root)
{
    // Must have the counter (i) and a range/call/name expression:
    EXPECT_CHILD_AT_INDEX(root, 0);
    EXPECT_CHILD_AT_INDEX(root, 1);
    EXPECT_SYMBOL(root->getChild(0));

    auto forIndex = root->getChild(0)->getSymbol();
    auto forInit  = traverseTreeRecursive(compiler, root->getChild(1));
    auto forPrep  = compiler.newInstruction(OpCode::ForLoopPrep, forIndex);
    auto forTest  = compiler.newInstruction(OpCode::ForLoopTest, forIndex);
    auto forStep  = compiler.newInstruction(OpCode::ForLoopStep, forIndex);
    auto forCont  = compiler.newInstruction(OpCode::Jmp, forStep);
    auto forEnd   = compiler.newInstruction(OpCode::NoOp);

    // Goes after the ForLoopTest to break if the result was false.
    auto jumpIfFalse = compiler.newInstruction(OpCode::JmpIfFalse, forEnd);

    // We have to jump over the first step of the loop on initialization (ForLoopPrep).
    auto postPrepJump = compiler.newInstruction(OpCode::Jmp, forTest);

    // If there's any code in the loop body:
    IntermediateInstr * loopBody;
    if (root->getChild(2) != nullptr)
    {
        compiler.setLoopAnchors(forStep, forEnd); // Jump targets for 'continue/break'
        loopBody = traverseTreeRecursive(compiler, root->getChild(2));
        compiler.clearLoopAnchors();
    }
    else // Empty for loop:
    {
        loopBody = nullptr;
    }

    auto block0 = linkInstr(postPrepJump, linkInstr(linkInstr(forStep, forTest), jumpIfFalse));
    auto block1 = linkInstr(linkInstr(forPrep, block0), loopBody);
    return linkInstr(forInit, linkInstr(block1, linkInstr(forCont, forEnd)));
}

static IntermediateInstr * emitBreak(Compiler & compiler, const SyntaxTreeNode *)
{
    MOON_ASSERT(compiler.lastLoopEndAnchor != nullptr);
    return compiler.newInstruction(OpCode::Jmp, compiler.lastLoopEndAnchor);
}

static IntermediateInstr * emitContinue(Compiler & compiler, const SyntaxTreeNode *)
{
    MOON_ASSERT(compiler.lastLoopStartAnchor != nullptr);
    return compiler.newInstruction(OpCode::Jmp, compiler.lastLoopStartAnchor);
}

template<OpCode OP>
static IntermediateInstr * emitUnaryOp(Compiler & compiler, const SyntaxTreeNode * root)
{
    EXPECT_NUM_CHILDREN(root, 1);
    auto argument  = traverseTreeRecursive(compiler, root->getChild(0));
    auto operation = compiler.newInstruction(OP);
    return linkInstr(argument, operation);
}

// The left-hand-side operand flag is set to true for the
// compound op+store opcodes, e.g.: AddStore, SubStore, etc.
// Those must also reference the target symbol that is to receive
// the result of the operation.
template<OpCode OP, bool lhsOperand = false>
static IntermediateInstr * emitBinaryOp(Compiler & compiler, const SyntaxTreeNode * root)
{
    EXPECT_NUM_CHILDREN(root, 2);
    auto arg0 = traverseTreeRecursive(compiler, root->getChild(0));
    auto arg1 = traverseTreeRecursive(compiler, root->getChild(1));
    auto operation = compiler.newInstruction(OP, lhsOperand ? root->getChild(0)->getSymbol() : nullptr);
    return linkInstr(linkInstr(arg0, arg1), operation);
}

static IntermediateInstr * emitLoad(Compiler & compiler, const SyntaxTreeNode * root)
{
    EXPECT_SYMBOL(root);
    auto symbol = root->getSymbol();

    OpCode op = OpCode::LoadGlob;
    std::uint16_t paramIdx = 0;

    // Booleans are converted to integer (0=false, 1=true).
    if (symbol->type == Symbol::Type::BoolLiteral)
    {
        const char * symVal = (symbol->value.asBoolean ? "1" : "0");
        symbol = compiler.symTable.findIntLiteral(symVal); // Note: Relies on the 0,1 literals being always defined build-ins
    }
    else if (symbol->type == Symbol::Type::Identifier)
    {
        if (compiler.symbolIsFunctionLocal(symbol, paramIdx)) // A local variable or function parameter?
        {
            op = OpCode::LoadLocal;
        }
    }

    auto operation = compiler.newInstruction(op, symbol);
    operation->paramIdx = paramIdx;

    // Function calls consist of a chain of load instructions followed by one CALL instruction.
    if (root->getChild(0) != nullptr)
    {
        auto argList = traverseTreeRecursive(compiler, root->getChild(0));
        return linkInstr(operation, argList); // <-- NOTE: Change this to alter func parameter passing order!
    }

    return operation;
}

static IntermediateInstr * makeLoadMemberOffsetInstr(Compiler & compiler,
                                                     const Symbol * objSymbol,
                                                     const Symbol * memSymbol,
                                                     const TypeId ** typeIdOut = nullptr,
                                                     int * memberOffsetOut = nullptr)
{
    const TypeId * typeId;
    if (compiler.insideFunctionDecl)
    {
        typeId = compiler.findFunctionLocalSymbolTypeId(objSymbol);
    }
    else
    {
        typeId = compiler.findGlobalSymbolTypeId(objSymbol);
    }

    if (typeId == nullptr || typeId->templateObject == nullptr)
    {
        MOON_INTERNAL_EXCEPTION("'" + toString(objSymbol->name) + "' is not a type or has no dynamic members table!");
    }

    const int memberOffset = typeId->templateObject->findMemberIndex(memSymbol->name);
    if (memberOffset < 0)
    {
        MOON_INTERNAL_EXCEPTION("object has no member '" + toString(memSymbol->name) + "'");
    }

    if (typeIdOut)       { *typeIdOut       = typeId;       }
    if (memberOffsetOut) { *memberOffsetOut = memberOffset; }

    auto offsetLiteral = compiler.symTable.findOrDefineValue(memberOffset);
    return compiler.newInstruction(OpCode::LoadGlob, offsetLiteral);
}

static void collectMemberRefs(std::vector<const Symbol *> & memberList, const SyntaxTreeNode * root)
{
    if (root == nullptr)
    {
        return;
    }

    collectMemberRefs(memberList, root->getChild(0));
    collectMemberRefs(memberList, root->getChild(1));

    if (root->getSymbol() != nullptr)
    {
        memberList.push_back(root->getSymbol());
    }
}

static IntermediateInstr * setUpMemberOffsetLoadChain(Compiler & compiler, const Symbol * objSymbol)
{
    MOON_ASSERT(objSymbol != nullptr);

    int lastMemberOffset = 0;
    const TypeId * lastMemberTypeId = nullptr;

    auto memOffsetInstr = makeLoadMemberOffsetInstr(compiler, objSymbol,
        compiler.memberRefList[1], &lastMemberTypeId, &lastMemberOffset);

    if (compiler.memberRefList.size() > 2)
    {
        const auto & member = lastMemberTypeId->templateObject->getMemberAt(lastMemberOffset);
        if (member.data.type == Variant::Type::Object)
        {
            lastMemberTypeId = member.data.value.asObject->typeId;
        }

        // [0:1] is the first obj.member pair. A chain might follow.
        for (unsigned i = 2; i < compiler.memberRefList.size(); ++i)
        {
            MOON_ASSERT(lastMemberTypeId != nullptr);
            const Symbol * memSymbol = compiler.memberRefList[i];

            lastMemberOffset = lastMemberTypeId->templateObject->findMemberIndex(memSymbol->name);
            if (lastMemberOffset < 0)
            {
                MOON_INTERNAL_EXCEPTION("sub object of type '" + toString(lastMemberTypeId->name) +
                                        "' has no member '" + toString(memSymbol->name) + "'");
            }

            auto offsetLiteral = compiler.symTable.findOrDefineValue(lastMemberOffset);
            auto loadMemOffs = compiler.newInstruction(OpCode::LoadGlob, offsetLiteral);
            linkInstr(memOffsetInstr, loadMemOffs);

            const auto & subMember = lastMemberTypeId->templateObject->getMemberAt(lastMemberOffset);
            if (subMember.data.type == Variant::Type::Object)
            {
                lastMemberTypeId = subMember.data.value.asObject->typeId;
            }
        }
    }

    return memOffsetInstr;
}

static IntermediateInstr * emitStore(Compiler & compiler, const SyntaxTreeNode * root)
{
    EXPECT_NUM_CHILDREN(root, 2);

    unsigned numMemberOffsets = 0;
    const Symbol * objSymbol  = nullptr;
    Variant::Type symbType    = Variant::Type::Null;

    compiler.memberRefList.clear();
    if (root->getChild(0)->getType() == SyntaxTreeNode::Type::ExprMemberRef)
    {
        collectMemberRefs(compiler.memberRefList, root->getChild(0));
        MOON_ASSERT(compiler.memberRefList.size() >= 2); // obj.member pair at least

        objSymbol = compiler.memberRefList[0];
        numMemberOffsets = compiler.memberRefList.size() - 1;
    }
    else
    {
        objSymbol = root->getChildSymbol(0);
        MOON_ASSERT(objSymbol != nullptr);
    }

    // Don't enter if doing a obj.member store
    if (root->getEval() == SyntaxTreeNode::Eval::Undefined && numMemberOffsets == 0)
    {
        // Assignments from a local/global var
        const Symbol * lhsSymbol = root->getChildSymbol(1);
        MOON_ASSERT(lhsSymbol != nullptr);

        const TypeId * typeId = (compiler.insideFunctionDecl ?
                compiler.findFunctionLocalSymbolTypeId(lhsSymbol) :
                compiler.findGlobalSymbolTypeId(lhsSymbol));

        // We want the specific type-id for the native types.
        // UDTs just decay to 'object'.
        MOON_ASSERT(typeId != nullptr);
        if (!typeId->isBuiltIn)
        {
            typeId = compiler.symbolToTypeId(compiler.symTable.findSymbol("object"));
        }

        symbType = variantTypeFromTypeId(typeId);
    }
    else
    {
        // obj.member stores will overwrite symbType below. No need to set it.
        if (numMemberOffsets == 0)
        {
            symbType = varTypeForNodeEval(compiler, root);
        }
    }

    OpCode op;
    std::uint16_t paramIdx = 0;
    IntermediateInstr * memOffsetInstr = nullptr;
    IntermediateInstr * argument = traverseTreeRecursive(compiler, root->getChild(1));

    // A local variable or function parameter?
    if (compiler.symbolIsFunctionLocal(objSymbol, paramIdx))
    {
        if (numMemberOffsets > 0)
        {
            memOffsetInstr = setUpMemberOffsetLoadChain(compiler, objSymbol);
            op = OpCode::MemberStoreLocal;
            symbType = Variant::Type::Object;
        }
        else
        {
            op = OpCode::StoreLocal;
        }
    }
    else // Global symbol
    {
        if (numMemberOffsets > 0)
        {
            memOffsetInstr = setUpMemberOffsetLoadChain(compiler, objSymbol);
            op = OpCode::MemberStoreGlob;
            symbType = Variant::Type::Object;
        }
        else
        {
            op = OpCode::StoreGlob;
        }
    }

    if (numMemberOffsets > 0)
    {
        auto offsetCountLiteral = compiler.symTable.findOrDefineValue(numMemberOffsets);
        auto offsCount = compiler.newInstruction(OpCode::LoadGlob, offsetCountLiteral);

        MOON_ASSERT(memOffsetInstr != nullptr);
        memOffsetInstr = linkInstr(memOffsetInstr, offsCount);
    }

    auto operation = compiler.newInstruction(op, objSymbol, symbType);
    operation->paramIdx = paramIdx;

    // Storing from the return value of a function references the Return Value Register (RVR).
    if (root->getChild(1)->getType() == SyntaxTreeNode::Type::ExprFuncCall)
    {
        auto getRVR = compiler.newInstruction(OpCode::LoadRVR);
        return linkInstr(linkInstr(argument, memOffsetInstr), linkInstr(getRVR, operation));
    }
    else // Normal store:
    {
        return linkInstr(linkInstr(argument, memOffsetInstr), operation);
    }
}

static IntermediateInstr * emitMemberRef(Compiler & compiler, const SyntaxTreeNode * root)
{
    EXPECT_NUM_CHILDREN(root, 2);
    EXPECT_SYMBOL(root->getChild(1));

    auto objLoad = traverseTreeRecursive(compiler, root->getChild(0));
    const Symbol * objSymbol = root->getChildSymbol(0);
    const Symbol * memSymbol = root->getChildSymbol(1);

    IntermediateInstr * loadMemOffs;
    if (objSymbol != nullptr) // First pass
    {
        int memberOffset = 0;
        loadMemOffs = makeLoadMemberOffsetInstr(compiler, objSymbol, memSymbol, &compiler.lastMemberTypeId, &memberOffset);

        const auto & member = compiler.lastMemberTypeId->templateObject->getMemberAt(memberOffset);
        if (member.data.type == Variant::Type::Object)
        {
            compiler.lastMemberTypeId = member.data.value.asObject->typeId;
        }
    }
    else // Going down a chain of obj.member.member.member ...
    {
        MOON_ASSERT(compiler.lastMemberTypeId != nullptr);
        const int memberOffset = compiler.lastMemberTypeId->templateObject->findMemberIndex(memSymbol->name);
        if (memberOffset < 0)
        {
            MOON_INTERNAL_EXCEPTION("sub object of type '" + toString(compiler.lastMemberTypeId->name) +
                                    "' has no member '" + toString(memSymbol->name) + "'");
        }

        auto offsetLiteral = compiler.symTable.findOrDefineValue(memberOffset);
        loadMemOffs = compiler.newInstruction(OpCode::LoadGlob, offsetLiteral);

        const auto & member = compiler.lastMemberTypeId->templateObject->getMemberAt(memberOffset);
        if (member.data.type == Variant::Type::Object)
        {
            compiler.lastMemberTypeId = member.data.value.asObject->typeId;
        }
    }

    return linkInstr(linkInstr(objLoad, loadMemOffs), compiler.newInstruction(OpCode::MemberRef));
}

static IntermediateInstr * emitArraySubscript(Compiler & compiler, const SyntaxTreeNode * root)
{
    // child[0] is reserved for parameter lists and may be null.
    EXPECT_CHILD_AT_INDEX(root, 1);
    EXPECT_CHILD_AT_INDEX(root, 2);
    auto arrayExpr     = traverseTreeRecursive(compiler, root->getChild(1));
    auto subscriptExpr = traverseTreeRecursive(compiler, root->getChild(2));
    auto subscriptOp   = compiler.newInstruction(OpCode::ArraySubscript);
    return linkInstr(arrayExpr, linkInstr(subscriptExpr, subscriptOp));
}

//FIXME the argument counting is still broken.
// it will count unrelated nodes like operators.
// e.g.: println("a" + to_string("b")) would yield 3 instead of 2
static void countArgsRecursive(const SyntaxTreeNode * root, int & argCountOut, Compiler & compiler)
{
    if (root == nullptr)
    {
        return;
    }

    ++argCountOut;
    compiler.markVisited(root);

    if (!compiler.nodeWasVisited(root->getChild(0)))
    {
        countArgsRecursive(root->getChild(0), argCountOut, compiler);
    }
    if (!compiler.nodeWasVisited(root->getChild(1)))
    {
        countArgsRecursive(root->getChild(1), argCountOut, compiler);
    }
}

// This function is used to resolve parameter chains for function calls,
// constructor calls and array literals. The OP template parameter is the
// instruction ending the list (i.e.: a Call, NewObj, NewArray, etc).
template<OpCode OP, Variant::Type OperandType>
static IntermediateInstr * emitParamChain(Compiler & compiler, const SyntaxTreeNode * root)
{
    auto operation = compiler.newInstruction(OP, root->getSymbol(), OperandType);

    // We have to keep track of the visited nodes to be able to properly
    // compute the argument counts. Some nodes will be visited more than
    // once due to the parameter list chains.
    compiler.clearVisited();

    int argumentCount = 0;
    IntermediateInstr * myArgs = nullptr;
    IntermediateInstr * argListChain = nullptr;

    // We might be linked to an argument list chain, so go down the child 0 hierarchy:
    if (root->getChild(0) != nullptr)
    {
        argListChain = traverseTreeRecursive(compiler, root->getChild(0));
    }

    // Child 1 has the argument list of this function:
    if (root->getChild(1) != nullptr)
    {
        myArgs = traverseTreeRecursive(compiler, root->getChild(1));
        countArgsRecursive(root->getChild(1), argumentCount, compiler);
    }

    // Argument count is appended as a literal integer:
    auto argCountLiteral = compiler.symTable.findOrDefineValue(argumentCount);
    auto loadArgCountOp  = compiler.newInstruction(OpCode::LoadGlob, argCountLiteral);

    // Wrap it up!
    if (myArgs != nullptr)
    {
        return linkInstr(linkInstr(myArgs, linkInstr(loadArgCountOp, operation)), argListChain);
    }
    else // No args for this function/constructor:
    {
        return linkInstr(linkInstr(loadArgCountOp, operation), argListChain);
    }
}

static IntermediateInstr * emitFunctionDecl(Compiler & compiler, const SyntaxTreeNode * root)
{
    EXPECT_SYMBOL(root); // Func name
    auto funcStart = compiler.newInstruction(OpCode::FuncStart, root->getSymbol(), Variant::Type::Function);
    auto funcEnd   = compiler.newInstruction(OpCode::FuncEnd, root->getSymbol());

    // Resolve the function body if any. Parameter list and return type are not relevant here.
    compiler.beginFunction(funcEnd, root);
    auto funcBody = ((root->getChild(1) != nullptr) ?
                     traverseTreeRecursive(compiler, root->getChild(1)) :
                     compiler.newInstruction(OpCode::NoOp));
    compiler.endFunction();

    // Chain everything together:
    return linkInstr(funcStart, linkInstr(funcBody, funcEnd));
}

static IntermediateInstr * emitReturn(Compiler & compiler, const SyntaxTreeNode * root)
{
    MOON_ASSERT(compiler.lastFuncEndAnchor != nullptr);
    auto retJump = compiler.newInstruction(OpCode::JmpReturn, compiler.lastFuncEndAnchor);

    // Return simply resolves its child expression if it has one.
    if (root->getChild(0) != nullptr)
    {
        auto retExpr = traverseTreeRecursive(compiler, root->getChild(0));

        // We only need to set the Return Value Register when the return
        // expression is not itself another call. If it is a call, the
        // leaf function in the call tree will set the register.
        if (root->getChild(0)->getType() != SyntaxTreeNode::Type::ExprFuncCall)
        {
            auto setRVR = compiler.newInstruction(OpCode::StoreRVR);
            return linkInstr(retExpr, linkInstr(setRVR, retJump));
        }
        else
        {
            return linkInstr(retExpr, retJump);
        }
    }
    else // Void return:
    {
        return retJump;
    }
}

static IntermediateInstr * emitNewVar(Compiler & compiler, const SyntaxTreeNode * root)
{
    EXPECT_SYMBOL(root); // Var name

    const Symbol * typeSymbol       = nullptr;
    const Symbol * objectTypeSymbol = nullptr;
    IntermediateInstr * argument    = nullptr;

    if (root->getChild(1) != nullptr) // Type node
    {
        // We just want 'object' for the NEW_VAR instruction.
        // If it is a dynamic object, the NEW_OBJ instr handles the type.
        if (root->getChild(1)->getEval() == SyntaxTreeNode::Eval::UDT)
        {
            typeSymbol = compiler.symTable.findSymbol("object");
        }
        else
        {
            typeSymbol = root->getChild(1)->getSymbol();
        }
    }

    if (typeSymbol == nullptr)
    {
        typeSymbol = symbolFromEval(compiler.symTable, root->getEval());
        if (root->getEval() == SyntaxTreeNode::Eval::UDT)
        {
            // Find out the exact type of the UDT by looking at the constructor call:
            auto initExpr = root->getChild(0);
            MOON_ASSERT(initExpr != nullptr && "Expected a constructor call!");
            objectTypeSymbol = initExpr->getSymbol();
        }
    }

    if (objectTypeSymbol == nullptr)
    {
        objectTypeSymbol = typeSymbol;
    }

    int argumentCount = 0;
    if (root->getChild(0) != nullptr) // Initializer expression node
    {
        argument = traverseTreeRecursive(compiler, root->getChild(0));
        argumentCount = 1;
    }

    const Symbol * symbol = root->getSymbol();
    OpCode op = OpCode::StoreGlob;
    std::uint16_t paramIdx = 0;

    if (compiler.symbolIsFunctionLocal(symbol, paramIdx)) // A local variable or function parameter?
    {
        op = OpCode::StoreLocal;
    }
    else // A global identifier
    {
        const auto typeId = compiler.symbolToTypeId(objectTypeSymbol);
        compiler.addGlobalSymbol(symbol, typeId);
    }

    auto newOp   = compiler.newInstruction(OpCode::NewVar, typeSymbol, Variant::Type::Tid);
    auto storeOp = compiler.newInstruction(op, symbol, varTypeForNodeEval(compiler, root));
    storeOp->paramIdx = paramIdx;

    // A load instruction is appended to indicate if we should pop one
    // value from the stack to initialize the new var or if it was left
    // uninitialized.
    auto argCountLiteral = compiler.symTable.findOrDefineValue(argumentCount);
    auto loadArgCountOp  = compiler.newInstruction(OpCode::LoadGlob, argCountLiteral);

    if (argument != nullptr)
    {
        return linkInstr(linkInstr(argument, linkInstr(loadArgCountOp, newOp)), storeOp);
    }
    else
    {
        return linkInstr(linkInstr(loadArgCountOp, newOp), storeOp);
    }
}

static IntermediateInstr * emitNewRange(Compiler & compiler, const SyntaxTreeNode * root)
{
    EXPECT_CHILD_AT_INDEX(root, 1); // range start
    EXPECT_CHILD_AT_INDEX(root, 2); // range end

    auto startExpr  = traverseTreeRecursive(compiler, root->getChild(1));
    auto endExpr    = traverseTreeRecursive(compiler, root->getChild(2));
    auto newRangeOp = compiler.newInstruction(OpCode::NewRange);

    return linkInstr(linkInstr(startExpr, endExpr), newRangeOp);
}

static IntermediateInstr * emitMatchPrep(Compiler & compiler, const SyntaxTreeNode * root)
{
    EXPECT_CHILD_AT_INDEX(root, 0); // The param to match with

    auto matchParam  = traverseTreeRecursive(compiler, root->getChild(0));
    auto matchPrepOp = compiler.newInstruction(OpCode::MatchPrep);
    auto matchEndOp  = compiler.newInstruction(OpCode::NoOp);

    // Reusing the return anchor to mark the end of the match switch.
    compiler.setReturnAnchor(matchEndOp);
    auto matchBody = root->getChild(1) ? traverseTreeRecursive(compiler, root->getChild(1)) : nullptr;
    compiler.clearReturnAnchor();

    return linkInstr(linkInstr(linkInstr(matchParam, matchPrepOp), matchBody), matchEndOp);
}

static IntermediateInstr * emitMatchTest(Compiler & compiler, const SyntaxTreeNode * root)
{
    MOON_ASSERT(compiler.lastFuncEndAnchor != nullptr);
    EXPECT_CHILD_AT_INDEX(root, 0); // The expression to match against

    auto caseExpr     = traverseTreeRecursive(compiler, root->getChild(0));
    auto caseBody     = root->getChild(1) ? traverseTreeRecursive(compiler, root->getChild(1)) : nullptr;
    auto caseEnd      = compiler.newInstruction(OpCode::NoOp);
    auto caseEndJump  = compiler.newInstruction(OpCode::JmpIfFalse, caseEnd);
    auto matchEndJump = compiler.newInstruction(OpCode::Jmp, compiler.lastFuncEndAnchor);
    auto matchTestOp  = compiler.newInstruction(OpCode::MatchTest);

    auto block0 = linkInstr(linkInstr(caseExpr, matchTestOp), caseEndJump);
    auto block1 = linkInstr(linkInstr(block0, caseBody), matchEndJump);
    auto siblingCase = root->getChild(2) ? traverseTreeRecursive(compiler, root->getChild(2)) : nullptr;

    return linkInstr(linkInstr(block1, caseEnd), siblingCase);
}

static IntermediateInstr * emitMatchDefault(Compiler & compiler, const SyntaxTreeNode * root)
{
    if (root->getChild(1))
    {
        // The default clause may be empty.
        return traverseTreeRecursive(compiler, root->getChild(1));
    }
    return compiler.newInstruction(OpCode::NoOp);
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
    &emitProgStart,                                         // TranslationUnit
    &emitNOOP,                                              // ModuleDefinition
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
    &emitNOOP,                                              // EnumDeclStatement
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
    &emitBinaryOp<OpCode::SubStore, true>,                  // ExprSubAssign
    &emitBinaryOp<OpCode::AddStore, true>,                  // ExprAddAssign
    &emitBinaryOp<OpCode::ModStore, true>,                  // ExprModAssign
    &emitBinaryOp<OpCode::DivStore, true>,                  // ExprDivAssign
    &emitBinaryOp<OpCode::MulStore, true>,                  // ExprMulAssign
    &emitUnaryOp<OpCode::Negate>,                           // ExprUnaryMinus
    &emitUnaryOp<OpCode::Plus>                              // ExprUnaryPlus
};
static_assert(arrayLength(emitInstrCallbacks) == unsigned(SyntaxTreeNode::Type::Count),
              "Keep this array in sync with the enum declaration!");

// Calls the appropriate handler according to the node type (which in turn might call this function again)
static IntermediateInstr * traverseTreeRecursive(Compiler & compiler, const SyntaxTreeNode * root)
{
    MOON_ASSERT(root != nullptr);
    const auto handlerIndex  = static_cast<unsigned>(root->getType());
    MOON_ASSERT(handlerIndex < static_cast<unsigned>(SyntaxTreeNode::Type::Count));
    return emitInstrCallbacks[handlerIndex](compiler, root);
}

// ========================================================
// Compiler class methods:
// ========================================================

void Compiler::compile(VM & vm)
{
    runtimeTypes  = &vm.runtimeTypes;
    instrListHead = traverseTreeRecursive(*this, syntTree.getRoot());
    intermediateToVM(vm);
}

void Compiler::print(std::ostream & os) const
{
    printIntermediateInstructions(os);
    os << "\n";
    printInstructionMapping(os);
}

void Compiler::printIntermediateInstructions(std::ostream & os) const
{
    os << "Global code:\n";
    printIntermediateInstrListHelper(instrListHead, os);
    os << "\nFunction code:\n";
    printIntermediateInstrListHelper(funcListHead,  os);
}

void Compiler::printInstructionMapping(std::ostream & os) const
{
    printInstructionMappingHelper(instrMapping, os);
}

void Compiler::setLoopAnchors(const IntermediateInstr * startLabel,
                              const IntermediateInstr * endLabel) noexcept
{
    lastLoopStartAnchor = startLabel;
    lastLoopEndAnchor   = endLabel;
}

void Compiler::clearLoopAnchors() noexcept
{
    lastLoopStartAnchor = nullptr;
    lastLoopEndAnchor   = nullptr;
}

void Compiler::setReturnAnchor(const IntermediateInstr * endLabel) noexcept
{
    lastFuncEndAnchor = endLabel;
}

void Compiler::clearReturnAnchor() noexcept
{
    lastFuncEndAnchor = nullptr;
}

void Compiler::markVisited(const SyntaxTreeNode * node)
{
    visitedNodes.push_back(node);
}

bool Compiler::nodeWasVisited(const SyntaxTreeNode * node) const
{
    if (node == nullptr)
    {
        return false;
    }
    const auto iter = std::find(std::begin(visitedNodes), std::end(visitedNodes), node);
    return iter != std::end(visitedNodes);
}

void Compiler::clearVisited() noexcept
{
    visitedNodes.clear();
}

const TypeId * Compiler::guessTypeId(const SyntaxTreeNode * root)
{
    const Symbol * typeSymbol       = nullptr;
    const Symbol * objectTypeSymbol = nullptr;

    if (root->getChild(1) != nullptr) // Type node
    {
        typeSymbol = root->getChild(1)->getSymbol();
    }

    if (typeSymbol == nullptr)
    {
        typeSymbol = symbolFromEval(symTable, root->getEval());
        if (root->getEval() == SyntaxTreeNode::Eval::UDT)
        {
            // Find out the exact type of the UDT by looking at the constructor call:
            auto initExpr = root->getChild(0);
            MOON_ASSERT(initExpr != nullptr && "Expected a constructor call!");
            objectTypeSymbol = initExpr->getSymbol();
        }
    }

    if (objectTypeSymbol == nullptr)
    {
        objectTypeSymbol = typeSymbol;
    }

    return symbolToTypeId(objectTypeSymbol);
}

void Compiler::collectFunctionArgSymbols(const SyntaxTreeNode * root)
{
    if (root == nullptr)
    {
        return;
    }

    addFunctionLocalSymbol(root->getSymbol(), guessTypeId(root));
    collectFunctionArgSymbols(root->getChild(0));
}

void Compiler::collectFunctionVarSymbols(const SyntaxTreeNode * root)
{
    if (root == nullptr)
    {
        return;
    }

    if (root->getType() == SyntaxTreeNode::Type::VarDeclStatement)
    {
        addFunctionLocalSymbol(root->getSymbol(), guessTypeId(root));
    }

    collectFunctionVarSymbols(root->getChild(0));
    collectFunctionVarSymbols(root->getChild(1));
}

void Compiler::beginFunction(const IntermediateInstr * endLabel, const SyntaxTreeNode * root)
{
    if (lastFuncEndAnchor != nullptr)
    {
        MOON_INTERNAL_EXCEPTION("Nested functions not supported yet!");
    }

    collectFunctionArgSymbols(root->getChild(0));
    collectFunctionVarSymbols(root->getChild(1));

    setReturnAnchor(endLabel);
    insideFunctionDecl = true;
}

void Compiler::endFunction()
{
    clearReturnAnchor();
    funcLocalIdentifiers.clear();
    insideFunctionDecl = false;
}

void Compiler::addFunctionLocalSymbol(const Symbol * symbol, const TypeId * tid)
{
    funcLocalIdentifiers.push_back(std::make_pair(symbol, tid));
}

void Compiler::addGlobalSymbol(const Symbol * symbol, const TypeId * tid)
{
    globalIdentifiers.push_back(std::make_pair(symbol, tid));
}

const TypeId * Compiler::findFunctionLocalSymbolTypeId(const Symbol * symbol) const
{
    for (auto && entry : funcLocalIdentifiers)
    {
        if (entry.first == symbol)
        {
            return entry.second;
        }
    }
    return nullptr;
}

const TypeId * Compiler::findGlobalSymbolTypeId(const Symbol * symbol) const
{
    for (auto && entry : globalIdentifiers)
    {
        if (entry.first == symbol)
        {
            return entry.second;
        }
    }
    return nullptr;
}

bool Compiler::symbolIsFunctionLocal(const Symbol * symbol, std::uint16_t & paramIdx) const
{
    const unsigned symbolCount = funcLocalIdentifiers.size();
    for (unsigned index = 0; index < symbolCount; ++index)
    {
        if (funcLocalIdentifiers[index].first == symbol)
        {
            paramIdx = index;
            return true;
        }
    }
    return false;
}

const TypeId * Compiler::symbolToTypeId(const Symbol * symbol) const
{
    MOON_ASSERT(symbol       != nullptr);
    MOON_ASSERT(runtimeTypes != nullptr);
    return runtimeTypes->findTypeId(symbol->name);
}

void Compiler::intermediateToVM(VM & vm)
{
    // We might still eliminate a few noops, but it should
    // be worthwhile reserving the memory beforehand anyway.
    vm.code.reserve(instructionCount);

    // This step removes duplicate data and unneeded noops from the intermediate code.
    // It also serves to separate the function definition from the rest of the code.
    createMappings(vm.data, vm.code, vm.functions, vm.runtimeTypes,
                   instrListHead, /* skipFunctions = */ true);

    // Pad the end with an instruction to anchor any potential jumps to the
    // end of the program, since we have removed all the other noop anchors.
    vm.code.push_back(packInstruction(OpCode::NoOp,    0));
    vm.code.push_back(packInstruction(OpCode::ProgEnd, 0));

    // Now we perform the same mapping for the function that got set aside in the previous step.
    createMappings(vm.data, vm.code, vm.functions, vm.runtimeTypes,
                   funcListHead, /* skipFunctions = */ false);

    // And map back the instructions into the code vector:
    IntermediateInstr * instr;
    for (instr = instrListHead; instr != nullptr; instr = instr->next)
    {
        fixReferences(instr, vm.code, vm.functions);
    }
    for (instr = funcListHead; instr != nullptr; instr = instr->next)
    {
        fixReferences(instr, vm.code, vm.functions);
    }
}

void Compiler::createMappings(VM::DataVector & progData, VM::CodeVector & progCode,
                              FunctionTable  & funcTable, TypeTable & typeTable,
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
                funcListHead = funcListTail = instr;
            }
            else
            {
                funcListTail->next = instr;
            }

            for (; instr != nullptr && instr->op != OpCode::FuncEnd;
                 instr = instr->next) { }

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
            // References any data? (Jumps reference other instructions)
            if (instr->operand.symbol != nullptr && !isJumpOpCode(instr->op) && !referencesStackData(instr->op))
            {
                // Each symbol is added once.
                if (dataMapping.find(instr->operand.symbol) == std::end(dataMapping))
                {
                    auto index = addSymbolToProgData(progData, funcTable, typeTable,
                                                     *(instr->operand.symbol), instr->type);
                    dataMapping.emplace(instr->operand.symbol, index);
                }
            }

            // Operand index is set later. For now this instruction is just a placeholder.
            progCode.push_back(packInstruction(instr->op, 0));
            instrMapping.emplace(instr, progCode.size() - 1);
        }
        else
        {
            // The noop doesn't get inserted, so this instruction maps to the
            // index of the next instruction to be added to the progCode vector.
            instrMapping.emplace(instr, progCode.size());
        }

        // Advance to the next on list:
        prevInstr = instr;
        instr = instr->next;
    }
}

void Compiler::fixReferences(const IntermediateInstr * instr, VM::CodeVector & progCode, FunctionTable & funcTable)
{
    MOON_ASSERT(instr != nullptr);
    switch (instr->op)
    {
    // Function declarations:
    case OpCode::FuncStart :
    case OpCode::FuncEnd :
        {
            // Index of this instruction and its data operand (the function object in funcTable):
            auto selfCodeIdx = getProgCodeIndex(instrMapping, instr);
            auto operandDataIndex = getProgDataIndex(dataMapping, instr->operand.symbol);
            MOON_ASSERT(selfCodeIdx < progCode.size() && "Index out-of-bounds!");

            const char * funcName = instr->operand.symbol->name;
            const auto funcObj = funcTable.findFunction(funcName);

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
    case OpCode::JmpIfFalse :
    case OpCode::JmpIfTrue :
    case OpCode::JmpReturn :
        {
            // Index of this instruction and its jump target:
            auto selfCodeIdx = getProgCodeIndex(instrMapping, instr);
            auto operandCodeIdx = getProgCodeIndex(instrMapping, instr->operand.jumpTarget);

            // Clamp if this instruction jumps to the end of the program.
            // (happens to removed noops that pointed to the end of a block/if-else-statement).
            operandCodeIdx = std::min(operandCodeIdx, static_cast<std::uint32_t>(progCode.size() - 1));

            MOON_ASSERT(selfCodeIdx < progCode.size() && "Index out-of-bounds!");
            progCode[selfCodeIdx] = packInstruction(instr->op, operandCodeIdx);
            break;
        }
    // Instructions that reference some global data or constant:
    case OpCode::Call :
    case OpCode::NewVar :
    case OpCode::NewObj :
    case OpCode::LoadGlob :
    case OpCode::StoreGlob :
    case OpCode::MemberStoreGlob :
    case OpCode::SubStore :
    case OpCode::AddStore :
    case OpCode::ModStore :
    case OpCode::DivStore :
    case OpCode::MulStore :
        {
            // Index of this instruction and its data operand:
            auto selfCodeIdx = getProgCodeIndex(instrMapping, instr);
            auto operandDataIndex = getProgDataIndex(dataMapping, instr->operand.symbol);
            MOON_ASSERT(selfCodeIdx < progCode.size() && "Index out-of-bounds!");
            progCode[selfCodeIdx] = packInstruction(instr->op, operandDataIndex);
            break;
        }
    // Instructions referencing local function-level data:
    case OpCode::LoadLocal :
    case OpCode::StoreLocal :
    case OpCode::MemberStoreLocal :
        {
            auto selfCodeIdx = getProgCodeIndex(instrMapping, instr);
            MOON_ASSERT(selfCodeIdx < progCode.size() && "Index out-of-bounds!");
            progCode[selfCodeIdx] = packInstruction(instr->op, instr->paramIdx);
            break;
        }
    default :
        break;
    } // switch (instr->op)
}

// ========================================================
// Intermediate Instruction node allocation:
// ========================================================

IntermediateInstr * Compiler::newInstruction(const OpCode op)
{
    auto instr                = instrPool.allocate();
    instr->next               = nullptr;
    instr->operand.symbol     = nullptr;
    instr->uid                = instructionCount++;
    instr->paramIdx           = 0;
    instr->type               = Variant::Type::Null;
    instr->op                 = op;
    return instr;
}

IntermediateInstr * Compiler::newInstruction(const OpCode op, const Symbol * symbol, const Variant::Type type)
{
    auto instr                = instrPool.allocate();
    instr->next               = nullptr;
    instr->operand.symbol     = symbol;
    instr->uid                = instructionCount++;
    instr->paramIdx           = 0;
    instr->type               = type;
    instr->op                 = op;
    return instr;
}

IntermediateInstr * Compiler::newInstruction(const OpCode op, const IntermediateInstr * jumpTarget)
{
    auto instr                = instrPool.allocate();
    instr->next               = nullptr;
    instr->operand.jumpTarget = jumpTarget;
    instr->uid                = instructionCount++;
    instr->paramIdx           = 0;
    instr->type               = Variant::Type::Null;
    instr->op                 = op;
    return instr;
}

} // namespace moon {}
