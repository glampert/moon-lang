
// ================================================================================================
// -*- C++ -*-
// File: compiler.cpp
// Author: Guilherme R. Lampert
// Created on: 06/07/15
// Brief: The Compiler class transforms a Syntax Tree representation into VM bytecode.
// ================================================================================================

#include "compiler.hpp"
#include "semantic_check.hpp"
#include <algorithm>
#include <iomanip> // For std::setw & friends
#include <fstream> // Used by the DefaultFileIOCallbacks

namespace moon
{

// ========================================================
// Local helper macros for error handling:
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
// FileIOCallbacks / DefaultFileIOCallbacks:
// ========================================================

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
    logStream() << "Moon: DefaultFileIOCallbacks - Opened script file \"" << scriptFile << "\"\n";
    #endif // MOON_DEBUG
    return succeeded;
}

void DefaultFileIOCallbacks::closeScript(std::istream ** stream)
{
    #if MOON_DEBUG
    logStream() << "Moon: DefaultFileIOCallbacks - Closing script file.\n";
    #endif // MOON_DEBUG

    delete (*stream);
    stream = nullptr;
}

FileIOCallbacks::~FileIOCallbacks()
{
    // This is here to anchor the vtable to this file,
    // preventing the 'weak-vtables' warning on Clang.
}

// ========================================================

// ========================================================
// Intermediate code generator context:
// ========================================================

struct CodeGen final
{
    //TODO move all the transient compilation states into this.
    //The only thing Compiler should hold on to is the SyntaxTree, SymbolTable
    //the instrPool, instr list pointers and maybe the Parser&Lexer.
    //
    //This can then be declared locally in the compile() method.
    //
};

static UInt32 getProgCodeIndex(const InstrMap & instrMapping, const IntermediateInstr * instr)
{
    MOON_ASSERT(instr != nullptr);

    auto && entry = instrMapping.find(instr);
    if (entry == std::end(instrMapping))
    {
        MOON_INTERNAL_EXCEPTION("instruction " + toString(instr->op) + " not found in InstructionMap");
    }
    return entry->second;
}

static UInt32 getProgDataIndex(const DataMap & dataMapping, const Symbol * symbol)
{
    MOON_ASSERT(symbol != nullptr);

    auto && entry = dataMapping.find(symbol);
    if (entry == std::end(dataMapping))
    {
        MOON_INTERNAL_EXCEPTION("symbol '" + toString(*symbol) + "' not found in DataMap");
    }
    return entry->second;
}

static Variant::Type varTypeForNodeEval(const Compiler & compiler, const SyntaxTreeNode * node)
{
    const SyntaxTreeNode::Eval stEval = node->evalType;
    if (stEval == SyntaxTreeNode::Eval::UDT)
    {
        const Symbol * symbol = node->symbol;
        if (symbol == nullptr)
        {
            symbol = node->getChildSymbol(1);
        }
        return compiler.symbolToTypeId(symbol) ? Variant::Type::Tid : Variant::Type::Object;
    }
    return eval2VarType(stEval);
}

static UInt32 addSymbolToProgData(VM & vm, const Symbol & symbol, const Variant::Type type)
{
    Variant var{}; // Default initialized to null.

    //FIXME temp
//    logStream() << "addSymbolToProgData: " << toString(symbol.name)
//        << ", " << toString(symbol.type) << ", " << toString(type) << "\n";

    // Identifies might be variables, function or user defined types.
    if (symbol.type == Symbol::Type::Identifier)
    {
        var.type = type;

        // Some of the types require special handing:
        if (type == Variant::Type::Function)
        {
            var.value.asFunction = vm.functions.findFunction(symbol.name);
            if (var.value.asFunction == nullptr)
            {
                // We should not hit this error. The parser should have already validated undefined func calls.
                //MOON_INTERNAL_EXCEPTION("referencing undefined function '" + toString(symbol.name) + "'");
                //
                //TODO the above causes problems with functions assigned to vars, but would
                //still be nice to check for null in here... the VM also checks against null Func pointers though...
            }
        }
        else if (type == Variant::Type::Tid)
        {
            var.value.asTypeId = vm.types.findTypeId(symbol.name);
            if (var.value.asTypeId == nullptr)
            {
                //MOON_INTERNAL_EXCEPTION("referencing undefined type '" + toString(symbol.name) + "'");
                //
                //TODO similar issue to the above. breaks when assigning tid to a var
            }
        }
    }
    else // Literal constants/strings:
    {
        var = symbol2Variant(vm, symbol);
    }

    vm.data.push_back(var);
    return static_cast<UInt32>(vm.data.size() - 1);
}

static void printIntermediateInstrListHelper(const IntermediateInstr * head, std::ostream & os)
{
    os << color::white() << "[[ begin intermediate instruction dump ]]" << color::restore() << "\n";

    int printedItems = 0;
    if (head != nullptr)
    {
        os << color::white() << "opcode count: " << static_cast<UInt32>(OpCode::Count)
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
    auto makeLoadInstr = [](Compiler & comp, const IntermediateInstr * inInstr) -> IntermediateInstr *
    {
        auto load = comp.newInstruction(inInstr);
        const bool isGlobal = (inInstr->op == OpCode::StoreGlobal);
        load->op = (isGlobal ? OpCode::LoadGlobal : OpCode::LoadLocal);
        return load;
    };

    // Must have the counter (i) and a for parameter.
    EXPECT_CHILD_AT_INDEX(root, 0);
    EXPECT_CHILD_AT_INDEX(root, 1);

    // The termination condition or collection to iterate (for-loop parameter):
    auto forParam = traverseTreeRecursive(compiler, root->getChild(1));
    if (root->getChild(1)->nodeType == SyntaxTreeNode::Type::ExprFuncCall)
    {
        forParam = linkInstr(forParam, compiler.newInstruction(OpCode::LoadRVR));
    }

    //
    // Iterator, internal index and stored parameter:
    //

    // 'i' iterator:
    auto forVars = traverseTreeRecursive(compiler, root->getChild(0));
    auto storeIter = forVars;

    IntermediateInstr * storeIterPrev  = nullptr;
    IntermediateInstr * storeIterSaved = nullptr;

    for (; storeIter->next != nullptr; storeIter = storeIter->next)
    {
        storeIterPrev = storeIter;
    }
    MOON_ASSERT(storeIter->op == OpCode::StoreLocal || storeIter->op == OpCode::StoreGlobal);
    storeIterSaved = storeIter;

    // The internal index '__for_idx__' is linked to the 'i' iterator:
    forVars = linkInstr(forVars, traverseTreeRecursive(compiler, root->getChild(0)->getChild(2)));
    auto storeIdx = forVars;
    for (; storeIdx->next != nullptr; storeIdx = storeIdx->next) { }
    MOON_ASSERT(storeIdx->op == OpCode::StoreLocal || storeIdx->op == OpCode::StoreGlobal);

    // The copy of the loop parameter/condition '__for_prm__' is linked to '__for_idx__':
    forVars = linkInstr(forVars, traverseTreeRecursive(compiler, root->getChild(0)->getChild(2)->getChild(2)));
    auto storeParam = forVars;
    for (; storeParam->next != nullptr; storeParam = storeParam->next) { }
    MOON_ASSERT(storeParam->op == OpCode::StoreLocal || storeParam->op == OpCode::StoreGlobal);

    // Assign the '__for_prm__'s initial value from stack top.
    auto forPrmInit = compiler.newInstruction(storeParam);

    //
    // ForLoopPrep:
    //

    // ForLoopPrep is preceded by loading the param, iterator and internal index:
    auto forPrep = compiler.newInstruction(OpCode::ForLoopPrep);
    forPrep = linkInstr(makeLoadInstr(compiler, storeParam), forPrep);
    forPrep = linkInstr(makeLoadInstr(compiler, storeIter),  forPrep);
    forPrep = linkInstr(makeLoadInstr(compiler, storeIdx),   forPrep);

    // After prepping, the updated values get written back to memory:
    forPrep = linkInstr(forPrep, compiler.newInstruction(storeIdx));
    forPrep = linkInstr(forPrep, compiler.newInstruction(storeIter));

    //
    // ForLoopTest:
    //

    // ForLoopTest requires loading the internal index and the loop param/termination cond:
    auto forTest = compiler.newInstruction(OpCode::ForLoopTest);
    forTest = linkInstr(makeLoadInstr(compiler, storeParam), forTest);
    forTest = linkInstr(makeLoadInstr(compiler, storeIdx),   forTest);

    //
    // ForLoopStep:
    //

    // Similarly to ForLoopPrep, we need the param, iterator and internal index.
    auto forStep = compiler.newInstruction(OpCode::ForLoopStep);
    forStep = linkInstr(makeLoadInstr(compiler, storeParam), forStep);
    forStep = linkInstr(makeLoadInstr(compiler, storeIter),  forStep);
    forStep = linkInstr(makeLoadInstr(compiler, storeIdx),   forStep);

    // After stepping, the updated values get written back to memory:
    forStep = linkInstr(forStep, compiler.newInstruction(storeIdx));
    forStep = linkInstr(forStep, compiler.newInstruction(storeIter));

    //
    // Wrap it up:
    //
    auto forCont = compiler.newInstruction(OpCode::Jmp, forStep);
    auto forEnd  = compiler.newInstruction(OpCode::NoOp);

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

    auto newStoreIter = compiler.newInstruction(storeIterSaved);
    newStoreIter->op  = (storeIterSaved->op == OpCode::StoreLocal ? OpCode::StoreSetTypeLocal : OpCode::StoreSetTypeGlobal);
    storeIterPrev->next = newStoreIter;

    return linkInstr(forVars, linkInstr(linkInstr(forParam, forPrmInit), linkInstr(block1, linkInstr(forCont, forEnd))));
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

static IntermediateInstr * getParamListChain(Compiler & compiler, const SyntaxTreeNode * root)
{
    return ((root->getChild(0) != nullptr) ? traverseTreeRecursive(compiler, root->getChild(0)) : nullptr);
}

template<OpCode OP>
static IntermediateInstr * emitUnaryOp(Compiler & compiler, const SyntaxTreeNode * root)
{
    // child 0 is reserved for parameter list chain in function/constructor calls.
    EXPECT_CHILD_AT_INDEX(root, 1);
    auto arg = traverseTreeRecursive(compiler, root->getChild(1));

    if (root->getChild(1)->nodeType == SyntaxTreeNode::Type::ExprFuncCall)
    {
        arg = linkInstr(arg, compiler.newInstruction(OpCode::LoadRVR));
    }

    auto operation = compiler.newInstruction(OP);
    return linkInstr(linkInstr(arg, operation), getParamListChain(compiler, root));
}

template<OpCode OP>
static IntermediateInstr * emitBinaryOp(Compiler & compiler, const SyntaxTreeNode * root)
{
    // child 0 is reserved for parameter list chain in function/constructor calls.
    EXPECT_CHILD_AT_INDEX(root, 1);
    EXPECT_CHILD_AT_INDEX(root, 2);
    auto arg0 = traverseTreeRecursive(compiler, root->getChild(1));
    auto arg1 = traverseTreeRecursive(compiler, root->getChild(2));

    // If either term is a func call, we must load from the Return Value Register.
    if (root->getChild(1)->nodeType == SyntaxTreeNode::Type::ExprFuncCall)
    {
        arg0 = linkInstr(arg0, compiler.newInstruction(OpCode::LoadRVR));
    }
    if (root->getChild(2)->nodeType == SyntaxTreeNode::Type::ExprFuncCall)
    {
        arg1 = linkInstr(arg1, compiler.newInstruction(OpCode::LoadRVR));
    }

    auto operation = compiler.newInstruction(OP);
    return linkInstr(linkInstr(linkInstr(arg0, arg1), operation), getParamListChain(compiler, root));
}

template<OpCode OP>
static IntermediateInstr * emitCompoundBinaryOp(Compiler & compiler, const SyntaxTreeNode * root)
{
    // child 0 is reserved for parameter list chain in function/constructor calls.
    EXPECT_CHILD_AT_INDEX(root, 1);
    EXPECT_CHILD_AT_INDEX(root, 2);

    const Symbol * targetSymbol;
    bool lhsIsArraySubscript;

    if (root->getChild(1)->nodeType == SyntaxTreeNode::Type::ExprArraySubscript)
    {
        targetSymbol = root->getChild(1)->getChildSymbol(1);
        lhsIsArraySubscript = true;
    }
    else
    {
        EXPECT_SYMBOL(root->getChild(1));
        targetSymbol = root->getChild(1)->symbol;
        lhsIsArraySubscript = false;
    }

    OpCode storeOp  = (!lhsIsArraySubscript ? OpCode::StoreGlobal : OpCode::StoreArraySubGlobal);
    OpCode loadOp   = OpCode::LoadGlobal;
    UInt16 paramIdx = InvalidParamIdx;

    if (targetSymbol->type == Symbol::Type::Identifier &&
        compiler.symbolIsFunctionLocal(targetSymbol, paramIdx))
    {
        storeOp = (!lhsIsArraySubscript ? OpCode::StoreLocal : OpCode::StoreArraySubLocal);
        loadOp  = OpCode::LoadLocal;
    }

    auto binOpInstr = compiler.newInstruction(OP);
    auto arg0 = traverseTreeRecursive(compiler, root->getChild(1));
    auto arg1 = traverseTreeRecursive(compiler, root->getChild(2));

    // If either term is a func call, we must load from the Return Value Register.
    if (root->getChild(1)->nodeType == SyntaxTreeNode::Type::ExprFuncCall)
    {
        arg0 = linkInstr(arg0, compiler.newInstruction(OpCode::LoadRVR));
    }
    if (root->getChild(2)->nodeType == SyntaxTreeNode::Type::ExprFuncCall)
    {
        arg1 = linkInstr(arg1, compiler.newInstruction(OpCode::LoadRVR));
    }

    if (root->getChild(1)->nodeType != SyntaxTreeNode::Type::ExprFuncCall)
    {
        auto storeOpInstr = compiler.newInstruction(storeOp, targetSymbol);
        storeOpInstr->paramIdx = paramIdx;

        // Load the subscript index right after if the left hand side is an array subscript.
        if (lhsIsArraySubscript)
        {
            auto subscriptOp = traverseTreeRecursive(compiler, root->getChild(1)->getChild(2));
            if (root->getChild(1)->getChild(2)->nodeType == SyntaxTreeNode::Type::ExprFuncCall)
            {
                // Subscript from function return value.
                auto getRVR = compiler.newInstruction(OpCode::LoadRVR);
                subscriptOp = linkInstr(subscriptOp, getRVR);
            }
            storeOpInstr = linkInstr(subscriptOp, storeOpInstr);
        }

        auto paramList = getParamListChain(compiler, root);
        if (paramList != nullptr)
        {
            // If the expression is being used on a parameter list we need to
            // load the symbol back into the stack after the write to memory.
            paramList = linkInstr(compiler.newInstruction(loadOp, targetSymbol), paramList);
        }

        return linkInstr(linkInstr(linkInstr(arg0, arg1), linkInstr(binOpInstr, storeOpInstr)), paramList);
    }
    else
    {
        // Silly "func() += xyz" kind of case, but we need to handle it nonetheless.
        // Just push the result back into the stack and be done with it.
        return linkInstr(linkInstr(linkInstr(arg0, arg1), binOpInstr), getParamListChain(compiler, root));
    }
}

static IntermediateInstr * emitLoad(Compiler & compiler, const SyntaxTreeNode * root)
{
    EXPECT_SYMBOL(root);
    auto symbol = root->symbol;

    OpCode op = OpCode::LoadGlobal;
    UInt16 paramIdx = InvalidParamIdx;

    // Booleans are converted to integer (0=false, 1=true).
    if (symbol->type == Symbol::Type::BoolLiteral)
    {
        symbol = compiler.symTable.findOrDefineIntLiteral(symbol->value.asBoolean ? 1 : 0);
    }
    else if (symbol->type == Symbol::Type::Identifier)
    {
        if (compiler.symbolIsFunctionLocal(symbol, paramIdx)) // A local variable or function parameter?
        {
            op = OpCode::LoadLocal;
        }
    }

    auto operation = compiler.newInstruction(op, symbol, eval2VarType(root->evalType));
    operation->paramIdx = paramIdx;

    // Function calls consist of a chain of load instructions followed by one CALL instruction.
    if (root->getChild(0) != nullptr)
    {
        auto argList = traverseTreeRecursive(compiler, root->getChild(0));
        return linkInstr(operation, argList); // <-- NOTE: Change this to alter func parameter passing order!
    }

    return operation;
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
        IntermediateInstr * last = argument;
        for (; last->next != nullptr; last = last->next) { }

        // Stupid "c = a += b" kid of code.
        // Need to load the result of the rhs expression back into the stack.
        // Reusing the last store op from the resolved argument for that.
        IntermediateInstr * tailLoad = compiler.newInstruction(OpCode::NoOp);
        tailLoad->op       = ((last->op == OpCode::StoreGlobal) ? OpCode::LoadGlobal : OpCode::LoadLocal);
        tailLoad->type     = last->type;
        tailLoad->paramIdx = last->paramIdx;
        tailLoad->operand  = last->operand;

        argument = linkInstr(argument, tailLoad);
    }

    return argument;
}

static IntermediateInstr * makeLoadMemberOffsetInstr(Compiler & compiler,
                                                     const Symbol * objSymbol,
                                                     const Symbol * memberSymbol,
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

    const int memberOffset = typeId->templateObject->findMemberIndex(memberSymbol->name);
    if (memberOffset < 0)
    {
        MOON_INTERNAL_EXCEPTION("object has no member '" + toString(memberSymbol->name) + "'");
    }

    if (typeIdOut)       { *typeIdOut       = typeId;       }
    if (memberOffsetOut) { *memberOffsetOut = memberOffset; }

    auto offsetLiteral = compiler.symTable.findOrDefineIntLiteral(memberOffset);
    return compiler.newInstruction(OpCode::LoadGlobal, offsetLiteral);
}

static void collectMemberRefs(std::vector<const Symbol *> & memberList, const SyntaxTreeNode * root)
{
    if (root == nullptr)
    {
        return;
    }

    collectMemberRefs(memberList, root->getChild(0));
    collectMemberRefs(memberList, root->getChild(1));

    if (root->symbol != nullptr)
    {
        memberList.push_back(root->symbol);
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
            lastMemberTypeId = member.data.value.asObject->getTypeId();
        }

        // [0:1] is the first obj.member pair. A chain might follow.
        for (UInt32 i = 2; i < compiler.memberRefList.size(); ++i)
        {
            MOON_ASSERT(lastMemberTypeId != nullptr);
            const Symbol * memberSymbol = compiler.memberRefList[i];

            lastMemberOffset = lastMemberTypeId->templateObject->findMemberIndex(memberSymbol->name);
            if (lastMemberOffset < 0)
            {
                MOON_INTERNAL_EXCEPTION("sub object of type '" + toString(lastMemberTypeId->name) +
                                        "' has no member '" + toString(memberSymbol->name) + "'");
            }

            auto offsetLiteral = compiler.symTable.findOrDefineIntLiteral(lastMemberOffset);
            auto loadMemOffs = compiler.newInstruction(OpCode::LoadGlobal, offsetLiteral);
            linkInstr(memOffsetInstr, loadMemOffs);

            const auto & subMember = lastMemberTypeId->templateObject->getMemberAt(lastMemberOffset);
            if (subMember.data.type == Variant::Type::Object)
            {
                lastMemberTypeId = subMember.data.value.asObject->getTypeId();
            }
        }
    }

    return memOffsetInstr;
}

static IntermediateInstr * emitStore(Compiler & compiler, const SyntaxTreeNode * root)
{
    EXPECT_CHILD_AT_INDEX(root, 1);
    EXPECT_CHILD_AT_INDEX(root, 2);

    UInt32 numMemberOffsets  = 0;
    const Symbol * objSymbol = nullptr;
    Variant::Type symbType   = Variant::Type::Null;
    bool isArraySubscript    = false;

    compiler.memberRefList.clear();
    if (root->getChild(1)->nodeType == SyntaxTreeNode::Type::ExprMemberRef)
    {
        collectMemberRefs(compiler.memberRefList, root->getChild(1));
        MOON_ASSERT(compiler.memberRefList.size() >= 2); // obj.member pair at least

        objSymbol = compiler.memberRefList[0];
        numMemberOffsets = compiler.memberRefList.size() - 1;
    }
    else if (root->getChild(1)->nodeType == SyntaxTreeNode::Type::ExprArraySubscript)
    {
        objSymbol = root->getChild(1)->getChildSymbol(1);
        isArraySubscript = true;
    }
    else
    {
        objSymbol = root->getChildSymbol(1);
    }
    MOON_ASSERT(objSymbol != nullptr);

    // Don't enter if doing a obj.member store
    if (root->evalType == SyntaxTreeNode::Eval::Undefined && numMemberOffsets == 0)
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

        symbType = typeId2VarType(typeId);
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
    UInt16 paramIdx = InvalidParamIdx;
    IntermediateInstr * memOffsetInstr = nullptr;
    IntermediateInstr * argument = traverseTreeRecursive(compiler, root->getChild(2));

    // We might need to load the rhs argument back up if this store
    // is part of a fancy composite operation like "c = (a += b)".
    argument = appendTrailingLoadOpIfNeeded(compiler, argument, root->getChild(2));

    // A local variable or function parameter?
    if (compiler.symbolIsFunctionLocal(objSymbol, paramIdx))
    {
        if (numMemberOffsets > 0)
        {
            memOffsetInstr = setUpMemberOffsetLoadChain(compiler, objSymbol);
            op = OpCode::MemberStoreLocal;
            symbType = Variant::Type::Object;
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
    else // Global symbol
    {
        if (numMemberOffsets > 0)
        {
            memOffsetInstr = setUpMemberOffsetLoadChain(compiler, objSymbol);
            op = OpCode::MemberStoreGlobal;
            symbType = Variant::Type::Object;
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

    if (numMemberOffsets > 0)
    {
        auto offsetCountLiteral = compiler.symTable.findOrDefineIntLiteral(numMemberOffsets);
        auto offsCount = compiler.newInstruction(OpCode::LoadGlobal, offsetCountLiteral);

        MOON_ASSERT(memOffsetInstr != nullptr);
        memOffsetInstr = linkInstr(memOffsetInstr, offsCount);
    }

    auto operation = compiler.newInstruction(op, objSymbol, symbType);
    operation->paramIdx = paramIdx;

    // Load the subscript right after.
    if (isArraySubscript)
    {
        auto subscriptOp = traverseTreeRecursive(compiler, root->getChild(1)->getChild(2));
        if (root->getChild(1)->getChild(2)->nodeType == SyntaxTreeNode::Type::ExprFuncCall)
        {
            // Subscript from function return value.
            auto getRVR = compiler.newInstruction(OpCode::LoadRVR);
            subscriptOp = linkInstr(subscriptOp, getRVR);
        }
        operation = linkInstr(subscriptOp, operation);
    }

    // Storing from the return value of a function references the Return Value Register (RVR).
    if (root->getChild(2)->nodeType == SyntaxTreeNode::Type::ExprFuncCall)
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
    // child 0 is reserved for parameter list chain in function/constructor calls.
    EXPECT_CHILD_AT_INDEX(root, 1);
    EXPECT_CHILD_AT_INDEX(root, 2);
    EXPECT_SYMBOL(root->getChild(2));

    auto objLoad = traverseTreeRecursive(compiler, root->getChild(1));
    const Symbol * objSymbol    = root->getChildSymbol(1);
    const Symbol * memberSymbol = root->getChildSymbol(2);

    IntermediateInstr * loadMemOffs;
    if (objSymbol != nullptr) // First pass
    {
        int memberOffset = 0;
        loadMemOffs = makeLoadMemberOffsetInstr(compiler, objSymbol, memberSymbol, &compiler.lastMemberTypeId, &memberOffset);

        const auto & member = compiler.lastMemberTypeId->templateObject->getMemberAt(memberOffset);
        if (member.data.type == Variant::Type::Object)
        {
            compiler.lastMemberTypeId = member.data.value.asObject->getTypeId();
        }
    }
    else // Going down a chain of obj.member.member.member ...
    {
        MOON_ASSERT(compiler.lastMemberTypeId != nullptr);
        const int memberOffset = compiler.lastMemberTypeId->templateObject->findMemberIndex(memberSymbol->name);
        if (memberOffset < 0)
        {
            MOON_INTERNAL_EXCEPTION("sub object of type '" + toString(compiler.lastMemberTypeId->name) +
                                    "' has no member '" + toString(memberSymbol->name) + "'");
        }

        auto offsetLiteral = compiler.symTable.findOrDefineIntLiteral(memberOffset);
        loadMemOffs = compiler.newInstruction(OpCode::LoadGlobal, offsetLiteral);

        const auto & member = compiler.lastMemberTypeId->templateObject->getMemberAt(memberOffset);
        if (member.data.type == Variant::Type::Object)
        {
            compiler.lastMemberTypeId = member.data.value.asObject->getTypeId();
        }
    }

    return linkInstr(linkInstr(linkInstr(objLoad, loadMemOffs), compiler.newInstruction(OpCode::MemberRef)), getParamListChain(compiler, root));
}

static IntermediateInstr * emitArraySubscript(Compiler & compiler, const SyntaxTreeNode * root)
{
    // child 0 is reserved for parameter lists and may be null.
    EXPECT_CHILD_AT_INDEX(root, 1);
    EXPECT_CHILD_AT_INDEX(root, 2);

    // If either term is a func call, we must load from the Return Value Register.
    auto arrayExpr = traverseTreeRecursive(compiler, root->getChild(1));
    if (root->getChild(1)->nodeType == SyntaxTreeNode::Type::ExprFuncCall)
    {
        arrayExpr = linkInstr(arrayExpr, compiler.newInstruction(OpCode::LoadRVR));
    }

    auto subscriptExpr = traverseTreeRecursive(compiler, root->getChild(2));
    if (root->getChild(2)->nodeType == SyntaxTreeNode::Type::ExprFuncCall)
    {
        subscriptExpr = linkInstr(subscriptExpr, compiler.newInstruction(OpCode::LoadRVR));
    }

    auto subscriptOp = compiler.newInstruction(OpCode::ArraySubscript);
    return linkInstr(arrayExpr, linkInstr(subscriptExpr, subscriptOp));
}

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

// doesn't seems to be necessary anymore...
//    if (!compiler.nodeWasVisited(root->getChild(1)))
//    {
//        countArgsRecursive(root->getChild(1), argCountOut, compiler);
//    }
}

// This function is used to resolve parameter chains for function calls,
// constructor calls and array literals. The OP template parameter is the
// instruction ending the list (i.e.: a Call, NewObj, NewArray, etc).
template<OpCode OP, Variant::Type OperandType>
static IntermediateInstr * emitParamChain(Compiler & compiler, const SyntaxTreeNode * root)
{
    UInt16 paramIdx = InvalidParamIdx;
    OpCode op = OP;

    // Check for the case where attempting an indirect function call
    // via a function parameter or local var of type 'function'.
    if (op == OpCode::Call && compiler.symbolIsFunctionLocal(root->symbol, paramIdx))
    {
        op = OpCode::CallLocal;
    }

    auto operation = compiler.newInstruction(op, root->symbol, OperandType);
    operation->paramIdx = paramIdx;

    // We have to keep track of the visited nodes to be able to properly
    // compute the argument counts. Some nodes will be visited more than
    // once due to the parameter list chains.
    compiler.clearVisited();

    int argumentCount = 0;
    IntermediateInstr * myArgs = nullptr;
    IntermediateInstr * argListChain = nullptr;

    // We might be linked to an parameter list chain, so go down the child 0 hierarchy:
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
    auto argCountLiteral = compiler.symTable.findOrDefineIntLiteral(argumentCount); //TODO this pattern is repeated in a couple places. make it a function!
    auto loadArgCountOp  = compiler.newInstruction(OpCode::LoadGlobal, argCountLiteral);

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
    auto funcStart = compiler.newInstruction(OpCode::FuncStart, root->symbol, Variant::Type::Function);
    auto funcEnd   = compiler.newInstruction(OpCode::FuncEnd, root->symbol);

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
        if (root->getChild(0)->nodeType != SyntaxTreeNode::Type::ExprFuncCall)
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
        if (root->evalType == SyntaxTreeNode::Eval::UDT)
        {
            // Find out the exact type of the UDT by looking at the constructor call:
            auto initExpr = root->getChild(0);
            MOON_ASSERT(initExpr != nullptr && "Expected a constructor call!");
            objectTypeSymbol = initExpr->symbol;
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
        argument = appendTrailingLoadOpIfNeeded(compiler, argument, root->getChild(0));
        argumentCount = 1;
    }

    const Symbol * symbol = root->symbol;
    OpCode op = OpCode::StoreGlobal;
    UInt16 paramIdx = InvalidParamIdx;

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
    auto argCountLiteral = compiler.symTable.findOrDefineIntLiteral(argumentCount); // TODO this pattern is repeated in a couple places!
    auto loadArgCountOp  = compiler.newInstruction(OpCode::LoadGlobal, argCountLiteral);

    if (argument != nullptr)
    {
        // Storing from the return value of a function references the Return Value Register (RVR).
        if (root->getChild(0)->nodeType == SyntaxTreeNode::Type::ExprFuncCall)
        {
            argument = linkInstr(argument, compiler.newInstruction(OpCode::LoadRVR));
        }

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

    return linkInstr(linkInstr(linkInstr(startExpr, endExpr), newRangeOp), getParamListChain(compiler, root));
}

static IntermediateInstr * emitMatchPrep(Compiler & compiler, const SyntaxTreeNode * root)
{
    EXPECT_CHILD_AT_INDEX(root, 0); // The param to match with

    auto matchParam = traverseTreeRecursive(compiler, root->getChild(0));
    auto matchEndOp = compiler.newInstruction(OpCode::MatchEnd);

    // Reusing the return anchor to mark the end of the match switch.
    compiler.setReturnAnchor(matchEndOp);
    auto matchBody = root->getChild(1) ? traverseTreeRecursive(compiler, root->getChild(1)) : nullptr;
    compiler.clearReturnAnchor();

    return linkInstr(linkInstr(matchParam, matchBody), matchEndOp);
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

static IntermediateInstr * emitTypecast(Compiler & compiler, const SyntaxTreeNode * root)
{
    EXPECT_CHILD_AT_INDEX(root, 1);
    EXPECT_CHILD_AT_INDEX(root, 2);

    // Second child must be a type identifier
    EXPECT_SYMBOL(root->getChild(2));
    auto loadTypeIdOp = compiler.newInstruction(OpCode::LoadGlobal,
                  root->getChild(2)->symbol, Variant::Type::Tid);

    auto argument   = traverseTreeRecursive(compiler, root->getChild(1));
    auto typecastOp = compiler.newInstruction(OpCode::Typecast);

    return linkInstr(linkInstr(linkInstr(argument, loadTypeIdOp), typecastOp), getParamListChain(compiler, root));
}

static IntermediateInstr * emitTypeof(Compiler & compiler, const SyntaxTreeNode * root)
{
    IntermediateInstr * typeofInstr;

    EXPECT_CHILD_AT_INDEX(root, 1);
    const SyntaxTreeNode * child1 = root->getChild(1);

    const bool nodeIsTypeId  = (child1->nodeType == SyntaxTreeNode::Type::ExprTypeIdent);
    const bool nodeIsLiteral = (child1->nodeType == SyntaxTreeNode::Type::ExprLiteralConst);
    const bool symbIsTypeId  = (child1->symbol && compiler.symbolToTypeId(child1->symbol));
    const bool isNullLiteral = (nodeIsLiteral && child1->evalType == SyntaxTreeNode::Eval::Null);

    // Simple case of 'type_of(TYPE_NAME)/type_of(123)' => just load the TypeId directly.
    // Note that 'null' is a special case we can't just load the TypeId directly.
    // 'null' behaves like an identifier and a literal constant at the same time,
    // so the easiest way to handle 'type_of(null)' is to just do the runtime resolution.
    if (!isNullLiteral && (nodeIsTypeId || nodeIsLiteral || symbIsTypeId))
    {
        const Symbol * typeSymbol = (nodeIsLiteral ? eval2Symbol(compiler.symTable, child1->evalType) : child1->symbol);
        typeofInstr = compiler.newInstruction(OpCode::LoadGlobal, typeSymbol, Variant::Type::Tid);
    }
    else // Resolve the argument expression first and emit a Typeof opcode:
    {
        IntermediateInstr * argument = traverseTreeRecursive(compiler, child1);

        // If either term is a func call, we must load from the Return Value Register.
        if (child1->nodeType == SyntaxTreeNode::Type::ExprFuncCall)
        {
            argument = linkInstr(argument, compiler.newInstruction(OpCode::LoadRVR));
        }
        typeofInstr = linkInstr(argument, compiler.newInstruction(OpCode::Typeof));
    }

    return linkInstr(typeofInstr, getParamListChain(compiler, root));
}

// ----------------------------------------------------------------------------
// emitInstrCallbacks[]:
//
// The handlers for each SyntaxTreeNode::Type (AKA STNode).
// Some will emit instructions and possibly recurse by calling
// traverseTreeRecursive() again with the child nodes of the subtree.
// ----------------------------------------------------------------------------
//TODO note: should avoid emitting those no-ops for type nodes and such...
static const EmitInstrForNodeCB emitInstrCallbacks[]
{
    &emitNOOP,                                              // NoOp
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

// Calls the appropriate handler according to the node type (which in turn might call this function again)
static IntermediateInstr * traverseTreeRecursive(Compiler & compiler, const SyntaxTreeNode * root)
{
    MOON_ASSERT(root != nullptr);
    const auto handlerIndex  = static_cast<UInt32>(root->nodeType);
    MOON_ASSERT(handlerIndex < static_cast<UInt32>(SyntaxTreeNode::Type::Count));

    /*
    //FIXME temp
    if (root->symbol)
    {
        logStream() << "visiting tnode: " << toString(root->nodeType) << " (" << toString(root->symbol->name) << ")\n";
    }
    else
    {
        logStream() << "visiting tnode: " << toString(root->nodeType) << "\n";
    }
    //*/

    return emitInstrCallbacks[handlerIndex](compiler, root);
}

// ========================================================
// Compiler class methods:
// ========================================================

void Compiler::compile(VM & vm)
{
    runtimeTypes = &vm.types;
    globCodeListHead = traverseTreeRecursive(*this, syntTree.getRoot());
    intermediateToVM(vm);
}

void Compiler::print(std::ostream & os) const
{
    os << "Intermediate instructions total: " << toString(instructionCount) << "\n\n";

    os << "Global code:\n";
    printIntermediateInstrListHelper(globCodeListHead, os);

    os << "\nFunction code:\n";
    printIntermediateInstrListHelper(funcCodeListHead,  os);
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
        typeSymbol = root->getChild(1)->symbol;
    }

    if (typeSymbol == nullptr)
    {
        typeSymbol = eval2Symbol(symTable, root->evalType);
        if (root->evalType == SyntaxTreeNode::Eval::UDT)
        {
            // Find out the exact type of the UDT by looking at the constructor call:
            auto initExpr = root->getChild(0);
            MOON_ASSERT(initExpr != nullptr && "Expected a constructor call!");
            objectTypeSymbol = initExpr->symbol;
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

    addFunctionLocalSymbol(root->symbol, guessTypeId(root));
    collectFunctionArgSymbols(root->getChild(0));
}

void Compiler::collectFunctionVarSymbols(const SyntaxTreeNode * root)
{
    if (root == nullptr)
    {
        return;
    }

    if (root->nodeType == SyntaxTreeNode::Type::VarDeclStatement)
    {
        addFunctionLocalSymbol(root->symbol, guessTypeId(root));
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
//    MOON_ASSERT(!findFunctionLocalSymbolTypeId(symbol));

    //TODO checking shouldn't be needed once we merge with the VarInfoTable.
    //This is needed right now because of the for-loop iterators.
    if (findFunctionLocalSymbolTypeId(symbol))
    {
        return;
    }

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

bool Compiler::symbolIsFunctionLocal(const Symbol * symbol, UInt16 & paramIdx) const
{
    const UInt32 symbolCount = funcLocalIdentifiers.size();
    for (UInt32 index = 0; index < symbolCount; ++index)
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
    createMappings(vm, globCodeListHead, /* skipFunctions = */ true);

    // Pad the end with an instruction to anchor any potential jumps to the
    // end of the program, since we have removed all the other noop anchors.
    vm.code.push_back(packInstruction(OpCode::NoOp,    0));
    vm.code.push_back(packInstruction(OpCode::ProgEnd, 0));

    // Now we perform the same mapping for the function that got set aside in the previous step.
    createMappings(vm, funcCodeListHead, /* skipFunctions = */ false);

    // And map back the instructions into the code vector:
    IntermediateInstr * instr;
    for (instr = globCodeListHead; instr != nullptr; instr = instr->next)
    {
        fixReferences(instr, vm.code, vm.functions);
    }
    for (instr = funcCodeListHead; instr != nullptr; instr = instr->next)
    {
        fixReferences(instr, vm.code, vm.functions);
    }
}

void Compiler::createMappings(VM & vm, IntermediateInstr * listHead, const bool skipFunctions)
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
                funcCodeListHead = funcListTail = instr;
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
            // References any global program data? (Jumps reference other instructions)
            if (instr->operand.symbol != nullptr && instr->paramIdx == InvalidParamIdx && !isJumpOpCode(instr->op))
            {
                // Each symbol is added once.
                if (dataMapping.find(instr->operand.symbol) == std::end(dataMapping))
                {
                    auto index = addSymbolToProgData(vm, *(instr->operand.symbol), instr->type);
                    dataMapping.emplace(instr->operand.symbol, index);
                }
            }

            // Operand index is set later. For now this instruction is just a placeholder.
            vm.code.push_back(packInstruction(instr->op, 0));
            instrMapping.emplace(instr, vm.code.size() - 1);
        }
        else
        {
            // The noop doesn't get inserted, so this instruction maps to the
            // index of the next instruction to be added to the vm.code vector.
            instrMapping.emplace(instr, vm.code.size());
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
            const UInt32 selfCodeIdx = getProgCodeIndex(instrMapping, instr);
            const UInt32 operandDataIndex = getProgDataIndex(dataMapping, instr->operand.symbol);
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
            const UInt32 selfCodeIdx = getProgCodeIndex(instrMapping, instr);
            UInt32 operandCodeIdx = getProgCodeIndex(instrMapping, instr->operand.jumpTarget);

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
    // Instructions referencing local function-level data:
    case OpCode::LoadLocal :
    case OpCode::StoreLocal :
    case OpCode::MemberStoreLocal :
    case OpCode::StoreArraySubLocal :
    case OpCode::StoreSetTypeLocal :
        {
            // Index of this instruction and its data operand:
            const UInt32 selfCodeIdx = getProgCodeIndex(instrMapping, instr);

            // Either grab the data from global prog memory or function stack (paramIdx):
            const UInt32 operandDataIndex = ((instr->paramIdx == InvalidParamIdx) ?
                                             getProgDataIndex(dataMapping, instr->operand.symbol) :
                                             instr->paramIdx);

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

// ========================================================
// Intermediate Instruction node allocation:
// ========================================================

IntermediateInstr * Compiler::newInstruction(const OpCode op)
{
    auto instr                = instrPool.allocate();
    instr->next               = nullptr;
    instr->operand.symbol     = nullptr;
    instr->uid                = instructionCount++;
    instr->paramIdx           = InvalidParamIdx;
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
    instr->paramIdx           = InvalidParamIdx;
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
    instr->paramIdx           = InvalidParamIdx;
    instr->type               = Variant::Type::Null;
    instr->op                 = op;
    return instr;
}

IntermediateInstr * Compiler::newInstruction(const IntermediateInstr * copy)
{
    auto instr                = instrPool.allocate();
    *instr                    = *copy;
    instr->next               = nullptr;
    instr->uid                = instructionCount++;
    return instr;
}

} // namespace moon {}
