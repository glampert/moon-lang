
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
namespace
{

// ========================================================
// Local helpers:
// ========================================================

using IntermediateInstr = Compiler::IntermediateInstr;
using InstructionMap    = Compiler::InstructionMap;
using DataMap           = Compiler::DataMap;
using STNode            = SyntaxTreeNode::Type;

// NOTES:
// The exceptions we throw in here are only meant to shield
// from internal programming errors. If we hit one, there's
// a bug in the compiler. The syntax tree should already
// represent a 100% valid program when we get here.

std::uint32_t getProgCodeIndex(const InstructionMap & instrMapping, const IntermediateInstr * instr)
{
    MOON_ASSERT(instr != nullptr);

    auto && entry = instrMapping.find(instr);
    if (entry == std::end(instrMapping))
    {
        throw CompilerException{ "Instruction " + toString(instr->op) + " not found in InstructionMap" };
    }
    return entry->second;
}

std::uint32_t getProgDataIndex(const DataMap & dataMapping, const Symbol * sym)
{
    MOON_ASSERT(sym != nullptr);

    auto && entry = dataMapping.find(sym);
    if (entry == std::end(dataMapping))
    {
        throw CompilerException{ "Symbol " + toString(*sym) + " not found in DataMap" };
    }
    return entry->second;
}

std::uint32_t addSymbolToProgData(VM::DataVector & progData, const Symbol & sym)
{
    progData.push_back(Variant::fromSymbol(sym));
    return static_cast<std::uint32_t>(progData.size() - 1);
}

void expectNumChildren(const SyntaxTreeNode * node, const int expected)
{
    MOON_ASSERT(node != nullptr);
    for (int n = 0; n < expected; ++n)
    {
        if (node->getChild(n) == nullptr)
        {
            throw CompilerException{ "Expected " + toString(expected) + " child nodes, got " + toString(n) };
        }
    }
}

void expectChildAtIndex(const SyntaxTreeNode * node, const int childIndex)
{
    MOON_ASSERT(node != nullptr);
    if (node->getChild(childIndex) == nullptr)
    {
        throw CompilerException{ "Expected node for child index " + toString(childIndex) };
    }
}

void expectSymbol(const SyntaxTreeNode * node)
{
    MOON_ASSERT(node != nullptr);
    if (node->getSymbol() == nullptr)
    {
        throw CompilerException{ "Expected symbol for node " + toString(node->getType()) };
    }
}

void printIntermediateInstrListHelper(const IntermediateInstr * head, std::ostream & os)
{
    os << color::white() << "[[ begin intermediate instruction dump ]]" << color::restore() << "\n";

    int printedItems = 0;
    if (head != nullptr)
    {
        os << color::white() << "Op-code count: " << static_cast<unsigned>(OpCode::Count)
           << color::restore() << "\n";

        for (auto instr = head; instr != nullptr; instr = instr->next)
        {
            os << color::cyan() << "[ " << std::setw(3) << instr->uid << " ] "
               << color::red() << toString(instr->op) << color::restore();

            if (!isJumpInstruction(instr->op))
            {
                if (instr->operand.symbol != nullptr)
                {
                    os << " \'" << color::magenta() << toString(*(instr->operand.symbol)) << color::restore() << "\'";
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

void printInstructionMappingHelper(const InstructionMap & instrMapping, std::ostream & os)
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
IntermediateInstr * traverseTreeRecursive(Compiler & compiler, const SyntaxTreeNode * root);

IntermediateInstr * linkInstr(IntermediateInstr * head, IntermediateInstr * newTail)
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

IntermediateInstr * emitNOOP(Compiler & compiler, const SyntaxTreeNode *)
{
    // For the node types that generate no code.
    return compiler.newInstruction(OpCode::NoOp);
}

IntermediateInstr * emitModuleStart(Compiler & compiler, const SyntaxTreeNode * root)
{
    auto block0 = compiler.newInstruction(OpCode::ModuleStart);
    if (root->getChild(0) != nullptr) // A translation unit might be empty.
    {
        auto block1 = traverseTreeRecursive(compiler, root->getChild(0));
        return linkInstr(block0, block1);
    }
    return block0;
}

IntermediateInstr * emitStatement(Compiler & compiler, const SyntaxTreeNode * root)
{
    expectChildAtIndex(root, 1);
    if (root->getChild(0) != nullptr)
    {
        auto block0 = traverseTreeRecursive(compiler, root->getChild(0));
        auto block1 = traverseTreeRecursive(compiler, root->getChild(1));
        return linkInstr(block0, block1);
    }
    return traverseTreeRecursive(compiler, root->getChild(1));
}

IntermediateInstr * emitIfThen(Compiler & compiler, const SyntaxTreeNode * root)
{
    // If the then-statement ('if' body) is not empty:
    if (root->getChild(1) != nullptr)
    {
        expectNumChildren(root, 2);
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

IntermediateInstr * emitIfThenElse(Compiler & compiler, const SyntaxTreeNode * root)
{
    // child[0] = condition/expression (must have)
    // child[1] = if-body/then-statement (may be empty)
    // child[2] = else-body/statement (may be empty)

    // Must have a if-condition part:
    expectChildAtIndex(root, 0);
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

IntermediateInstr * emitIfThenElseIf(Compiler & compiler, const SyntaxTreeNode * root)
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
    expectChildAtIndex(root, 0);
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

IntermediateInstr * emitLoop(Compiler & compiler, const SyntaxTreeNode * root)
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

IntermediateInstr * emitWhileLoop(Compiler & compiler, const SyntaxTreeNode * root)
{
    // The conditional expression:
    expectChildAtIndex(root, 0);
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

IntermediateInstr * emitForLoop(Compiler & compiler, const SyntaxTreeNode * root)
{
    // Must have the counter (i) and a range/call/name expression:
    expectChildAtIndex(root, 0);
    expectChildAtIndex(root, 1);
    expectSymbol(root->getChild(0));

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

IntermediateInstr * emitBreak(Compiler & compiler, const SyntaxTreeNode *)
{
    MOON_ASSERT(compiler.getLoopEndAnchor() != nullptr);
    return compiler.newInstruction(OpCode::Jmp, compiler.getLoopEndAnchor());
}

IntermediateInstr * emitContinue(Compiler & compiler, const SyntaxTreeNode *)
{
    MOON_ASSERT(compiler.getLoopStartAnchor() != nullptr);
    return compiler.newInstruction(OpCode::Jmp, compiler.getLoopStartAnchor());
}

template<OpCode OP>
IntermediateInstr * emitUnaryOp(Compiler & compiler, const SyntaxTreeNode * root)
{
    expectNumChildren(root, 1);
    auto argument  = traverseTreeRecursive(compiler, root->getChild(0));
    auto operation = compiler.newInstruction(OP);
    return linkInstr(argument, operation);
}

template<OpCode OP>
IntermediateInstr * emitBinaryOp(Compiler & compiler, const SyntaxTreeNode * root)
{
    expectNumChildren(root, 2);
    auto arg0 = traverseTreeRecursive(compiler, root->getChild(0));
    auto arg1 = traverseTreeRecursive(compiler, root->getChild(1));
    auto operation = compiler.newInstruction(OP);
    return linkInstr(linkInstr(arg0, arg1), operation);
}

IntermediateInstr * emitLoad(Compiler & compiler, const SyntaxTreeNode * root)
{
    expectSymbol(root);
    auto operation = compiler.newInstruction(OpCode::Load, root->getSymbol());

    // Function calls consist of a chain of load instructions followed by one CALL instruction.
    if (root->getChild(0) != nullptr)
    {
        auto argList = traverseTreeRecursive(compiler, root->getChild(0));
        return linkInstr(operation, argList); // <-- NOTE: Change this to alter func parameter passing order!
    }
    return operation;
}

IntermediateInstr * emitStore(Compiler & compiler, const SyntaxTreeNode * root)
{
    expectNumChildren(root, 2);
    expectSymbol(root->getChild(0));
    auto operation = compiler.newInstruction(OpCode::Store, root->getChildSymbol(0));
    auto argument  = traverseTreeRecursive(compiler, root->getChild(1));
    return linkInstr(argument, operation);
}

IntermediateInstr * emitArraySubscript(Compiler & compiler, const SyntaxTreeNode * root)
{
    // child[0] is reserved for parameter lists and may be null.
    expectChildAtIndex(root, 1);
    expectChildAtIndex(root, 2);
    auto arrayExpr     = traverseTreeRecursive(compiler, root->getChild(1));
    auto subscriptExpr = traverseTreeRecursive(compiler, root->getChild(2));
    auto subscriptOp   = compiler.newInstruction(OpCode::ArraySubscript);
    return linkInstr(arrayExpr, linkInstr(subscriptExpr, subscriptOp));
}

void countArgsRecursive(const SyntaxTreeNode * root, int & argCountOut, Compiler & compiler)
{
    if (root == nullptr)
    {
        return;
    }

    ++argCountOut;
    compiler.markVisited(root);

    if (!compiler.nodeVisited(root->getChild(0)))
    {
        countArgsRecursive(root->getChild(0), argCountOut, compiler);
    }
    if (!compiler.nodeVisited(root->getChild(1)))
    {
        countArgsRecursive(root->getChild(1), argCountOut, compiler);
    }
}

template<OpCode OP>
IntermediateInstr * emitCallChain(Compiler & compiler, const SyntaxTreeNode * root)
{
    expectSymbol(root);
    auto operation = compiler.newInstruction(OP, root->getSymbol());

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
    auto loadArgCountOp  = compiler.newInstruction(OpCode::Load, argCountLiteral);

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

IntermediateInstr * emitFunctionDecl(Compiler & compiler, const SyntaxTreeNode * root)
{
    expectSymbol(root); // Func name
    auto funcStart = compiler.newInstruction(OpCode::FuncStart, root->getSymbol());
    auto funcEnd   = compiler.newInstruction(OpCode::FuncEnd);

    // Resolve the function body if any. Parameter list and return type are not relevant here.
    compiler.setReturnAnchor(funcEnd);
    auto funcBody = ((root->getChild(1) != nullptr) ?
                     traverseTreeRecursive(compiler, root->getChild(1)) :
                     compiler.newInstruction(OpCode::NoOp));
    compiler.clearReturnAnchor();

    // Chain everything together:
    return linkInstr(funcStart, linkInstr(funcBody, funcEnd));
}

IntermediateInstr * emitReturn(Compiler & compiler, const SyntaxTreeNode * root)
{
    MOON_ASSERT(compiler.getFuncEndAnchor() != nullptr);

    // Return simply resolves its child expression if it has one.
    auto retExpr = ((root->getChild(0) != nullptr) ?
                    traverseTreeRecursive(compiler, root->getChild(0)) :
                    compiler.newInstruction(OpCode::NoOp));

    auto retJump = compiler.newInstruction(OpCode::JmpReturn, compiler.getFuncEndAnchor());
    return linkInstr(retExpr, retJump);
}

IntermediateInstr * emitNewVar(Compiler & compiler, const SyntaxTreeNode * root)
{
    expectSymbol(root); // Var name

    const Symbol * type = nullptr;
    IntermediateInstr * argument = nullptr;

    if (root->getChild(1) != nullptr) // Type node
    {
        type = root->getChild(1)->getSymbol();
    }
    if (type == nullptr)
    {
        type = symbolFromEval(compiler.symTable, root->getEval());
    }

    int argumentCount = 0;
    if (root->getChild(0) != nullptr) // Initializer expression node
    {
        argument = traverseTreeRecursive(compiler, root->getChild(0));
        argumentCount = 1;
    }

    auto newOp   = compiler.newInstruction(OpCode::NewVar, type);
    auto storeOp = compiler.newInstruction(OpCode::Store, root->getSymbol());

    // A load instruction is appended to indicate if we should pop one
    // value from the stack to initialize the new var or if it was left
    // uninitialized.
    auto argCountLiteral = compiler.symTable.findOrDefineValue(argumentCount);
    auto loadArgCountOp  = compiler.newInstruction(OpCode::Load, argCountLiteral);

    if (argument != nullptr)
    {
        return linkInstr(linkInstr(argument, linkInstr(loadArgCountOp, newOp)), storeOp);
    }
    else
    {
        return linkInstr(linkInstr(loadArgCountOp, newOp), storeOp);
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
    &emitModuleStart,                       // TranslationUnit
    &emitNOOP,                              // ModuleDefinition
    &emitStatement,                         // Statement
    &emitIfThen,                            // IfThenStatement
    &emitIfThenElse,                        // IfThenElseStatement
    &emitIfThenElseIf,                      // IfThenElseIfStatement
    &emitLoop,                              // LoopStatement
    &emitWhileLoop,                         // WhileStatement
    &emitForLoop,                           // ForStatement
    &emitNOOP,                              // MatchStatement
    &emitNOOP,                              // MatchCaseStatement
    &emitNOOP,                              // MatchDefaultStatement
    &emitFunctionDecl,                      // FuncDeclStatement
    &emitNOOP,                              // EnumDeclStatement
    &emitNOOP,                              // StructDeclStatement
    &emitNOOP,                              // TypeAliasDeclStatement
    &emitNewVar,                            // VarDeclStatement
    &emitReturn,                            // ReturnStatement
    &emitBreak,                             // BreakStatement
    &emitContinue,                          // ContinueStatement
    &emitNOOP,                              // ExprRange
    &emitNOOP,                              // ExprArrayLiteral
    &emitArraySubscript,                    // ExprArraySubscript
    &emitCallChain<OpCode::Call>,           // ExprFuncCall
    &emitLoad,                              // ExprNameIdent
    &emitNOOP,                              // ExprTypeIdent
    &emitLoad,                              // ExprLiteralConst
    &emitCallChain<OpCode::NewObj>,         // ExprObjectConstructor
    &emitStore,                             // ExprAssign
    &emitBinaryOp<OpCode::CmpNotEqual>,     // ExprCmpNotEqual
    &emitBinaryOp<OpCode::CmpEqual>,        // ExprCmpEqual
    &emitBinaryOp<OpCode::CmpGreaterEqual>, // ExprCmpGreaterEqual
    &emitBinaryOp<OpCode::CmpGreater>,      // ExprCmpGreaterThan
    &emitBinaryOp<OpCode::CmpLessEqual>,    // ExprCmpLessEqual
    &emitBinaryOp<OpCode::CmpLess>,         // ExprCmpLessThan
    &emitBinaryOp<OpCode::LogicOr>,         // ExprLogicOr
    &emitBinaryOp<OpCode::LogicAnd>,        // ExprLogicAnd
    &emitUnaryOp<OpCode::LogicNot>,         // ExprLogicNot
    &emitBinaryOp<OpCode::Sub>,             // ExprSubtract
    &emitBinaryOp<OpCode::Add>,             // ExprAdd
    &emitBinaryOp<OpCode::Mod>,             // ExprModulo
    &emitBinaryOp<OpCode::Div>,             // ExprDivide
    &emitBinaryOp<OpCode::Mul>,             // ExprMultiply
    &emitBinaryOp<OpCode::SubAssign>,       // ExprSubAssign
    &emitBinaryOp<OpCode::AddAssign>,       // ExprAddAssign
    &emitBinaryOp<OpCode::ModAssign>,       // ExprModAssign
    &emitBinaryOp<OpCode::DivAssign>,       // ExprDivAssign
    &emitBinaryOp<OpCode::MulAssign>,       // ExprMulAssign
    &emitUnaryOp<OpCode::Negate>,           // ExprUnaryMinus
    &emitUnaryOp<OpCode::Plus>              // ExprUnaryPlus
};
static_assert(arrayLength(emitInstrCallbacks) == unsigned(STNode::Count),
              "Keep this array in sync with the enum declaration!");

// Calls the appropriate handler according to the node type (which in turn might call this function again)
IntermediateInstr * traverseTreeRecursive(Compiler & compiler, const SyntaxTreeNode * root)
{
    MOON_ASSERT(root != nullptr);
    const auto handlerIndex  = static_cast<unsigned>(root->getType());
    MOON_ASSERT(handlerIndex < static_cast<unsigned>(STNode::Count));
    return emitInstrCallbacks[handlerIndex](compiler, root);
}

} // namespace {}

// ========================================================
// Compiler class methods:
// ========================================================

Compiler::Compiler()
    : instructionCount   { 0       }
    , instrListHead      { nullptr }
    , lastLoopStartLabel { nullptr }
    , lastLoopEndLabel   { nullptr }
    , lastFuncEndLabel   { nullptr }
{ }

void Compiler::compile(VM & vm)
{
    instrListHead = traverseTreeRecursive(*this, syntTree.getRoot());
    intermediateToVM(vm.data, vm.code);
}

void Compiler::printIntermediateInstructions(std::ostream & os) const
{
    printIntermediateInstrListHelper(instrListHead, os);
}

void Compiler::printInstructionMapping(std::ostream & os) const
{
    printInstructionMappingHelper(instrMapping, os);
}

std::ostream & operator << (std::ostream & os, const Compiler & compiler)
{
    compiler.printIntermediateInstructions(os);
    os << "\n";
    compiler.printInstructionMapping(os);
    return os;
}

void Compiler::setLoopAnchors(const IntermediateInstr * startLabel, const IntermediateInstr * endLabel) noexcept
{
    lastLoopStartLabel = startLabel;
    lastLoopEndLabel   = endLabel;
}

void Compiler::clearLoopAnchors() noexcept
{
    lastLoopStartLabel = nullptr;
    lastLoopEndLabel   = nullptr;
}

void Compiler::setReturnAnchor(const IntermediateInstr * endLabel) noexcept
{
    lastFuncEndLabel = endLabel;
}

void Compiler::clearReturnAnchor() noexcept
{
    lastFuncEndLabel = nullptr;
}

void Compiler::markVisited(const SyntaxTreeNode * node)
{
    visitedNodes.push_back(node);
}

bool Compiler::nodeVisited(const SyntaxTreeNode * node) const
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

void Compiler::fixReferences(const IntermediateInstr * instr, VM::DataVector & progData, VM::CodeVector & progCode)
{
    MOON_ASSERT(instr != nullptr);

    (void)progData;
    //TODO NOTE: we need to update function definitions in the
    //global function table with the jump address of each script method!!!

    switch (instr->op)
    {
    //case OpCode::FuncDecl : //TODO

    case OpCode::Jmp        :
    case OpCode::JmpIfTrue  :
    case OpCode::JmpIfFalse :
        {
            // Index of this instruction and its jump target:
            auto selfCodeIdx    = getProgCodeIndex(instrMapping, instr);
            auto operandCodeIdx = getProgCodeIndex(instrMapping, instr->operand.jumpTarget);

            // Clamp if this instruction jumps to the end of the program.
            // (happens to removed noops that pointed to the end of a block/if-statement).
            operandCodeIdx = std::min(operandCodeIdx, static_cast<std::uint32_t>(progCode.size() - 1));

            MOON_ASSERT(selfCodeIdx < progCode.size() && "Index out-of-bounds!");
            progCode[selfCodeIdx] = packInstruction(instr->op, operandCodeIdx);
            break;
        }

    // this rule can actually be shared by several instruction types...
    //case OpCode::Load  :
    //case OpCode::Store :
    case OpCode::Call :
        {
            // Index of this instruction and its data operand:
            auto selfCodeIdx      = getProgCodeIndex(instrMapping, instr);
            auto operandDataIndex = getProgDataIndex(dataMapping, instr->operand.symbol);

            MOON_ASSERT(selfCodeIdx < progCode.size() && "Index out-of-bounds!");
            progCode[selfCodeIdx] = packInstruction(instr->op, operandDataIndex);
            break;
        }

    default :
        break;
    } // switch (instr->op)
}

void Compiler::intermediateToVM(VM::DataVector & progData, VM::CodeVector & progCode)
{
    // We might still eliminate a few noops, but it should
    // be worthwhile reserving the memory beforehand anyway.
    progCode.reserve(instructionCount);

    for (auto instr = instrListHead; instr != nullptr; instr = instr->next)
    {
        // On our setup, noops are just placeholders for
        // the jump targets. They can be eliminated now.
        if (instr->op != OpCode::NoOp)
        {
            // References any data?
            if (!isJumpInstruction(instr->op) && instr->operand.symbol != nullptr)
            {
                // Each symbol is added once.
                if (dataMapping.find(instr->operand.symbol) == std::end(dataMapping))
                {
                    auto index = addSymbolToProgData(progData, *(instr->operand.symbol));
                    dataMapping.emplace(instr->operand.symbol, index);
                }
            }

            // Operand index and size are set later.
            // For now this instruction is just a placeholder.
            progCode.push_back(packInstruction(instr->op, 0));
            instrMapping.emplace(instr, progCode.size() - 1);
        }
        else
        {
            // The noop doesn't get inserted, so this instruction maps to the
            // index of the next instruction to be added to the progCode vector.
            instrMapping.emplace(instr, progCode.size());
        }
    }

    // Pad the end with one noop to anchor any potential jumps to the
    // end of the program, since we have removed all the other noops.
    progCode.push_back(packInstruction(OpCode::NoOp, 0));

    // Now map back the data and instructions into the progCode and progData:
    for (auto instr = instrListHead; instr != nullptr; instr = instr->next)
    {
        fixReferences(instr, progData, progCode);
    }
}

// ========================================================
// Intermediate Instruction node allocation:
// ========================================================

Compiler::IntermediateInstr * Compiler::newInstruction(const OpCode op)
{
    auto instr                = instrPool.allocate();
    instr->next               = nullptr;
    instr->operand.symbol     = nullptr;
    instr->uid                = instructionCount++;
    instr->op                 = op;
    return instr;
}

Compiler::IntermediateInstr * Compiler::newInstruction(const OpCode op, const Symbol * symbol)
{
    auto instr                = instrPool.allocate();
    instr->next               = nullptr;
    instr->operand.symbol     = symbol;
    instr->uid                = instructionCount++;
    instr->op                 = op;
    return instr;
}

Compiler::IntermediateInstr * Compiler::newInstruction(const OpCode op, const IntermediateInstr * jumpTarget)
{
    auto instr                = instrPool.allocate();
    instr->next               = nullptr;
    instr->operand.jumpTarget = jumpTarget;
    instr->uid                = instructionCount++;
    instr->op                 = op;
    return instr;
}

} // namespace moon {}
