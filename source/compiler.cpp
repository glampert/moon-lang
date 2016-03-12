
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

IntermediateInstr * emitCall(Compiler & compiler, const SyntaxTreeNode * root)
{
    expectSymbol(root);
    auto operation = compiler.newInstruction(OpCode::Call, root->getSymbol());

    // Resolve the potential function argument list:
    if (root->getChild(0) != nullptr)
    {
        // function_id => arg_0 => arg_1 => arg_N
        auto argList = traverseTreeRecursive(compiler, root->getChild(0));

        // Child node 0 will have a linked list with the provided arguments:
        int argumentsProvided = 0;
        for (auto c = root->getChild(0); c != nullptr; c = c->getChild(0))
        {
            ++argumentsProvided;
        }

        // Argument count is appended as a literal integer:
        auto argCountLiteral = compiler.symTable.findOrDefineValue(argumentsProvided);
        auto loadArgCountOp  = compiler.newInstruction(OpCode::Load, argCountLiteral);
        return linkInstr(argList, linkInstr(loadArgCountOp, operation));
    }
    else // Function called with zero arguments.
    {
        auto argCountLiteral = compiler.symTable.findOrDefineValue(0);
        auto loadArgCountOp  = compiler.newInstruction(OpCode::Load, argCountLiteral);
        return linkInstr(loadArgCountOp, operation);
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
    &emitNOOP,                              // ForStatement
    &emitNOOP,                              // MatchStatement
    &emitNOOP,                              // MatchCaseStatement
    &emitNOOP,                              // MatchDefaultStatement
    &emitNOOP,                              // FuncDeclStatement
    &emitNOOP,                              // EnumDeclStatement
    &emitNOOP,                              // StructDeclStatement
    &emitNOOP,                              // TypeAliasDeclStatement
    &emitNOOP,                              // VarDeclStatement
    &emitNOOP,                              // ReturnStatement
    &emitBreak,                             // BreakStatement
    &emitContinue,                          // ContinueStatement
    &emitNOOP,                              // ExprRange
    &emitNOOP,                              // ExprArrayLiteral
    &emitNOOP,                              // ExprArraySubscript
    &emitCall,                              // ExprFuncCall
    &emitLoad,                              // ExprNameIdent
    &emitNOOP,                              // ExprTypeIdent
    &emitLoad,                              // ExprLiteralConst
    &emitNOOP,                              // ExprObjectConstructor
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

Compiler::Compiler(SymbolTable & symtab, SyntaxTree & ast)
    : symTable           { symtab  }
    , syntTree           { ast     }
    , firstInstr         { nullptr }
    , instructionCount   { 0       }
    , lastLoopStartLabel { nullptr }
    , lastLoopEndLabel   { nullptr }
{ }

void Compiler::compile(VM::DataVector & progData, VM::CodeVector & progCode)
{
    firstInstr = traverseTreeRecursive(*this, syntTree.getRoot());
    intermediateToVM(progData, progCode);
}

void Compiler::printIntermediateInstructions(std::ostream & os) const
{
    printIntermediateInstrListHelper(firstInstr, os);
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

#if 0
Compiler::IntermediateInstr * Compiler::traverseTreeRecursive(const SyntaxTreeNode * root)
{
    MOON_ASSERT(root != nullptr);

    const STNode nodeType = root->getType();

    // Common statements/expressions/temps:
    IntermediateInstr * block0        = nullptr;
    IntermediateInstr * block1        = nullptr;
    IntermediateInstr * block2        = nullptr;

    // Used to assemble conditional statements:
    IntermediateInstr * noop          = nullptr;
    IntermediateInstr * condition     = nullptr;
    IntermediateInstr * thenStatement = nullptr;
    IntermediateInstr * elseStatement = nullptr;
    IntermediateInstr * jumpIfFalse   = nullptr;
    IntermediateInstr * jumpToEnd     = nullptr;

    switch (nodeType)
    {
    case STNode::TranslationUnit :
        {
            block0 = newInstruction(OpCode::ModuleStart);
            if (root->getChild(0) != nullptr) // A translation unit might be empty.
            {
                block1 = traverseTreeRecursive(root->getChild(0));
                return linkInstr(block0, block1);
            }
            return block0;
        }
    case STNode::Statement :
        {
            expectChildAtIndex(root, 1);
            if (root->getChild(0) != nullptr)
            {
                block0 = traverseTreeRecursive(root->getChild(0));
                block1 = traverseTreeRecursive(root->getChild(1));
                return linkInstr(block0, block1);
            }
            else
            {
                return traverseTreeRecursive(root->getChild(1));
            }
        }
    case STNode::IfThenStatement :
        {
            // If the then-statement ('if' body) is not empty:
            if (root->getChild(1) != nullptr)
            {
                expectNumChildren(root, 2);
                condition     = traverseTreeRecursive(root->getChild(0));
                thenStatement = traverseTreeRecursive(root->getChild(1));
                noop          = newInstruction(OpCode::NoOp);
                jumpIfFalse   = newInstruction(OpCode::JmpIfFalse, noop);

                // if-cond => jump-end-if-false => if-body => noop[jump-target/end]
                block0 = linkInstr(condition, jumpIfFalse);
                block1 = linkInstr(block0, thenStatement);
                return linkInstr(block1, noop);
            }
            else // Empty then-statement/body:
            {
                condition   = traverseTreeRecursive(root->getChild(0));
                noop        = newInstruction(OpCode::NoOp);
                jumpIfFalse = newInstruction(OpCode::JmpIfFalse, noop);

                // if-cond => jump-end-if-false => noop[jump-target/end]
                block0 = linkInstr(condition, jumpIfFalse);
                return linkInstr(block0, noop);
            }
        }
    case STNode::IfThenElseStatement :
        {
            // child[0] = condition/expression (must have)
            // child[1] = if-body/then-statement (may be empty)
            // child[2] = else-body/statement (may be empty)

            // Must have a if-condition part:
            expectChildAtIndex(root, 0);
            condition = traverseTreeRecursive(root->getChild(0));

            // If the then-statement body and else-statement body are not empty, resolve them:
            if (root->getChild(1) != nullptr)
            {
                thenStatement = traverseTreeRecursive(root->getChild(1));
            }
            if (root->getChild(2) != nullptr)
            {
                elseStatement = traverseTreeRecursive(root->getChild(2));
            }

            noop        = newInstruction(OpCode::NoOp);
            jumpToEnd   = newInstruction(OpCode::Jmp, noop);
            jumpIfFalse = newInstruction(OpCode::JmpIfFalse, (elseStatement != nullptr) ? elseStatement : jumpToEnd);

            // Handle the instruction sequence differently if the if or else bodies are empty:
            block0 = linkInstr(condition, jumpIfFalse);
            if (thenStatement != nullptr)
            {
                block1 = linkInstr(block0, thenStatement);
                block2 = linkInstr(block1, jumpToEnd);
            }
            else
            {
                block2 = linkInstr(block0, jumpToEnd);
            }

            if (elseStatement != nullptr)
            {
                return linkInstr(linkInstr(block2, elseStatement), noop);
            }
            else
            {
                return linkInstr(block2, noop);
            }
            // if-cond => jump-else-if-false => if-body =>
            //     jump-end => else-body => noop[jump-target/end]
        }
    case STNode::IfThenElseIfStatement :
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
            condition = traverseTreeRecursive(root->getChild(0));

            // If the then-statement body and elseif body are not empty, resolve them:
            if (root->getChild(1) != nullptr)
            {
                thenStatement = traverseTreeRecursive(root->getChild(1));
            }
            if (root->getChild(2) != nullptr)
            {
                elseStatement = traverseTreeRecursive(root->getChild(2)); // else|elseif
            }

            // Note: This will produce a noop at the end for each of the
            // if/elseif conditions in the chain. Those can be consolidated into
            // a single noop/jump-target by the final bytecode compilation stage.
            noop        = newInstruction(OpCode::NoOp);
            jumpToEnd   = newInstruction(OpCode::Jmp, noop);
            jumpIfFalse = newInstruction(OpCode::JmpIfFalse, (elseStatement != nullptr) ? elseStatement : jumpToEnd);

            block0 = linkInstr(condition, jumpIfFalse);
            block1 = linkInstr(block0, thenStatement);
            block2 = linkInstr(block1, jumpToEnd);
            return linkInstr(linkInstr(block2, elseStatement), noop);

            // if|elseif-cond => jump-next-if-false => if|elseif-body =>
            //     jump-end => [repeat till an else] => noop[jump-target/end]
        }
    case STNode::VarDeclStatement :
        {
            //TEMP
            if (root->getEval() == SyntaxTreeNode::Eval::Int)
            {
                return newInstruction(OpCode::IntNew);
            }
            else
            {
                MOON_ASSERT(0); //TODO
            }
        }

//  ReturnStatement

    case STNode::ExprFuncCall :
        {
            int functionArgCount = 0;

            expectSymbol(root);
            block0 = newInstruction(OpCode::Call, root->getSymbol());

            // Resolve the potential function parameters:
            if (root->getChild(0) != nullptr)
            {
                // function_id => parameter_0 => parameter_1 => parameter_N
                block1 = traverseTreeRecursive(root->getChild(0));

                //FIXME don't think this is enough, might need to test the other children too?
                for (auto c = root->getChild(0); c != nullptr; c = c->getChild(0))
                {
                    ++functionArgCount;
                }

                block2 = newInstruction(OpCode::IntLoad, symTable.findOrDefineValue(functionArgCount));
                return linkInstr(block1, linkInstr(block2, block0));
            }
            else // Function with zero parameters/arguments.
            {
                block2 = newInstruction(OpCode::IntLoad, symTable.findOrDefineValue(0));
                return linkInstr(block2, block0);
            }
        }
    case STNode::ExprNameIdent :
    case STNode::ExprLiteralConst :
        {
            //TODO other data types!
            expectSymbol(root);
            block0 = newInstruction(OpCode::IntLoad, root->getSymbol());

            // !!! for functions !!!
            if (root->getChild(0) != nullptr)
            {
                block1 = traverseTreeRecursive(root->getChild(0));
                return linkInstr(block0, block1); // <-- CHANGE THIS TO ALTER FUNC PARAMETER PASSING ORDER!
            }
            else
            {
                return block0;
            }
        }
    case STNode::ExprAssign :
        {
            //TODO other data types (not just Int)!
            expectNumChildren(root, 2);
            expectSymbol(root->getChild(0));
            block0 = traverseTreeRecursive(root->getChild(1));
            block1 = newInstruction(OpCode::IntStore, root->getChildSymbol(0));
            return linkInstr(block0, block1);
        }

    // All binary expressions are implemented the same way, except
    // for the instructions emitted. Simplify the code with a
    // little bit of preprocessor black magic ;)
    #define CASE_BINARY_EXPR(nodeType, opType)                 \
    case STNode::nodeType :                                   \
        do                                                     \
        {                                                      \
            expectNumChildren(root, 2);                        \
            block0 = traverseTreeRecursive(root->getChild(0)); \
            block1 = traverseTreeRecursive(root->getChild(1)); \
            block2 = newInstruction(OpCode::opType);           \
            return linkInstr(linkInstr(block0, block1), block2);     \
        } while (0)
    CASE_BINARY_EXPR( ExprCmpNotEqual,     IntCmpNotEq        );
    CASE_BINARY_EXPR( ExprCmpEqual,        IntCmpEq           );
    CASE_BINARY_EXPR( ExprCmpGreaterEqual, IntCmpGreaterEqual );
    CASE_BINARY_EXPR( ExprCmpGreaterThan,  IntCmpGreater      );
    CASE_BINARY_EXPR( ExprCmpLessEqual,    IntCmpLessEqual    );
    CASE_BINARY_EXPR( ExprCmpLessThan,     IntCmpLess         );
    CASE_BINARY_EXPR( ExprSubtract,        IntSub             );
    CASE_BINARY_EXPR( ExprAdd,             IntAdd             );
    CASE_BINARY_EXPR( ExprModulo,          IntMod             );
    CASE_BINARY_EXPR( ExprDivide,          IntDiv             );
    CASE_BINARY_EXPR( ExprMultiply,        IntMul             );
    #undef CASE_BINARY_EXPR

    default :
        //TODO
        break;
    } // switch (nodeType)

    MOON_ASSERT(0); //TODO throw a CompilerError or something...
}
#endif//0

void Compiler::fixReferences(const IntermediateInstr * instr, VM::DataVector & progData, VM::CodeVector & progCode)
{
    MOON_ASSERT(instr != nullptr);

//////
    (void)progData;

    switch (instr->op)
    {
    //
    // -- Miscellaneous:
    //
//  case OpCode::NoOp :
//  case OpCode::ModuleStart :

    //
    // -- Jump instructions:
    //
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

    //
    // -- Integer instructions:
    //
//  case OpCode::IntNew :

    //case OpCode::IntLoad  :
    //case OpCode::IntStore :
        // this rule can actually be shared by several instruction types...
    case OpCode::Call :
    //case OpCode::CallNative :
        {
            // Index of this instruction and its data operand:
            auto selfCodeIdx      = getProgCodeIndex(instrMapping, instr);
            auto operandDataIndex = getProgDataIndex(dataMapping, instr->operand.symbol);

            MOON_ASSERT(selfCodeIdx < progCode.size() && "Index out-of-bounds!");
            progCode[selfCodeIdx] = packInstruction(instr->op, operandDataIndex);
            break;
        }

//  case OpCode::IntCmpNotEq :
//  case OpCode::IntCmpEq :
//  case OpCode::IntCmpGreaterEqual :
//  case OpCode::IntCmpGreater :
//  case OpCode::IntCmpLessEqual :
//  case OpCode::IntCmpLess :

//  case OpCode::IntSub :
//  case OpCode::IntAdd :
//  case OpCode::IntMod :
//  case OpCode::IntDiv :
//  case OpCode::IntMul :

    default :
        //MOON_ASSERT(0);//TODO
        break;
    } // switch (instr->op)
}

void Compiler::intermediateToVM(VM::DataVector & progData, VM::CodeVector & progCode)
{
    // We might still eliminate a few noops, but it should
    // be worth while reserving the memory beforehand anyway.
    progCode.reserve(instructionCount);

    for (auto instr = firstInstr; instr != nullptr; instr = instr->next)
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
    for (auto instr = firstInstr; instr != nullptr; instr = instr->next)
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
