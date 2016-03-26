
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

void internalError(const std::string & errorMessage, const int srcLineNum)
{
    const std::string errorTag = color::red() + toString("internal compiler error:") + color::restore();
    const std::string message  = toString(__FILE__) + "(" + toString(srcLineNum) + "): " + errorTag + " " + errorMessage;

    std::cerr << message << std::endl;
    throw CompilerException{ "Internal compiler error" };
}

std::uint32_t getProgCodeIndex(const InstructionMap & instrMapping, const IntermediateInstr * instr)
{
    MOON_ASSERT(instr != nullptr);

    auto && entry = instrMapping.find(instr);
    if (entry == std::end(instrMapping))
    {
        internalError("instruction " + toString(instr->op) + " not found in InstructionMap", __LINE__);
    }
    return entry->second;
}

std::uint32_t getProgDataIndex(const DataMap & dataMapping, const Symbol * sym)
{
    MOON_ASSERT(sym != nullptr);

    auto && entry = dataMapping.find(sym);
    if (entry == std::end(dataMapping))
    {
        internalError("symbol " + toString(*sym) + " not found in DataMap", __LINE__);
    }
    return entry->second;
}

Variant::Type varTypeForNodeEval(const SyntaxTreeNode * node)
{
    const auto eval = node->getEval();
    switch (eval)
    {
    case SyntaxTreeNode::Eval::Int    : return Variant::Type::Integer;
    case SyntaxTreeNode::Eval::Long   : return Variant::Type::Integer;
    case SyntaxTreeNode::Eval::Float  : return Variant::Type::Float;
    case SyntaxTreeNode::Eval::Bool   : return Variant::Type::Integer;
    case SyntaxTreeNode::Eval::String : return Variant::Type::String;
    default                           : return Variant::Type::Null;
    } // switch (eval)
}

std::uint32_t addSymbolToProgData(VM::DataVector & progData, FunctionTable & funcTable,
                                  const Symbol & sym, const Variant::Type type)
{
    Variant var{}; // Default initialized to null.

    // Identifies might be variables, function or user defined types.
    if (sym.type == Symbol::Type::Identifier && !sym.isBuiltInTypeId())
    {
        var.type = type;

        // Some of the types require special handing:
        if (type == Variant::Type::Function)
        {
            var.value.asFunctionPtr = funcTable.findFunction(sym.name);
            if (var.value.asFunctionPtr == nullptr)
            {
                // We should not hit this error. The parser should have already validated undefined func calls.
                internalError("referencing undefined function '" + toString(sym.name) + "'", __LINE__);
            }
        }
    }
    else // Literal constants/strings:
    {
        var = Variant::fromSymbol(sym);
    }

    progData.push_back(var);
    return static_cast<std::uint32_t>(progData.size() - 1);
}

void expectNumChildren(const SyntaxTreeNode * node, const int expected)
{
    MOON_ASSERT(node != nullptr);
    for (int n = 0; n < expected; ++n)
    {
        if (node->getChild(n) == nullptr)
        {
            internalError("expected " + toString(expected) + " child nodes, got " + toString(n), __LINE__);
        }
    }
}

void expectChildAtIndex(const SyntaxTreeNode * node, const int childIndex)
{
    MOON_ASSERT(node != nullptr);
    if (node->getChild(childIndex) == nullptr)
    {
        internalError("expected node for child index " + toString(childIndex), __LINE__);
    }
}

void expectSymbol(const SyntaxTreeNode * node)
{
    MOON_ASSERT(node != nullptr);
    if (node->getSymbol() == nullptr)
    {
        internalError("expected symbol for node " + toString(node->getType()), __LINE__);
    }
}

void printIntermediateInstrListHelper(const IntermediateInstr * head, std::ostream & os)
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
                    os << " \'" << color::magenta() << unescapeString(toString(*(instr->operand.symbol)).c_str())
                       << color::restore() << "\'";
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

IntermediateInstr * emitProgStart(Compiler & compiler, const SyntaxTreeNode * root)
{
    auto block0 = compiler.newInstruction(OpCode::ProgStart);
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

// The left-hand-side operand flag is set to true for the
// compound op+store opcodes, e.g.: AddStore, SubStore, etc.
// Those must also reference the target symbol that is to receive
// the result of the operation.
template<OpCode OP, bool lhsOperand = false>
IntermediateInstr * emitBinaryOp(Compiler & compiler, const SyntaxTreeNode * root)
{
    expectNumChildren(root, 2);
    auto arg0 = traverseTreeRecursive(compiler, root->getChild(0));
    auto arg1 = traverseTreeRecursive(compiler, root->getChild(1));
    auto operation = compiler.newInstruction(OP, lhsOperand ? root->getChild(0)->getSymbol() : nullptr);
    return linkInstr(linkInstr(arg0, arg1), operation);
}

IntermediateInstr * emitLoad(Compiler & compiler, const SyntaxTreeNode * root)
{
    expectSymbol(root);
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

IntermediateInstr * emitStore(Compiler & compiler, const SyntaxTreeNode * root)
{
    expectNumChildren(root, 2);
    expectSymbol(root->getChild(0));
    auto symbol    = root->getChildSymbol(0);
    auto symbType  = varTypeForNodeEval(root);
    auto argument  = traverseTreeRecursive(compiler, root->getChild(1));

    OpCode op = OpCode::StoreGlob;
    std::uint16_t paramIdx = 0;

    if (symbol->type == Symbol::Type::Identifier &&
        compiler.symbolIsFunctionLocal(symbol, paramIdx)) // A local variable or function parameter?
    {
        op = OpCode::StoreLocal;
    }

    auto operation = compiler.newInstruction(op, symbol, symbType);
    operation->paramIdx = paramIdx;

    // Storing from the return value of a function references the Return Value Register (RVR).
    if (root->getChild(1)->getType() == STNode::ExprFuncCall)
    {
        auto getRVR = compiler.newInstruction(OpCode::LoadRVR);
        return linkInstr(argument, linkInstr(getRVR, operation));
    }
    else // Normal store:
    {
        return linkInstr(argument, operation);
    }
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

//FIXME the argument counting is still broken.
// it will count unrelated nodes like operators.
// e.g.: println("a" + to_string("b")) would yield 3 instead of 2
void countArgsRecursive(const SyntaxTreeNode * root, int & argCountOut, Compiler & compiler)
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
template<OpCode OP>
IntermediateInstr * emitParamChain(Compiler & compiler, const SyntaxTreeNode * root)
{
    auto operation = compiler.newInstruction(OP, root->getSymbol(), Variant::Type::Function);//FIXME probably not Function for everything!!!

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

IntermediateInstr * emitFunctionDecl(Compiler & compiler, const SyntaxTreeNode * root)
{
    expectSymbol(root); // Func name
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

IntermediateInstr * emitReturn(Compiler & compiler, const SyntaxTreeNode * root)
{
    MOON_ASSERT(compiler.getReturnAnchor() != nullptr);
    auto retJump = compiler.newInstruction(OpCode::JmpReturn, compiler.getReturnAnchor());

    // Return simply resolves its child expression if it has one.
    if (root->getChild(0) != nullptr)
    {
        auto retExpr = traverseTreeRecursive(compiler, root->getChild(0));

        // We only need to set the Return Value Register when the return
        // expression is not itself another call. If it is a call, the
        // leaf function in the call tree will set the register.
        if (root->getChild(0)->getType() != STNode::ExprFuncCall)
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

    const Symbol * symbol = root->getSymbol();
    OpCode op = OpCode::StoreGlob;
    std::uint16_t paramIdx = 0;

    if (symbol->type == Symbol::Type::Identifier &&
        compiler.symbolIsFunctionLocal(symbol, paramIdx)) // A local variable or function parameter?
    {
        op = OpCode::StoreLocal;
    }

    auto newOp   = compiler.newInstruction(OpCode::NewVar, type);
    auto storeOp = compiler.newInstruction(op, symbol, varTypeForNodeEval(root));
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

IntermediateInstr * emitNewRange(Compiler & compiler, const SyntaxTreeNode * root)
{
    expectChildAtIndex(root, 1); // range start
    expectChildAtIndex(root, 2); // range end

    auto startExpr  = traverseTreeRecursive(compiler, root->getChild(1));
    auto endExpr    = traverseTreeRecursive(compiler, root->getChild(2));
    auto newRangeOp = compiler.newInstruction(OpCode::NewRange);

    return linkInstr(linkInstr(startExpr, endExpr), newRangeOp);
}

IntermediateInstr * emitMatchPrep(Compiler & compiler, const SyntaxTreeNode * root)
{
    expectChildAtIndex(root, 0); // The param to match with

    auto matchParam  = traverseTreeRecursive(compiler, root->getChild(0));
    auto matchPrepOp = compiler.newInstruction(OpCode::MatchPrep);
    auto matchEndOp  = compiler.newInstruction(OpCode::NoOp);

    // Reusing the return anchor to mark the end of the match switch.
    compiler.setReturnAnchor(matchEndOp);
    auto matchBody = root->getChild(1) ? traverseTreeRecursive(compiler, root->getChild(1)) : nullptr;
    compiler.clearReturnAnchor();

    return linkInstr(linkInstr(linkInstr(matchParam, matchPrepOp), matchBody), matchEndOp);
}

IntermediateInstr * emitMatchTest(Compiler & compiler, const SyntaxTreeNode * root)
{
    MOON_ASSERT(compiler.getReturnAnchor() != nullptr);
    expectChildAtIndex(root, 0); // The expression to match against

    auto caseExpr     = traverseTreeRecursive(compiler, root->getChild(0));
    auto caseBody     = root->getChild(1) ? traverseTreeRecursive(compiler, root->getChild(1)) : nullptr;
    auto caseEnd      = compiler.newInstruction(OpCode::NoOp);
    auto caseEndJump  = compiler.newInstruction(OpCode::JmpIfFalse, caseEnd);
    auto matchEndJump = compiler.newInstruction(OpCode::Jmp, compiler.getReturnAnchor());
    auto matchTestOp  = compiler.newInstruction(OpCode::MatchTest);

    auto block0 = linkInstr(linkInstr(caseExpr, matchTestOp), caseEndJump);
    auto block1 = linkInstr(linkInstr(block0, caseBody), matchEndJump);
    auto siblingCase = root->getChild(2) ? traverseTreeRecursive(compiler, root->getChild(2)) : nullptr;

    return linkInstr(linkInstr(block1, caseEnd), siblingCase);
}

IntermediateInstr * emitMatchDefault(Compiler & compiler, const SyntaxTreeNode * root)
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
    &emitProgStart,                         // TranslationUnit
    &emitNOOP,                              // ModuleDefinition
    &emitStatement,                         // Statement
    &emitIfThen,                            // IfThenStatement
    &emitIfThenElse,                        // IfThenElseStatement
    &emitIfThenElseIf,                      // IfThenElseIfStatement
    &emitLoop,                              // LoopStatement
    &emitWhileLoop,                         // WhileStatement
    &emitForLoop,                           // ForStatement
    &emitMatchPrep,                         // MatchStatement
    &emitMatchTest,                         // MatchCaseStatement
    &emitMatchDefault,                      // MatchDefaultStatement
    &emitFunctionDecl,                      // FuncDeclStatement
    &emitNOOP,                              // EnumDeclStatement
    &emitNOOP,                              // StructDeclStatement
    &emitNOOP,                              // TypeAliasDeclStatement
    &emitNewVar,                            // VarDeclStatement
    &emitReturn,                            // ReturnStatement
    &emitBreak,                             // BreakStatement
    &emitContinue,                          // ContinueStatement
    &emitNewRange,                          // ExprRange
    &emitParamChain<OpCode::NewArray>,      // ExprArrayLiteral
    &emitArraySubscript,                    // ExprArraySubscript
    &emitParamChain<OpCode::Call>,          // ExprFuncCall
    &emitLoad,                              // ExprNameIdent
    &emitNOOP,                              // ExprTypeIdent
    &emitLoad,                              // ExprLiteralConst
    &emitParamChain<OpCode::NewObj>,        // ExprObjectConstructor
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
    &emitBinaryOp<OpCode::SubStore, true>,  // ExprSubAssign
    &emitBinaryOp<OpCode::AddStore, true>,  // ExprAddAssign
    &emitBinaryOp<OpCode::ModStore, true>,  // ExprModAssign
    &emitBinaryOp<OpCode::DivStore, true>,  // ExprDivAssign
    &emitBinaryOp<OpCode::MulStore, true>,  // ExprMulAssign
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
    , funcListHead       { nullptr }
    , lastLoopStartLabel { nullptr }
    , lastLoopEndLabel   { nullptr }
    , lastFuncEndLabel   { nullptr }
{ }

void Compiler::compile(VM & vm)
{
    instrListHead = traverseTreeRecursive(*this, syntTree.getRoot());
    intermediateToVM(vm.data, vm.code, vm.functions);
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

void Compiler::collectFunctionArgSymbols(const SyntaxTreeNode * root)
{
    if (root == nullptr)
    {
        return;
    }
    addFunctionLocalSymbol(root->getSymbol());
    collectFunctionArgSymbols(root->getChild(0));
}

void Compiler::collectFunctionVarSymbols(const SyntaxTreeNode * root)
{
    if (root == nullptr)
    {
        return;
    }
    if (root->getType() == STNode::VarDeclStatement)
    {
        addFunctionLocalSymbol(root->getSymbol());
    }
    collectFunctionVarSymbols(root->getChild(0));
    collectFunctionVarSymbols(root->getChild(1));
}

void Compiler::beginFunction(const IntermediateInstr * endLabel, const SyntaxTreeNode * root)
{
    if (getReturnAnchor() != nullptr)
    {
        internalError("Nested functions not supported yet!", __LINE__);
    }

    collectFunctionArgSymbols(root->getChild(0));
    collectFunctionVarSymbols(root->getChild(1));
    setReturnAnchor(endLabel);
}

bool Compiler::symbolIsFunctionLocal(const Symbol * symbol, std::uint16_t & paramIdx)
{
    const int symbolCount = static_cast<int>(funcScopeIdentifiers.size());
    for (int index = 0; index < symbolCount; ++index)
    {
        if (funcScopeIdentifiers[index] == symbol)
        {
            paramIdx = index;
            return true;
        }
    }
    return false;
}

void Compiler::addFunctionLocalSymbol(const Symbol * symbol)
{
    funcScopeIdentifiers.push_back(symbol);
}

void Compiler::endFunction()
{
    clearReturnAnchor();
    funcScopeIdentifiers.clear();
}

void Compiler::intermediateToVM(VM::DataVector & progData, VM::CodeVector & progCode, FunctionTable & funcTable)
{
    // We might still eliminate a few noops, but it should
    // be worthwhile reserving the memory beforehand anyway.
    progCode.reserve(instructionCount);

    // This step removes duplicate data and unneeded noops from the intermediate code.
    // It also serves to separate the function definition from the rest of the code.
    createMappings(progData, progCode, funcTable, instrListHead, /* skipFunctions = */ true);

    // Pad the end with an instruction to anchor any potential jumps to the
    // end of the program, since we have removed all the other noop anchors.
    progCode.push_back(packInstruction(OpCode::NoOp,    0));
    progCode.push_back(packInstruction(OpCode::ProgEnd, 0));

    // Now we perform the same mapping for the function that got set aside in the previous step.
    createMappings(progData, progCode, funcTable, funcListHead, /* skipFunctions = */ false);

    // And map back the instructions into the progCode vector:
    IntermediateInstr * instr;
    for (instr = instrListHead; instr != nullptr; instr = instr->next)
    {
        fixReferences(instr, progCode, funcTable);
    }
    for (instr = funcListHead; instr != nullptr; instr = instr->next)
    {
        fixReferences(instr, progCode, funcTable);
    }
}

void Compiler::createMappings(VM::DataVector & progData, VM::CodeVector & progCode,
                              FunctionTable & funcTable, IntermediateInstr * listHead,
                              const bool skipFunctions)
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
                    auto index = addSymbolToProgData(progData, funcTable, *(instr->operand.symbol), instr->type);
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
    case OpCode::FuncEnd   :
        {
            // Index of this instruction and its data operand (the function object in funcTable):
            auto selfCodeIdx = getProgCodeIndex(instrMapping, instr);
            auto operandDataIndex = getProgDataIndex(dataMapping, instr->operand.symbol);
            MOON_ASSERT(selfCodeIdx < progCode.size() && "Index out-of-bounds!");

            const char * funcName = instr->operand.symbol->name;
            const auto funcObj = funcTable.findFunction(funcName);

            if (funcObj == nullptr)
            {
                internalError("missing function table entry for '" +
                              toString(funcName) + "'", __LINE__);
            }
            if (instr->op == OpCode::FuncStart)
            {
                funcTable.setJumpTargetFor(funcName, selfCodeIdx);
            }
            progCode[selfCodeIdx] = packInstruction(instr->op, operandDataIndex);
            break;
        }
    // Instructions that reference a code address:
    case OpCode::Jmp        :
    case OpCode::JmpIfFalse :
    case OpCode::JmpIfTrue  :
    case OpCode::JmpReturn  :
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
    case OpCode::NewVar    :
    case OpCode::LoadGlob  :
    case OpCode::StoreGlob :
    case OpCode::SubStore  ://FIXME would need the local/param handling as well
    case OpCode::AddStore  :// just remove and switch to compound load a,b + op + store?
    case OpCode::ModStore  :
    case OpCode::DivStore  :
    case OpCode::MulStore  :
    case OpCode::Call      :
        {
            // Index of this instruction and its data operand:
            auto selfCodeIdx = getProgCodeIndex(instrMapping, instr);
            auto operandDataIndex = getProgDataIndex(dataMapping, instr->operand.symbol);
            MOON_ASSERT(selfCodeIdx < progCode.size() && "Index out-of-bounds!");
            progCode[selfCodeIdx] = packInstruction(instr->op, operandDataIndex);
            break;
        }
    // Instructions referencing local function-level data:
    case OpCode::LoadLocal  :
    case OpCode::StoreLocal :
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

Compiler::IntermediateInstr * Compiler::newInstruction(const OpCode op)
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

Compiler::IntermediateInstr * Compiler::newInstruction(const OpCode op, const Symbol * symbol, const Variant::Type type)
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

Compiler::IntermediateInstr * Compiler::newInstruction(const OpCode op, const IntermediateInstr * jumpTarget)
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
