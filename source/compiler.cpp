
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

IntermediateInstr * concat(IntermediateInstr * block0, IntermediateInstr * block1)
{
    // Instruction chains where we use this function are short.
    // The longest would be from a big if/elseif construct, which
    // shouldn't be longer than 20 or so nodes on any sane piece
    // of code. This is unlikely to ever be a performance issue.
    auto search = block0;
    for (; search->next != nullptr; search = search->next) { }
    search->next = block1;
    return block0;
}

std::uint32_t getProgCodeIndex(const InstructionMap & instrMapping, const IntermediateInstr * instr)
{
    MOON_ASSERT(instr != nullptr);
    auto && entry = instrMapping.find(instr);
    MOON_ASSERT(entry != std::end(instrMapping)); // TODO perhaps an exception instead?
    return entry->second;
}

std::uint32_t getProgDataIndex(const DataMap & dataMapping, const Symbol * sym)
{
    MOON_ASSERT(sym != nullptr);
    auto && entry = dataMapping.find(sym);
    MOON_ASSERT(entry != std::end(dataMapping)); // TODO perhaps exceptions instead?
    return entry->second;
}

std::uint32_t addSymbolToProgData(VM::DataVector & progData, const Symbol & sym)
{
    progData.push_back(Variant::fromSymbol(sym));
    return static_cast<std::uint32_t>(progData.size() - 1);
}

// TODO MACROS INSTEAD?
// perhaps throw compiler error instead?
void expectNumChildren(const SyntaxTreeNode * node, const int expected)
{
    for (int n = 0; n < expected; ++n)
    {
        MOON_ASSERT(node->getChild(n) != nullptr);
    }
}

void expectChildAtIndex(const SyntaxTreeNode * node, const int childIndex)
{
    MOON_ASSERT(node->getChild(childIndex) != nullptr);
}

void expectSymbol(const SyntaxTreeNode * node)
{
    MOON_ASSERT(node->getSymbol() != nullptr);
}

// ========================================================
// Debug printing helpers:
// ========================================================

void printIntermediateInstrList(const IntermediateInstr * head, std::ostream & os = std::cout)
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

void printInstructionMap(const InstructionMap & instrMapping, std::ostream & os = std::cout)
{
    os << color::white() << "[[ instruction map dump ]]" << color::restore() << "\n";

    for (const auto & entry : instrMapping)
    {
        os << color::cyan() << "[ " << std::setw(3) << entry.second << " ] "
           << color::red() << toString(entry.first->op) << color::restore() << "\n";
    }

    os << color::white() << "[[ listed " << instrMapping.size() << " instructions ]]"
       << color::restore() << "\n";
}

} // namespace {}

// ========================================================
// Compiler class methods:
// ========================================================

Compiler::Compiler(SymbolTable & symtab, SyntaxTree & ast)
    : symTable          { symtab  }
    , syntTree          { ast     }
    , firstInstr        { nullptr }
    , nextInstructionId { 0       }
{ }

void Compiler::compile(VM::DataVector & progData, VM::CodeVector & progCode)
{
    //TODO work in progress
    std::cout << "generating intermediate representation...\n";
    firstInstr = traverseTreeRecursive(syntTree.getRoot());
    std::cout << "-------------------------------\n";
    printIntermediateInstrList(firstInstr);
    intermediateToVM(progData, progCode);
}

// TODO NOTES:
//
// - Need to handle something like:
//     `if X then`
// differently in the VM
// it will have no cmp instruction before the jump!
//
Compiler::IntermediateInstr * Compiler::traverseTreeRecursive(const SyntaxTreeNode * root)
{
    MOON_ASSERT(root != nullptr);

    using AstNode = SyntaxTreeNode::Type;
    const AstNode nodeType = root->getType();

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
    case AstNode::TranslationUnit :
        {
            //TODO program might be empty? child[0] = null???
            block0 = newInstruction(OpCode::ModuleStart);
            block1 = traverseTreeRecursive(root->getChild(0));
            return concat(block0, block1);
        }
    case AstNode::Statement :
        {
            if (root->getChild(0) != nullptr)
            {
                expectChildAtIndex(root, 1);
                block0 = traverseTreeRecursive(root->getChild(0));
                block1 = traverseTreeRecursive(root->getChild(1));
                return concat(block0, block1);
            }
            else
            {
                expectChildAtIndex(root, 1);
                return traverseTreeRecursive(root->getChild(1));
            }
        }
    case AstNode::IfThenStatement :
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
                block0 = concat(condition, jumpIfFalse);
                block1 = concat(block0, thenStatement);
                return concat(block1, noop);
            }
            else // Empty then-statement/body:
            {
                condition   = traverseTreeRecursive(root->getChild(0));
                noop        = newInstruction(OpCode::NoOp);
                jumpIfFalse = newInstruction(OpCode::JmpIfFalse, noop);

                // if-cond => jump-end-if-false => noop[jump-target/end]
                block0 = concat(condition, jumpIfFalse);
                return concat(block0, noop);
            }
        }
    case AstNode::IfThenElseStatement :
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
            block0 = concat(condition, jumpIfFalse);
            if (thenStatement != nullptr)
            {
                block1 = concat(block0, thenStatement);
                block2 = concat(block1, jumpToEnd);
            }
            else
            {
                block2 = concat(block0, jumpToEnd);
            }

            if (elseStatement != nullptr)
            {
                return concat(concat(block2, elseStatement), noop);
            }
            else
            {
                return concat(block2, noop);
            }
            // if-cond => jump-else-if-false => if-body =>
            //     jump-end => else-body => noop[jump-target/end]
        }
    case AstNode::IfThenElseIfStatement :
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

            block0 = concat(condition, jumpIfFalse);
            block1 = concat(block0, thenStatement);
            block2 = concat(block1, jumpToEnd);
            return concat(concat(block2, elseStatement), noop);

            // if|elseif-cond => jump-next-if-false => if|elseif-body =>
            //     jump-end => [repeat till an else] => noop[jump-target/end]
        }
    case AstNode::VarDeclStatement :
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

    case AstNode::ExprFuncCall :
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
                return concat(block1, concat(block2, block0));
            }
            else // Function with zero parameters/arguments.
            {
                block2 = newInstruction(OpCode::IntLoad, symTable.findOrDefineValue(0));
                return concat(block2, block0);
            }
        }
    case AstNode::ExprNameIdent :
    case AstNode::ExprLiteralConst :
        {
            //TODO other data types!
            expectSymbol(root);
            block0 = newInstruction(OpCode::IntLoad, root->getSymbol());

            // !!! for functions !!!
            if (root->getChild(0) != nullptr)
            {
                block1 = traverseTreeRecursive(root->getChild(0));
                return concat(block0, block1); // <-- CHANGE THIS TO ALTER FUNC PARAMETER PASSING ORDER!
            }
            else
            {
                return block0;
            }
        }
    case AstNode::ExprAssign :
        {
            //TODO other data types (not just Int)!
            expectNumChildren(root, 2);
            expectSymbol(root->getChild(0));
            block0 = traverseTreeRecursive(root->getChild(1));
            block1 = newInstruction(OpCode::IntStore, root->getChildSymbol(0));
            return concat(block0, block1);
        }

    // All binary expressions are implemented the same way, except
    // for the instructions emitted. Simplify the code with a
    // little bit of preprocessor black magic ;)
    #define CASE_BINARY_EXPR(nodeType, opType)                 \
    case AstNode::nodeType :                                   \
        do                                                     \
        {                                                      \
            expectNumChildren(root, 2);                        \
            block0 = traverseTreeRecursive(root->getChild(0)); \
            block1 = traverseTreeRecursive(root->getChild(1)); \
            block2 = newInstruction(OpCode::opType);           \
            return concat(concat(block0, block1), block2);     \
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

    case OpCode::IntLoad  :
    case OpCode::IntStore :
        // this rule can actually be shared by several instruction types...
    case OpCode::Call :
    case OpCode::CallNative :
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
    std::cout << "compiling to vm instructions...\n";

    //TODO reserve memory? probably worth it.
//  progCode.reserve(intermediateInstructionCount);

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

//  printCodeVector(progCode);
//  printInstructionMap(instrMapping);

    for (auto instr = firstInstr; instr != nullptr; instr = instr->next)
    {
        fixReferences(instr, progData, progCode);
    }

    printCodeVector(progCode);
    printDataVector(progData);
}

//
// TODO use another pool allocator for IntermediateInstr!
//

Compiler::IntermediateInstr * Compiler::newInstruction(const OpCode op)
{
    auto instr = new IntermediateInstr;
    instr->next               = nullptr;
    instr->operand.symbol     = nullptr;
    instr->uid                = nextInstructionId++;
    instr->op                 = op;
    return instr;
}

Compiler::IntermediateInstr * Compiler::newInstruction(const OpCode op, const Symbol * symbol)
{
    auto instr = new IntermediateInstr;
    instr->next               = nullptr;
    instr->operand.symbol     = symbol;
    instr->uid                = nextInstructionId++;
    instr->op                 = op;
    return instr;
}

Compiler::IntermediateInstr * Compiler::newInstruction(const OpCode op, const IntermediateInstr * jumpTarget)
{
    auto instr = new IntermediateInstr;
    instr->next               = nullptr;
    instr->operand.jumpTarget = jumpTarget;
    instr->uid                = nextInstructionId++;
    instr->op                 = op;
    return instr;
}

} // namespace moon {}
