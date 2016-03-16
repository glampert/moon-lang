
// ================================================================================================
// -*- C++ -*-
// File: compiler.hpp
// Author: Guilherme R. Lampert
// Created on: 06/07/15
// Brief: The Compiler class transforms a Syntax Tree representation into VM bytecode.
// ================================================================================================

#ifndef MOON_COMPILER_HPP
#define MOON_COMPILER_HPP

#include "symbol_table.hpp"
#include "syntax_tree.hpp"
#include "vm.hpp"

#ifndef MOON_INTERMEDIATE_INSTR_POOL_GRANULARITY
    #define MOON_INTERMEDIATE_INSTR_POOL_GRANULARITY 1024
#endif // MOON_INTERMEDIATE_INSTR_POOL_GRANULARITY

namespace moon
{

// ========================================================
// class Compiler:
// ========================================================

class Compiler final
{
public:

    //
    // Intermediate code representation:
    //

    struct IntermediateInstr final
    {
        union Operand
        {
            const Symbol            * symbol;
            const IntermediateInstr * jumpTarget;
        };

        IntermediateInstr * next;
        Operand             operand;
        std::uint32_t       uid;
        OpCode              op;
    };

    IntermediateInstr * newInstruction(OpCode op);
    IntermediateInstr * newInstruction(OpCode op, const Symbol * symbol);
    IntermediateInstr * newInstruction(OpCode op, const IntermediateInstr * jumpTarget);

    const IntermediateInstr * getLoopStartAnchor() const noexcept { return lastLoopStartLabel; }
    const IntermediateInstr * getLoopEndAnchor()   const noexcept { return lastLoopEndLabel;   }
    const IntermediateInstr * getReturnAnchor()    const noexcept { return lastFuncEndLabel;   }

    void setLoopAnchors(const IntermediateInstr * startLabel, const IntermediateInstr * endLabel) noexcept;
    void clearLoopAnchors() noexcept;

    void setReturnAnchor(const IntermediateInstr * endLabel) noexcept;
    void clearReturnAnchor() noexcept;

    void markVisited(const SyntaxTreeNode * node);
    bool nodeWasVisited(const SyntaxTreeNode * node) const;
    void clearVisited() noexcept;

    //
    // Helper types:
    //

    using STNodeList     = std::vector<const SyntaxTreeNode *>;
    using DataMap        = std::unordered_map<const Symbol *, std::uint32_t>;
    using InstructionMap = std::unordered_map<const IntermediateInstr *, std::uint32_t>;

    //
    // Compiler interface and aux data:
    //

    // These are filled by a Parser.
    SymbolTable symTable;
    SyntaxTree  syntTree;

    Compiler();

    // Not copyable.
    Compiler(const Compiler &) = delete;
    Compiler & operator = (const Compiler &) = delete;

    // Runs the compilation process to produce bytecode and data from the SyntaxTree,
    // which is then ready to be run on a Moon VM instance. Might throw compiler errors.
    void compile(VM & vm);

    // Debug printing:
    void printIntermediateInstructions(std::ostream & os = std::cout) const;
    void printInstructionMapping(std::ostream & os = std::cout) const;

private:

    void intermediateToVM(VM::DataVector & progData, VM::CodeVector & progCode);
    void fixReferences(const IntermediateInstr * instr, VM::DataVector & progData, VM::CodeVector & progCode);

    // instructionCount is also the uid.
    std::uint32_t instructionCount;
    IntermediateInstr * instrListHead;

    // Somewhat hackish way of keeping track of the
    // head and tail of a loop for break/continue jumps.
    const IntermediateInstr * lastLoopStartLabel;
    const IntermediateInstr * lastLoopEndLabel;

    // Same as above, return statements need to reference the end of the parent function.
    const IntermediateInstr * lastFuncEndLabel;

    // For some expressions we need to keep track of the visited nodes
    // (e.g.: counting function parameter lists). This temporary array
    // is used for such cases.
    STNodeList visitedNodes;

    // These are used to combine repeated program data/symbols
    // and to optimize away noops in the intermediateToVM step.
    DataMap dataMapping;
    InstructionMap instrMapping;

    // All IntermediateInstr instances are sourced from this pool.
    ObjectPool<IntermediateInstr, MOON_INTERMEDIATE_INSTR_POOL_GRANULARITY> instrPool;
};

// Prints the intermediate instructions and their mappings (instrListHead & instrMapping).
std::ostream & operator << (std::ostream & os, const Compiler & compiler);

} // namespace moon {}

#endif // MOON_COMPILER_HPP
