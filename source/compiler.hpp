
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
// struct IntermediateInstr:
// ========================================================

struct IntermediateInstr final
{
    union Operand
    {
        const Symbol            * symbol;
        const IntermediateInstr * jumpTarget;
    };

    IntermediateInstr * next;
    Operand             operand;
    UInt32              uid;
    UInt16              paramIdx;
    Variant::Type       type;
    OpCode              op;
};

using DataMap  = std::unordered_map<const Symbol *, UInt32>;
using InstrMap = std::unordered_map<const IntermediateInstr *, UInt32>;

// ========================================================
// class Compiler:
// ========================================================

class Compiler final
{
public:

    //
    // Public compiler interface:
    //

    // These are filled by the Parser.
    SymbolTable symTable;
    SyntaxTree  syntTree;

    Compiler() = default;

    // Not copyable.
    Compiler(const Compiler &) = delete;
    Compiler & operator = (const Compiler &) = delete;

    // Runs the compilation process to produce bytecode and data from the SyntaxTree,
    // which is then ready to be run on a Moon VM instance. Might raise compiler errors.
    void compile(VM & vm);

    // Debug printing for the generated intermediate code.
    void print(std::ostream & os) const;

    //
    // Intermediate code generation:
    //

    //TODO: move most of this internal stuff into a CodeGenerator helper
    //this class is too fat already!

    const TypeId * lastMemberTypeId = nullptr;

    // Somewhat hackish way of keeping track of the
    // head and tail of a loop for break/continue jumps.
    const IntermediateInstr * lastLoopStartAnchor = nullptr;
    const IntermediateInstr * lastLoopEndAnchor   = nullptr;

    // Same as above, return statements need to reference the end of the parent function.
    const IntermediateInstr * lastFuncEndAnchor = nullptr;

    std::vector<const Symbol *> memberRefList;

    bool insideFunctionDecl = false;

    void setLoopAnchors(const IntermediateInstr * startLabel, const IntermediateInstr * endLabel) noexcept;
    void clearLoopAnchors() noexcept;

    void setReturnAnchor(const IntermediateInstr * endLabel) noexcept;
    void clearReturnAnchor() noexcept;

    void markVisited(const SyntaxTreeNode * node);
    bool nodeWasVisited(const SyntaxTreeNode * node) const;
    void clearVisited() noexcept;

    const TypeId * guessTypeId(const SyntaxTreeNode * node);

    bool symbolIsFunctionLocal(const Symbol * symbol, UInt16 & paramIdx) const;
    const TypeId * symbolToTypeId(const Symbol * symbol) const;

    const TypeId * findFunctionLocalSymbolTypeId(const Symbol * symbol) const;
    const TypeId * findGlobalSymbolTypeId(const Symbol * symbol) const;

    void addFunctionLocalSymbol(const Symbol * symbol, const TypeId * tid);
    void addGlobalSymbol(const Symbol * symbol, const TypeId * tid);

    void beginFunction(const IntermediateInstr * endLabel, const SyntaxTreeNode * root);
    void endFunction();

    IntermediateInstr * newInstruction(OpCode op);
    IntermediateInstr * newInstruction(OpCode op, const Symbol * symbol, Variant::Type type = Variant::Type::Null);//FIXME no default value in here!
    IntermediateInstr * newInstruction(OpCode op, const IntermediateInstr * jumpTarget);

private:

    void createMappings(VM::DataVector & progData,
                        VM::CodeVector & progCode,
                        FunctionTable  & funcTable,
                        TypeTable      & typeTable,
                        IntermediateInstr * listHead,
                        bool skipFunctions);

    void fixReferences(const IntermediateInstr * instr,
                       VM::CodeVector & progCode,
                       FunctionTable  & funcTable);

    void intermediateToVM(VM & vm);
    void collectFunctionVarSymbols(const SyntaxTreeNode * root);
    void collectFunctionArgSymbols(const SyntaxTreeNode * root);

private:

    // instructionCount is also the uid generator.
    UInt32 instructionCount = 0;
    IntermediateInstr * globCodeListHead = nullptr;
    IntermediateInstr * funcCodeListHead = nullptr;

    // Ref to the VM types table. Set by a call to compile().
    const TypeTable * runtimeTypes = nullptr;

    // For some expressions we need to keep track of the visited nodes
    // (e.g.: counting function parameter lists). This temporary array
    // is used for such cases.
    std::vector<const SyntaxTreeNode *> visitedNodes;

    using TypedSymbol = std::pair<const Symbol *, const TypeId *>;

    // Temp store we use to gather local vars and function parameters.
    std::vector<TypedSymbol> funcLocalIdentifiers;

    // Temp for the globals found so far in a module.
    std::vector<TypedSymbol> globalIdentifiers;

    // These are used to combine repeated program data/symbols
    // and to optimize away noops in the intermediateToVM step.
    DataMap  dataMapping;
    InstrMap instrMapping;

    // All IntermediateInstr instances are sourced from this pool.
    Pool<IntermediateInstr, MOON_INTERMEDIATE_INSTR_POOL_GRANULARITY> instrPool;
};

inline std::ostream & operator << (std::ostream & os, const Compiler & compiler)
{
    // Prints the globCodeListHead and funcCodeListHead.
    compiler.print(os);
    return os;
}

} // namespace moon {}

#endif // MOON_COMPILER_HPP
