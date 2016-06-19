
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

#include "semantic_check.hpp"

//TODO maybe gather all these pool size constants and the like into a build_config.hpp?
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
    UInt16              stackIndex;
    Variant::Type       dataType;
    OpCode              op;
};

constexpr UInt16 InvalidStackIndex = UInt16(-1);
using InstrPool = Pool<IntermediateInstr, MOON_INTERMEDIATE_INSTR_POOL_GRANULARITY>;

// ================================================================================================

namespace new_
{

// ========================================================
// class FileIOCallbacks:
// ========================================================

class FileIOCallbacks
{
public:
    virtual ~FileIOCallbacks();

    // Called to open a file for an explicit Compiler::parseScript() call.
    // Should return true only if the file was successfully opened for reading.
    virtual bool openScript(const std::string & scriptFile, std::istream ** streamOut) = 0;

    // Called when an 'import' directive is encountered in a script.
    // Receives the unaltered name string that followed the keyword.
    // The implementation is free to perform any path-dependent file
    // lookup or append predefined search paths to the name.
    virtual bool openScriptImport(const std::string & importFile, std::istream ** streamOut) = 0;

    // Called to return a stream object previously acquired by the above methods.
    // NOTE: If the open method returned false, closeScript() is not called!
    virtual void closeScript(std::istream ** stream) = 0;
};

// ========================================================
// class Compiler:
// ========================================================

class Compiler final
{
public:

    //
    // Public compiler interface:
    //

    Compiler();
    explicit Compiler(FileIOCallbacks * newIOCallbacks);

    // Not copyable.
    Compiler(const Compiler &) = delete;
    Compiler & operator = (const Compiler &) = delete;

    // Opens the given file to parse the script source code and generate a syntax tree
    // from it. This will also perform semantic validation in the source and throw parse
    // errors if there are any. Bytecode is only generated later when compile() is called.
    // Functions and types will be registered with the given VM instance. You should pass
    // the same pointer again when calling compile().
    void parseScript(VM * vm, const std::string & filename,
                     SyntaxTreeNode ** outTreeRoot = nullptr);

    // Same as the above, but takes an already opened file stream,
    // so the FileIOCallbacks are not used.
    void parseScript(VM * vm, std::istream * source,
                     const std::string & filename,
                     SyntaxTreeNode ** outTreeRoot = nullptr);

    // This one opens the script using FileIOCallbacks::openScriptImport(), so it is
    // meant for internal parser use to handle the import directives in the scripts.
    void parseScriptImport(VM * vm, const std::string & filename,
                           SyntaxTreeNode ** outImportRoot = nullptr);

    // Runs the compilation process to produce bytecode and data from the syntax tree built during
    // parsing, which is then ready to be run on a Moon VM instance. Might throw compiler errors.
    void compile(VM * vm);

    // Set the FileIOCallbacks used by this compiler.
    // The IO callbacks are used to open the script file on parseScript() and
    // also to open subsequent imports found while parsing the initial script.
    // By default the compiler will use the Standard <fstream> C++ API to open files.
    // Passing null to set() will restore the default callbacks.
    void setFileIOCallbacks(FileIOCallbacks * newIOCallbacks) noexcept;

    // Debug printing for the generated intermediate code.
    void print(std::ostream & os) const;

    //
    // Compiler internals:
    //

    SymbolTable         symTable;
    SyntaxTree          syntTree;
    VarInfoTable        varInfo;
    ImportTable         importTable;
    InstrPool           instrPool;
    UInt32              instrCount       = 0;
    IntermediateInstr * globCodeListHead = nullptr;
    IntermediateInstr * funcCodeListHead = nullptr;
    FileIOCallbacks   * fileIOCallbacks  = nullptr;
};

inline std::ostream & operator << (std::ostream & os, const Compiler & compiler)
{
    // Prints the globCodeListHead and funcCodeListHead.
    compiler.print(os);
    return os;
}

} // namespace new_ {}

// ================================================================================================

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

    //-------
    // parseScript(std::ostream)
    // avoids doing file IO. user has to provide an open file or whatever
    //
    // struct ImportHandler {
    //     virtual std::ostream openScriptImport(const std::string & importFile) = 0;
    // }
    // so the user can handle include paths in whatever ways.
    // we rely on the callback to open imports referenced by the scripts.
    //

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

    bool symbolIsFunctionLocal(const Symbol * symbol, UInt16 & stackIndex) const;
    const TypeId * symbolToTypeId(const Symbol * symbol) const;

    const TypeId * findFunctionLocalSymbolTypeId(const Symbol * symbol) const;
    const TypeId * findGlobalSymbolTypeId(const Symbol * symbol) const;

    void addFunctionLocalSymbol(const Symbol * symbol, const TypeId * tid);
    void addGlobalSymbol(const Symbol * symbol, const TypeId * tid);

    void beginFunction(const IntermediateInstr * endLabel, const SyntaxTreeNode * root);
    void endFunction();

    IntermediateInstr * newInstruction(OpCode op);
    IntermediateInstr * newInstruction(OpCode op, const Symbol * symbol, Variant::Type dataType = Variant::Type::Null);//FIXME no default value in here!
    IntermediateInstr * newInstruction(OpCode op, const IntermediateInstr * jumpTarget);
    IntermediateInstr * newInstruction(const IntermediateInstr * copy); // clone the input into a new instr

private:

    void createMappings(VM & vm, IntermediateInstr * listHead, bool skipFunctions);
    void fixReferences(const IntermediateInstr * instr, VM::CodeVector & progCode, FunctionTable & funcTable);

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

    //FIXME: this should be replaced by the VarInfoTable. No need to generate this data twice.
    using TypedSymbol = std::pair<const Symbol *, const TypeId *>;

    // Temp store we use to gather local vars and function parameters.
    std::vector<TypedSymbol> funcLocalIdentifiers;

    // Temp for the globals found so far in a translation unit.
    std::vector<TypedSymbol> globalIdentifiers;

using DataMap  = HashTable<const Symbol *, UInt32>;
using InstrMap = HashTable<const IntermediateInstr *, UInt32>;

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
