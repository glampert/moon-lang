
// ================================================================================================
// -*- C++ -*-
// File: cli_main.cpp
// Author: Guilherme R. Lampert
// Created on: 10/03/16
// Brief: Command-line interpreter entry point.
// ================================================================================================

#include "compiler.hpp"
#include "vm.hpp"

#include <iostream>
#include <iomanip>

//TEMP
#include "semantic_check.hpp"

/*
=================================================
TODO NOTES:

- Should test this on a tool like Valgrind or Clang Mem Sanitizer to check for possible leaks!!!

- Custom, user supplied, memory allocator? Could integrate with a std::allocator perhaps?...

- perhaps keep an internal list of allocated RcStrings
  for debug checking of leaked references. We should of course
  allow disabling this check on a "release" build.

- Replace std::string with the larger SmallStr from NTB to try avoiding mallocs for small strings?

- Script exceptions should probably carry a script stack trace with them.

*****

NOTES ON THE LANGUAGE SYNTAX SIDE:

- Define some construct for debug only code, e.g. some `debug` keyword, like in D.

- Built-in assert() function (with func-file-line info).
- Built-in panic() function (with func-file-line info).

- Assign functions to variables (a Function/Callable type?)

- C-style language to keep it simple:
  - structs
  - functions
  - easy to integrate with native methods (so we can for instance call OpenGL).

=================================================
*/

using namespace moon;

static IntermediateInstr * newInstr(int idx)
{
    auto instr = new IntermediateInstr{};
    instr->uid = idx;
    return instr;
}

static void printInstrList(const IntermediateInstr * head)
{
    std::cout << "\n";
    for (auto instr = head; instr != nullptr; instr = instr->next)
    {
        std::cout << instr->uid << " -> ";
    }
    std::cout << "~\n" << std::endl;
}

//
//TODO replace the messy linkInstr in compiler with these new template ones!
//

static IntermediateInstr * linkInstructions(IntermediateInstr * head, IntermediateInstr * newTail)
{
    MOON_ASSERT(head != nullptr);

    // Instruction chains where we use this function are short.
    // The longest would be from a big if/elseif construct, which
    // shouldn't be longer than 20 or so nodes on any sane piece
    // of code. This is unlikely to ever be a performance issue.
    auto search = head;
    for (; search->next != nullptr; search = search->next) { }
    search->next = newTail;
    return head;
}

template<typename... Instructions>
static IntermediateInstr * linkInstructions(IntermediateInstr * head, Instructions... args)
{
    MOON_ASSERT(head != nullptr);

    auto search = head;
    for (; search->next != nullptr; search = search->next) { }
    search->next = linkInstructions(args...);
    return head;
}

// ================================================================================================

#ifndef MOON_RT_OBJECT_POOL_GRANULARITY
    #define MOON_RT_OBJECT_POOL_GRANULARITY 512
#endif // MOON_RT_OBJECT_POOL_GRANULARITY

template<typename T>
constexpr T maxOf2(T a, T b) { return (a > b) ? a : b; }

template<typename T>
constexpr T maxOfN(T x) { return x; }

template<typename T, typename... Args>
constexpr T maxOfN(T x, Args... args) { return maxOf2(x, maxOfN(args...)); }

template<typename... Objects>
struct RtObjMemoryBlobImpl final
{
    static constexpr int LargestSize  = maxOfN(sizeof(Objects)...);
    static constexpr int LargestAlign = maxOfN(alignof(Objects)...);

    alignas(LargestAlign) UInt8 blob[LargestSize];
};

// Actually, might want to separate Str and Array from the rest.
// Str and Array are ~128Bytes, we can probably fit the rest in under 64
using RtObjMemoryBlob =
    RtObjMemoryBlobImpl
    <
        Object,
        Struct,
        Enum,
        Str,
        Array
    >;
using RuntimeObjectPool = Pool<RtObjMemoryBlob, MOON_RT_OBJECT_POOL_GRANULARITY>;

// ================================================================================================

int main(const int argc, const char * argv[])
{
    ////////
    {
        auto a = newInstr(0);
        auto b = newInstr(1);
        auto c = newInstr(2);
        auto d = newInstr(3);
        auto e = newInstr(4);
        IntermediateInstr * nul = nullptr;
        auto f = linkInstructions(a, b, c, d, e, nul);
        printInstrList(f);
    }

    (void)argc;
    (void)argv;
    using namespace moon;

    /* Final interface should look something like:
    try
    {
        moon::VM       vm;               // Executes the bytecode generate by a Compiler.
        moon::Compiler compiler;         // Parses scripts & generates VM bytecode.

        compiler.parseScript("test.ml"); // Parsing & semantic checks.
        compiler.parseScript([std::istream]); // from open file/stream of whatever sort
        compiler.compile(vm);            // Bytecode generation.

        vm.execute();                    // Execute the whole program.
    }
    catch (const moon::BaseException & e)
    {
        // Compiler or runtime errors (including script panic()/assert)
    }
    */

    // Misc temporary tests
    {
        assert(hashCString("hello") == hashCString("hello", std::strlen("hello")));

        logStream() << "sizeof(VM)                = " << sizeof(VM) << std::endl;
        logStream() << "sizeof(Compiler)          = " << sizeof(Compiler) << std::endl;
        logStream() << "sizeof(Parser)            = " << sizeof(Parser) << std::endl;
        logStream() << "sizeof(Lexer)             = " << sizeof(Lexer) << std::endl;
        logStream() << "sizeof(IntermediateInstr) = " << sizeof(IntermediateInstr) << std::endl;
        logStream() << "sizeof(Symbol)            = " << sizeof(Symbol) << std::endl;
        logStream() << "sizeof(SyntaxTreeNode)    = " << sizeof(SyntaxTreeNode) << std::endl;
        logStream() << "sizeof(Variant)           = " << sizeof(Variant) << std::endl;
        logStream() << "sizeof(Function)          = " << sizeof(Function) << std::endl;
        logStream() << "sizeof(ConstRcString)     = " << sizeof(ConstRcString) << std::endl;
        logStream() << "sizeof(Object)            = " << sizeof(Object) << std::endl;
        logStream() << "sizeof(Struct)            = " << sizeof(Struct) << std::endl;
        logStream() << "sizeof(Str)               = " << sizeof(Str) << std::endl;
        logStream() << "sizeof(Array)             = " << sizeof(Array) << std::endl;
        logStream() << "sizeof(Enum)              = " << sizeof(Enum) << std::endl;
        logStream() << "sizeof(Range)             = " << sizeof(Range) << std::endl;

        constexpr UInt32 minConstStrLen = sizeof(std::string) - (sizeof(std::size_t) * 2);
        logStream() << "sizeof(minConstStrLen)    = " << sizeof(minConstStrLen) << std::endl;
        logStream() << "sizeof(std::string)       = " << sizeof(std::string) << std::endl;
        logStream() << "sizeof(VarInfo)           = " << sizeof(std::string) << std::endl;
        logStream() << "\n";

        DefaultFileIOCallbacks iocb;

        OpenScriptRAII raii1{ "test.ml", &iocb, &FileIOCallbacks::openScript };
        MOON_ASSERT(raii1.isOpen() == true);

        OpenScriptRAII raii2{ "inexistent.ml", &iocb, &FileIOCallbacks::openScript };
        MOON_ASSERT(raii2.isOpen() == false);
    }

    ParseContext parseCtx;

    std::string srcFile = "test.ml";
    std::string currText;

    VM vm; //{false, VM::DefaultStackSize};
    Compiler compiler;
    Lexer lexer{ parseCtx, &std::cin };
    Parser parser{ parseCtx };
    VarInfoTable varInfo{ parseCtx };

    parseCtx.lexer = &lexer;
    parseCtx.parser = &parser;
    parseCtx.symTable = &compiler.symTable;
    parseCtx.syntTree = &compiler.syntTree;
    parseCtx.varInfo = &varInfo;
    parseCtx.vm = &vm;
    parseCtx.currText = &currText;
    parseCtx.srcFile = &srcFile;

    //parseCtx.debugMode = false;
    //parseCtx.enableWarnings = false;

    try
    {
        logStream() << (parser.parse() == 0 ? "\n-- FINISHED OK --" : "\n-- FINISHED WITH PARSE ERROR --") << "\n\n";
        logStream() << compiler.symTable << "\n";
        //logStream() << vm.types << "\n";
        logStream() << compiler.syntTree << "\n";

        #if MOON_ENABLE_ASSERT
        compiler.syntTree.validateNodes();
        #endif // MOON_ENABLE_ASSERT

        //****** TEMP
        /*
        logStream() << "----------------------------------" << std::endl;
        logStream() << "GC OBJECTS:\n" << std::endl;
        for (const Object * obj = vm.gc.getGCListHead(); obj != nullptr; obj = obj->getGCLink())
        {
            obj->print(logStream());
            logStream() << std::endl;
        }
        */
        //return 0;

        compiler.compile(vm);
        logStream() << compiler << "\n";
        //logStream() << vm.functions << "\n";
        logStream() << vm.code << "\n";
        logStream() << vm.data << "\n";
        //return 0;

        vm.execute();
        logStream() << vm << "\n";

        /*
        logStream() << "----------------------------------" << std::endl;
        logStream() << "GC OBJECTS:\n" << std::endl;
        for (const Object * obj = vm.gc.getGCListHead(); obj != nullptr; obj = obj->getGCLink())
        {
            obj->print(logStream());
            logStream() << std::endl;
        }
        logStream() << "----------------------------------" << std::endl;
        //*/
    }
    catch (const std::exception & e)
    {
        logStream() << e.what() << std::endl;
    }
}
