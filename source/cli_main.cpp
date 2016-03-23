
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

/*
=================================================
TODO NOTES:

- Should test this on a tool like Valgrind or Clang Mem Sanitizer to check for possible leaks!!!

- Might want to separate literal constants on lists by type, to accelerate lookup....

- Probably also define a StringTable class that uses ref-counting to keep track of our strings.
  Most strings defined during parsing in the SymbolTable can be reused by the VM, so we should
  avoid copying them.

- Custom, user supplied, memory allocator? Could integrate with a std::allocator perhaps?...

*****

NOTES ON THE LANGUAGE SYNTAX SIDE:

- Define some construct for debug only code, e.g. some `debug` keyword, like in D.

- Built-in assert() function.
- Built-in panic() function.

- Concept of modules/namespaces?

- Assign functions to variables (a Function/Callable type?)

- C-style language to keep it simple:
  - structs
  - functions
  - easy to integrate with native methods (so we can for instance call OpenGL).

- Make `let` variables immutable (then add `mut`).

=================================================
*/

int main(int argc, const char * argv[])
{
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
        std::cout << "sizeof(Compiler::IntermediateInstr) = " << sizeof(Compiler::IntermediateInstr) << std::endl;
        std::cout << "sizeof(Variant)  = " << sizeof(Variant) << std::endl;
        std::cout << "sizeof(Function) = " << sizeof(Function) << std::endl;

        std::cout << "sizeof(ConstRcString)   = " << sizeof(ConstRcString) << std::endl;
        std::cout << "sizeof(MutableRcString) = " << sizeof(MutableRcString) << std::endl;

        auto rstr1 = newConstRcString("\'testing, 1234\'");
        auto rstr2 = newMutableRcString("\'testing, 1234\'");

        std::cout << "s: " << rstr1->chars << std::endl;
        std::cout << "l: " << rstr1->length << std::endl;
        std::cout << "h: " << rstr1->hashVal << std::endl;
        std::cout << "r: " << rstr1->refCount << std::endl;
        std::cout << std::endl;
        std::cout << "s: " << rstr2->chars << std::endl;
        std::cout << "l: " << rstr2->length << std::endl;
        std::cout << "r: " << rstr2->refCount << std::endl;

        HashTableCRcStr<int> ht1;
        HashTableMRcStr<int> ht2;
        ht1.emplace(rstr1, 1);
        ht2.emplace(rstr2, 2);

        releaseRcString(rstr1);
        releaseRcString(rstr2);

        ///////////////
        Variant v0;
        Variant v1;

        v0.type = Variant::Type::Integer;
        v1.type = Variant::Type::Integer;

        v0.value.asInteger = 3;
        v1.value.asInteger = 3;

        auto res = performBinaryOp(OpCode::Mod, v0, v1);
        std::cout << std::endl;
        std::cout << toString(res.type) << std::endl;
        std::cout << toString(res) << std::endl;
        std::cout << std::endl;
    }

    try
    {
        ParseContext parseCtx;

        std::string srcFile = "test.ml";
        std::string currText;

        VM       vm;
        Compiler compiler;
        Lexer    lexer  { parseCtx, std::cin };
        Parser   parser { parseCtx };

        parseCtx.yylval   = nullptr;
        parseCtx.lexer    = &lexer;
        parseCtx.parser   = &parser;
        parseCtx.symTable = &compiler.symTable;
        parseCtx.fnTable  = &vm.functions;
        parseCtx.syntTree = &compiler.syntTree;
        parseCtx.currText = &currText;
        parseCtx.srcFile  = &srcFile;

        std::cout << (parser.parse() == 0 ? "\n-- FINISHED OK --" : "\n-- FINISHED WITH PARSE ERROR --") << "\n\n";
        std::cout << compiler.symTable << "\n";
        std::cout << compiler.syntTree << "\n";

        compiler.compile(vm);
        std::cout << compiler << "\n";
        std::cout << vm.functions << "\n";
        std::cout << vm.code << "\n";
        std::cout << vm.data << "\n";

        vm.execute();
        std::cout << vm << "\n";
    }
    catch (const moon::BaseException & e)
    {
        std::cout << e.what() << std::endl;
    }
}
