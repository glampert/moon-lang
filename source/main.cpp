
// ================================================================================================
// -*- C++ -*-
// File: main.cpp
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
- Might want to separate literal constants on lists by type, to accelerate lookup.

- Probably also define a StringTable class that uses ref-counting to keep track of our strings.
  Most strings defined during parsing in the SymbolTable can be reused by the VM, so we should
  avoid copying them.

- Custom, user supplied, memory allocator? Could integrate with a std::allocator perhaps?...

- Add the Travis build script on GitHub for auto build on commits!

*****

NOTES ON THE LANGUAGE SYNTAX SIDE:

- Define some construct for debug only code, e.g. some `debug` keyword, like in D.

- Built-in assert function.

- Concept of modules/namespaces?

- Assign functions to variables (a Function/Callable type?)

- C-style language to keep it simple:
  - structs
  - functions
  - easy to integrate with native methods (so we can for instance call OpenGL).

- Add a pow() operator? E.g. '^' as it is in some math languages?
  Also, should modulo (%) work on floats? Could use `reminder()` or similar...

- Make `let` variables immutable (then add `mut`).

=================================================
*/

int main(int argc, const char * argv[])
{
    (void)argc;
    (void)argv;
    using namespace moon;

    try
    {
        ParseContext parseCtx;

        std::string srcFile = "test.ml";
        std::string currText;

        SymbolTable symTable;
        SyntaxTree  syntTree;

        Lexer  lexer{ parseCtx, std::cin };
        Parser parser{ parseCtx };

        parseCtx.yylval   = nullptr;
        parseCtx.lexer    = &lexer;
        parseCtx.parser   = &parser;
        parseCtx.symTable = &symTable;
        parseCtx.syntTree = &syntTree;
        parseCtx.currText = &currText;
        parseCtx.srcFile  = &srcFile;

        std::cout << (parser.parse() == 0 ? "\n-- FINISHED OK --" : "\n-- FINISHED WITH PARSE ERROR --") << "\n\n";

        std::cout << symTable << "\n";
        std::cout << syntTree << "\n";

        VM::DataVector progData;
        VM::CodeVector progCode;

        Compiler compiler{ symTable, syntTree };
        compiler.compile(progData, progCode);
        std::cout << compiler << "\n";

        std::cout << progCode << "\n";
        std::cout << progData << "\n";

        VM vm{ std::move(progData), std::move(progCode) };
        vm.executeProgram();
        std::cout << vm << "\n";
    }
    catch (const std::exception & e)
    {
        std::cout << "EXCEPTION: " << e.what() << std::endl;
    }
}
