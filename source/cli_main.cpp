
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

- Custom, user supplied, memory allocator? Could integrate with a std::allocator perhaps?...

- perhaps keep an internal list of allocated RcStrings
  for debug checking of leaked references. We should of course
  allow disabling this check on a "release" build.

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

=================================================
*/

int main(int argc, const char * argv[])
{
    #if MOON_DEBUG
    moon::logStream() << "Moon: This is a debug build.\n";
    #endif // MOON_DEBUG

    #if MOON_ENABLE_ASSERT
    moon::logStream() << "Moon: Runtime asserts are enabled.\n";
    #endif // MOON_ENABLE_ASSERT

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
        logStream() << "sizeof(VM)                = " << sizeof(VM) << std::endl;
        logStream() << "sizeof(Compiler)          = " << sizeof(Compiler) << std::endl;
        logStream() << "sizeof(IntermediateInstr) = " << sizeof(IntermediateInstr) << std::endl;
        logStream() << "sizeof(Variant)           = " << sizeof(Variant) << std::endl;
        logStream() << "sizeof(Function)          = " << sizeof(Function) << std::endl;
        logStream() << "sizeof(ConstRcString)     = " << sizeof(ConstRcString) << std::endl;
        logStream() << "sizeof(Object)            = " << sizeof(Object) << std::endl;
        logStream() << "sizeof(Struct)            = " << sizeof(Struct) << std::endl;
        logStream() << "sizeof(Str)               = " << sizeof(Str) << std::endl;
        logStream() << "sizeof(Array)             = " << sizeof(Array) << std::endl;
        logStream() << "sizeof(Enum)              = " << sizeof(Enum) << std::endl;

        constexpr auto minConstStrLen = sizeof(std::string) - (sizeof(std::size_t) * 2);
        logStream() << "sizeof(minConstStrLen)    = " << sizeof(minConstStrLen) << std::endl;
        logStream() << "sizeof(std::string)       = " << sizeof(std::string) << std::endl;

        auto rstr1 = newConstRcString("\'testing, 1234\'");

        logStream() << "s: " << rstr1->chars << std::endl;
        logStream() << "l: " << rstr1->length << std::endl;
        logStream() << "h: " << rstr1->hashVal << std::endl;
        logStream() << "r: " << rstr1->refCount << std::endl;
        logStream() << std::endl;

        HashTableConstRcStr<int> ht1;
        ht1.emplace(rstr1, 1);
        releaseRcString(rstr1);

        ///////////////
        Variant v0;
        Variant v1;

        v0.type = Variant::Type::Integer;
        v1.type = Variant::Type::Integer;

        v0.value.asInteger = 3;
        v1.value.asInteger = 3;

        auto res = performBinaryOp(OpCode::Mod, v0, v1);
        logStream() << std::endl;
        logStream() << toString(res.type) << std::endl;
        logStream() << toString(res) << std::endl;
        logStream() << std::endl;

        void * pv = v0.getAsVoidPointer();
        (void)pv;
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

        parseCtx.yylval    = nullptr;
        parseCtx.lexer     = &lexer;
        parseCtx.parser    = &parser;
        parseCtx.symTable  = &compiler.symTable;
        parseCtx.fnTable   = &vm.functions;
        parseCtx.typeTable = &vm.runtimeTypes;
        parseCtx.syntTree  = &compiler.syntTree;
        parseCtx.objList   = &vm.gcListHead;
        parseCtx.currText  = &currText;
        parseCtx.srcFile   = &srcFile;

        logStream() << (parser.parse() == 0 ? "\n-- FINISHED OK --" : "\n-- FINISHED WITH PARSE ERROR --") << "\n\n";
        logStream() << compiler.symTable << "\n";
        logStream() << vm.runtimeTypes << "\n";
        logStream() << compiler.syntTree << "\n";

        compiler.compile(vm);
        logStream() << compiler << "\n";
        logStream() << vm.functions << "\n";
        logStream() << vm.code << "\n";
        logStream() << vm.data << "\n";

        vm.execute();
        logStream() << vm << "\n";

        //*
        auto lstr = Str::newFromString(vm, "Testing", true);
        std::cout << std::boolalpha << "isConstString = " << lstr->isConstString() << std::endl;
        std::cout << std::boolalpha << "isEmptyString = " << lstr->isEmptyString() << std::endl;
        std::cout << std::boolalpha << "getLength     = " << lstr->getStringLength() << std::endl;
        std::cout << std::boolalpha << "c_str         = " << lstr->c_str() << std::endl;
        std::cout << std::endl;
        auto lstr2 = Str::newFromString(vm, "-Testing-2", true);
        auto lstr3 = Str::newFromStrings(vm, *lstr, *lstr2, true);
        std::cout << std::boolalpha << "isConstString = " << lstr3->isConstString() << std::endl;
        std::cout << std::boolalpha << "isEmptyString = " << lstr3->isEmptyString() << std::endl;
        std::cout << std::boolalpha << "getLength     = " << lstr3->getStringLength() << std::endl;
        std::cout << std::boolalpha << "c_str         = " << lstr3->c_str() << std::endl;
        //*/

        //*
        logStream() << "----------------------------------" << std::endl;
        logStream() << "GC OBJECTS:\n" << std::endl;
        for (auto obj = vm.gcListHead; obj != nullptr; obj = obj->next)
        {
            obj->print(logStream());
            logStream() << std::endl;
        }
        logStream() << "----------------------------------" << std::endl;
        //*/
    }
    catch (const moon::BaseException & e)
    {
        logStream() << e.what() << std::endl;
    }
}
