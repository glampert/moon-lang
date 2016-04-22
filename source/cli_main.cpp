
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

static void testScriptString(VM & vm)
{
    // Small string allocated into the in-place buffer (not a ConstRcString).
    auto str1 = Str::newFromString(vm, "hello-", false);

    // Initial invariants:
    MOON_ASSERT(std::strcmp(str1->c_str(), "hello-") == 0);
    MOON_ASSERT(str1->isEmptyString()   == false);
    MOON_ASSERT(str1->isConstString()   == false);
    MOON_ASSERT(str1->getStringLength() == 6);

    // Share string reference:
    ConstRcString * rstr = newConstRcString("this is a ref counted string");
    auto str2 = Str::newFromString(vm, rstr);

    MOON_ASSERT(std::strcmp(str2->c_str(), rstr->chars) == 0);
    MOON_ASSERT(str2->isEmptyString()   == false);
    MOON_ASSERT(str2->isConstString()   == true);
    MOON_ASSERT(str2->getStringLength() == rstr->length);

    // Append into new string:
    auto str3 = Str::newFromStrings(vm, *str1, *str2, false);
    const char result[] = "hello-this is a ref counted string";
    MOON_ASSERT(std::strcmp(str3->c_str(), result) == 0);
    MOON_ASSERT(str3->isEmptyString()   == false);
    MOON_ASSERT(str3->isConstString()   == false);
    MOON_ASSERT(str3->getStringLength() == std::strlen(result));

    // Comparisons:
    MOON_ASSERT(str1->cmpEqual(*str2) == false);
    MOON_ASSERT(str2->cmpEqual(*str3) == false);
    MOON_ASSERT(str1->compare(*str2)  != 0);
    MOON_ASSERT(str2->compare(*str3)  != 0);

    // Binary ops:
    MOON_ASSERT(Str::binaryOp(OpCode::LogicOr,  *str1, *str2).toBool() == true);
    MOON_ASSERT(Str::binaryOp(OpCode::LogicAnd, *str1, *str2).toBool() == true);
    str1->clear();
    MOON_ASSERT(Str::binaryOp(OpCode::LogicOr,  *str1, *str2).toBool() == true);
    MOON_ASSERT(Str::binaryOp(OpCode::LogicAnd, *str1, *str2).toBool() == false);
    str2->clear();
    MOON_ASSERT(Str::binaryOp(OpCode::LogicOr,  *str1, *str2).toBool() == false);
    MOON_ASSERT(Str::binaryOp(OpCode::LogicAnd, *str1, *str2).toBool() == false);

    MOON_ASSERT(str1->isEmptyString() == true);
    MOON_ASSERT(str2->isEmptyString() == true);
    MOON_ASSERT(str3->isEmptyString() == false);

    releaseRcString(rstr);
    freeRuntimeObject(vm, str1);
    freeRuntimeObject(vm, str2);
    freeRuntimeObject(vm, str3);

    logStream() << "Moon: Script String test passed.\n";
}

static void testScriptArray(VM & vm)
{
    // Array of Int32's (intTypeId="int")
    auto array = Array::newEmpty(vm, vm.types.intTypeId, /* capacityHint = */ 5);

    // Initial invariants:
    MOON_ASSERT(array->isEmptyArray()     == true);
    MOON_ASSERT(array->isSmallArray()     == true);
    MOON_ASSERT(array->isDynamicArray()   == false);
    MOON_ASSERT(array->getArrayLength()   == 0);
    MOON_ASSERT(array->getItemSize()      == sizeof(Int32));
    MOON_ASSERT(array->getArrayCapacity() >= 0);

    constexpr int M = 5;
    constexpr int N = 15;
    const Int32 testData[M] = { 1, 2, 3, 4, 5 };

    // Append the contents of another array:
    auto other = Array::newFromRawData(vm, vm.types.intTypeId,
                                       testData, arrayLength(testData),
                                       sizeof(Int32), Variant::Type::Integer);

    MOON_ASSERT(other->getItemSize()    == sizeof(Int32));
    MOON_ASSERT(other->getArrayLength() == arrayLength(testData));
    array->push(*other);

    // Append some more items:
    for (int i = 0; i < N; ++i)
    {
        Variant var{ Variant::Type::Integer };
        var.value.asInteger = i + M + 1;
        array->push(var);
    }

    MOON_ASSERT(array->isEmptyArray()     == false);
    MOON_ASSERT(array->isSmallArray()     == false);
    MOON_ASSERT(array->isDynamicArray()   == true);
    MOON_ASSERT(array->getArrayLength()   == N + other->getArrayLength());
    MOON_ASSERT(array->getItemSize()      == sizeof(Int32));
    MOON_ASSERT(array->getArrayCapacity() >= N);

    // Change backing store capacity:
    array->reserveCapacity(128);
    MOON_ASSERT(array->isSmallArray()     == false);
    MOON_ASSERT(array->isDynamicArray()   == true);
    MOON_ASSERT(array->getArrayLength()   == N + other->getArrayLength());
    MOON_ASSERT(array->getArrayCapacity() >= 128);

    // Validate:
    for (int i = 0; i < array->getArrayLength(); ++i)
    {
        Variant var = array->getIndex(i);
        MOON_ASSERT(var.type == Variant::Type::Integer);
        MOON_ASSERT(var.value.asInteger == i + 1);
    }

    // Pop values:
    array->pop(1);
    array->pop(2);
    array->pop(2);
    MOON_ASSERT(array->getArrayLength() == N + other->getArrayLength() - 5);

    // Reset every remaining element:
    for (int i = 0; i < array->getArrayLength(); ++i)
    {
        Variant var{ Variant::Type::Integer };
        var.value.asInteger = 42;
        array->setIndex(i, var);
    }
    // Validate the rest:
    for (int i = 0; i < array->getArrayLength(); ++i)
    {
        Variant var = array->getIndex(i);
        MOON_ASSERT(var.type == Variant::Type::Integer);
        MOON_ASSERT(var.value.asInteger == 42);
    }

    array->clear();
    MOON_ASSERT(array->isEmptyArray()   == true);
    MOON_ASSERT(array->isDynamicArray() == true); // Once it goes dynamic, it never returns to small-array
    MOON_ASSERT(array->getArrayLength() == 0);
    MOON_ASSERT(array->getItemSize()    == sizeof(Int32));

    // Now let's try pushing a few floats to test the implicit type conversion:
    for (int i = 0; i < N; ++i)
    {
        Variant var{ Variant::Type::Float };
        var.value.asFloat = static_cast<Float64>(i) + 0.1;
        array->push(var);
    }
    // And validate (now truncated integers):
    for (int i = 0; i < N; ++i)
    {
        Variant var = array->getIndex(i);
        MOON_ASSERT(var.type == Variant::Type::Integer);
        MOON_ASSERT(var.value.asInteger == i);
    }

    // And finally, test the elusive 'Any' type:
    auto anyArray = Array::newEmpty(vm, vm.types.anyTypeId, /* capacityHint = */ 2);
    {
        // Put a few integers:
        for (int i = 0; i < N; ++i)
        {
            Variant var{ Variant::Type::Integer };
            var.value.asInteger = i;
            anyArray->push(var);
        }
        // A few floats...
        for (int i = 0; i < N; ++i)
        {
            Variant var{ Variant::Type::Float };
            var.value.asFloat = static_cast<Float64>(i) + 0.5;
            anyArray->push(var);
        }

        // A couple ranges:
        Variant r{ Variant::Type::Range };
        r.value.asRange = Range{ 0, 10 };
        anyArray->push(r);
        r.value.asRange = Range{ -1, 1 };
        anyArray->push(r);

        // And finally, other Anys:
        Variant a{ Variant::Type::Any };
        a.anyType = Variant::Type::Integer;
        a.value.asInteger = 0xAA;
        anyArray->push(a);
        a.anyType = Variant::Type::Float;
        a.value.asFloat = 3.14;
        anyArray->push(a);

        // Now validate the above:
        int i = 0;
        for (int j = 0; j < N; ++j)
        {
            Variant var = anyArray->getIndex(i++);
            MOON_ASSERT(var.type    == Variant::Type::Any);
            MOON_ASSERT(var.anyType == Variant::Type::Integer);
            MOON_ASSERT(var.value.asInteger == j);
        }
        for (int j = 0; j < N; ++j)
        {
            Variant var = anyArray->getIndex(i++);
            MOON_ASSERT(var.type    == Variant::Type::Any);
            MOON_ASSERT(var.anyType == Variant::Type::Float);
            MOON_ASSERT(var.value.asFloat == static_cast<Float64>(j) + 0.5);
        }

        // Two ranges and two more numbers:
        const Variant r0 = anyArray->getIndex(i++);
        const Variant r1 = anyArray->getIndex(i++);
        MOON_ASSERT(r0.type    == Variant::Type::Any);
        MOON_ASSERT(r0.anyType == Variant::Type::Range);
        MOON_ASSERT(r1.type    == Variant::Type::Any);
        MOON_ASSERT(r1.anyType == Variant::Type::Range);
        MOON_ASSERT(r0.value.asRange.begin ==  0 && r0.value.asRange.end == 10);
        MOON_ASSERT(r1.value.asRange.begin == -1 && r1.value.asRange.end == 1);

        const Variant a0 = anyArray->getIndex(i++);
        const Variant a1 = anyArray->getIndex(i++);
        MOON_ASSERT(a0.type    == Variant::Type::Any);
        MOON_ASSERT(a0.anyType == Variant::Type::Integer);
        MOON_ASSERT(a1.type    == Variant::Type::Any);
        MOON_ASSERT(a1.anyType == Variant::Type::Float);
        MOON_ASSERT(a0.value.asInteger == 0xAA);
        MOON_ASSERT(a1.value.asFloat   == 3.14);
    }

    freeRuntimeObject(vm, array);
    freeRuntimeObject(vm, other);
    freeRuntimeObject(vm, anyArray);

    logStream() << "Moon: Script Array test passed.\n";
}

#include "semantic_check.hpp"

// ================================================================================================

int main(int argc, const char * argv[])
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

        constexpr std::size_t minConstStrLen = sizeof(std::string) - (sizeof(std::size_t) * 2);
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

    try
    {
        ParseContext parseCtx;

        std::string srcFile = "test.ml";
        std::string currText;

        VM vm;//{false, VM::DefaultStackSize};
        Compiler compiler;
        Lexer lexer{ parseCtx, &std::cin };
        Parser parser{ parseCtx };
        VarInfoTable varInfo{ parseCtx };

        parseCtx.lexer     = &lexer;
        parseCtx.parser    = &parser;
        parseCtx.symTable  = &compiler.symTable;
        parseCtx.syntTree  = &compiler.syntTree;
        parseCtx.varInfo   = &varInfo;
        parseCtx.vm        = &vm;
        parseCtx.currText  = &currText;
        parseCtx.srcFile   = &srcFile;

        //parseCtx.debugMode = false;
        //parseCtx.enableWarnings = false;

        logStream() << (parser.parse() == 0 ? "\n-- FINISHED OK --" : "\n-- FINISHED WITH PARSE ERROR --") << "\n\n";
        //logStream() << compiler.symTable << "\n";
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

        testScriptString(vm);
        testScriptArray(vm);

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
