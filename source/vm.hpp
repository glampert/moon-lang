
// ================================================================================================
// -*- C++ -*-
// File: vm.hpp
// Author: Guilherme R. Lampert
// Created on: 06/07/15
// Brief: The Virtual Machine class and Garbage Collector helper.
// ================================================================================================

#ifndef MOON_VM_HPP
#define MOON_VM_HPP

#include "runtime.hpp"
#include <utility>
#include <vector>

namespace moon
{

// ========================================================
// class GC:
// ========================================================

class GC final
{
public:

    GC() = default;

    // Not copyable.
    GC(const GC &) = delete;
    GC & operator = (const GC &) = delete;

    template<typename T, typename... Args>
    T * alloc(Args&&... args)
    {
        //TODO temp. we'll use a pool later on
        T * newObj = (T*) ::operator new(sizeof(T));

        ::new(newObj) T{ std::forward<Args>(args)... };
        linkObject(newObj);
        return newObj;
    }

    Object * linkObject(Object * obj)
    {
        MOON_ASSERT(obj != nullptr);
        obj->gcNext = gcListHead;
        gcListHead  = obj;
        return obj;
    }

    const Object * getGCListHead() const noexcept { return gcListHead; }

private:

    Object * gcListHead = nullptr;
};

// ========================================================
// class VM:
// ========================================================

class VM final
{
public:

    // Default program stack size in Variants.
    static constexpr int DefaultStackSize = 8192;

    // Helper types:
    //
    // TODO NOTES: in the compilation stage these get filled
    // with push_backs. We should optimize to allocate the
    // exact size beforehand.
    //
    using DataVector = std::vector<Variant>;
    using CodeVector = std::vector<Instruction>;

    // Program context:
    Stack         stack;
    Stack         locals;
    DataVector    data;
    CodeVector    code;
    GC            gc;
    TypeTable     types;
    FunctionTable functions;
    Stack::Slice  funcArgs;

    //TODO add a way of accessing all the program
    //globals by name. that will be useful if you
    //just want to use a script to store some runtime
    //configuration data!
    //
    //Also a method to call a script function or registered native
    //call by name will be nice, e.g.: vm.call("foo", [args...], arg_count);
    //
    //A way of defining global script variables via the C++ code

    //
    // VM interface:
    //

    // loadBuiltIns: Load built-in native functions and helper modules.
    // stackSize:    Size in Variants of the program stack. Stack-size is fixed.
    VM(bool loadBuiltIns = true, int stackSize = DefaultStackSize);

    // Not copyable.
    VM(const VM &) = delete;
    VM & operator = (const VM &) = delete;

    void setProgramCounter(int target);
    int getProgramCounter() const noexcept { return pc; }

    void setReturnAddress(int target);
    int getReturnAddress() const noexcept { return retAddr; }

    void setReturnValue(Variant retVal) noexcept { rvr = retVal; }
    Variant getReturnValue() const noexcept { return rvr; }

    void execute();
    void executeSingleInstruction(OpCode op, UInt32 operandIndex);

    // TODO other useful overloads for execute:
    //void execute(int maxInstructions);
    //void execute(int firstInstruction, int maxInstructions);

    // Prints the current Program Counter, data vector and stack.
    void print(std::ostream & os) const;

private:

    // The "Program Counter" (index of the next
    // instruction to be executed from the code vector).
    int pc;

    // Saved by CALL instructions so that we can push it
    // into the stack when entering a script function.
    // Native functions don't require setting the return address.
    int retAddr;

    // The "Return Value Register" (holds the latest return value from a func call).
    Variant rvr;
};

// ========================================================
// Debug printing helpers:
// ========================================================

void printDataVector(const VM::DataVector & data, std::ostream & os);
void printCodeVector(const VM::CodeVector & code, std::ostream & os);

inline std::ostream & operator << (std::ostream & os, const VM::DataVector & data)
{
    printDataVector(data, os);
    return os;
}
inline std::ostream & operator << (std::ostream & os, const VM::CodeVector & code)
{
    printCodeVector(code, os);
    return os;
}
inline std::ostream & operator << (std::ostream & os, const VM & vm)
{
    vm.print(os);
    return os;
}

} // namespace moon {}

#endif // MOON_VM_HPP
