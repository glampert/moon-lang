
## Moon scripting language compiler and VM

[![Build Status](https://travis-ci.org/glampert/moon-lang.svg)](https://travis-ci.org/glampert/moon-lang)

`Moon` is a custom scripting language that borrows some of its syntax from
[Lua](http://www.lua.org/) and [Rust](https://www.rust-lang.org/).

This is mainly a toy/hobby project to learn more about compiler design, Virtual Machines and interpreters.

The name is just a play on the meaning of Lua (Moon in Portuguese). I chose the name on purpose
to leave it clear this is a lame rip-off on the syntax of the popular scripting language.
This project is not affiliated to [MoonScript](https://github.com/leafo/moonscript).

### Building

To build the project on Mac and Linux, use the provided Makefile. Before building, make sure you have:

- [Flex](http://flex.sourceforge.net/) version `2.6` or newer installed.
- [Bison](http://www.gnu.org/software/bison/) version `3.0.4` or newer installed.
- A C++11 compiler. GCC v5 or above is recommended for Linux and Clang v6 or newer for Mac OSX.

To install Bison and Flex on Mac OSX, [brew](http://brew.sh/) is the tool of choice. If you experience problems
with the system path, make sure to check out [this thread](http://stackoverflow.com/a/29053701/1198654).

On Linux, you can use `apt` or whichever is your package manager of choice.
You might need to download and build Bison yourself if a recent binary image is not available.

**Building on Windows** is currently not officially supported. The source code should be fully portable,
so all that should be required for a Windows build is to create a Visual Studio project. I was not able to
find a binary distribution or even compilable source for the required Bison & Flex versions for Windows,
so I have included the generated source files in the `generated/` dir. You'll be able to build on Windows
without Bison & Flex, but you won't be able to generate new files if the parser or lexer are changed.

### Gallery

Hello world in Moon Lang:

```rust
// Once
println("Hello world!");

// A few times
for i in 0..5 do
    println(i, ": Hello world!");
end
```

Declaring variables:

```rust
let i  = 42;              // An integer number
let pi = 3.141592;        // A floating-point number
let s  = "hello";         // A string
let r  = 0..10;           // A range
let a  = ["a", "b", "c"]; // An array of strings
```

User-defined structs and enums:

```rust
type Foo struct
    int_member:   int,
    float_member: float,
    str_member:   str,
end

// Declaring an instance of a structure:
let foo = Foo{ 1, 2.2, "4" };

type Colors enum
    Red,
    Green,
    Blue,
end

// Use enum constants:
let color = Colors.Blue;

// Type alias:
type Bar = Colors;
```

Recursive Fibonacci:

```rust
func fibonacci(n: int) -> int
    if n <= 1 then
        return n;
    end
    return fibonacci(n - 1) + fibonacci(n - 2);
end

let fib = fibonacci(15);
println("Fibonacci of 15 = ", fib);
```

Iterative Fibonacci:

```rust
func fibonacci(n: int) -> int
    let a: int = 0;
    let b: int = 1;
    let c: int = 0;

    if n == 0 then
        return a;
    end

    let count = n + 1;
    for i in 2..count do
        c = a + b;
        a = b;
        b = c;
    end
    return b;
end

let fib = fibonacci(15);
println("Fibonacci of 15 = ", fib);
```

C++ interface:

```cpp
// Minial sample of how to run a Moon Lang script from C++
#include "compiler.hpp"
#include "vm.hpp"

int main()
{
    moon::Compiler compiler; // Script parsing and bytecode generation.
    moon::VM       vm;       // Executes the bytecode generation by a Compiler.

    try
    {
        compiler.parseScript(&vm, "my_script.ml");
        compiler.compile(&vm);
        vm.execute();
    }
    catch (const moon::BaseException & e)
    {
        // Handle a compilation error or a runtime-error.
        // The exeption type will differ accordingly
        // (CompilerException, RuntimeException, etc).
    }
}
```

### License

This project's source code is released under the [MIT License](http://opensource.org/licenses/MIT).

