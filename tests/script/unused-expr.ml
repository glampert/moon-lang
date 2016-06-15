
// -----------------------------------------------
// Unused expression result samples (dead code
// that gets stripped and a warning is generated)
// -----------------------------------------------

func get_foo() -> int
    println("get_foo called");
    return 10;
end

type Foo struct
    int_member: int,
    str_member: str,
end

// Currently we won't warn about unused variables or functions.
let a = 42;                 // OK
let b = 1;                  // OK
let c: int;                 // OK
let d = [1,2,3];            // OK
let f = Foo{ 42, "hello" }; // OK

a;                  // warning

42;                 // warning

"test";             // warning

d[1];               // warning

[1, 2, 3];          // warning

0..9;               // warning

Foo{ 123, "test" }; // warning

f.int_member;       // warning

a as float;         // warning

type_of(a);         // warning

-a; // warnings
+b;

not a; // warnings
a or  b;
a and b;

a + b; // warnings
a * b;
a == b;
a <= b;

a + get_foo();      // warning and the call gets stripped
let x = get_foo();  // OK
a += b;             // OK
assert(a == 43);    // OK

get_foo();          // OK
println("a = ", a); // OK

println("Finished running test script ", SRC_FILE_NAME);

