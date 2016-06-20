
// ----------------------------------
// Testing user-defined enums
// ----------------------------------

type Colors enum
    Red,           // = 0
    Green,         // = 1
    Blue,          // = 2
    Yellow,        // = 3
    Last = Yellow, // = 3
end

type Foo1 enum
    Const0 = 2,
    Const1,      // = 3
    Const2 = 10,
    Const3,      // = 11
    Const4 = 44,
    Const5,      // = 45
end

type Foo2 enum
    Const0 = 1337,
    Const1 = Foo1.Const1, // = 3
    Const2,               // = 4
    Const3 = "hi",
    Const4 = true,
    Const5 = 3.14,
    Const6 = Const3,      // = "hi"
    Const7 = Const2,      // = 4
end

//
// Validate the constants:
//

assert(Colors.Red    == 0);
assert(Colors.Green  == 1);
assert(Colors.Blue   == 2);
assert(Colors.Yellow == 3);
assert(Colors.Last   == 3);
assert(Colors.Last   == Colors.Yellow);

assert(Foo1.Const0 == 2);
assert(Foo1.Const1 == 3);
assert(Foo1.Const2 == 10);
assert(Foo1.Const3 == 11);
assert(Foo1.Const4 == 44);
assert(Foo1.Const5 == 45);

assert(Foo2.Const0 == 1337);
assert(Foo2.Const1 == 3);
assert(Foo2.Const2 == 4);
assert(Foo2.Const3 == "hi");
assert(Foo2.Const4 == true);
assert(Foo2.Const5 == 3.14);
assert(Foo2.Const6 == "hi");
assert(Foo2.Const7 == 4);

//
// Assignment to variable:
//

let color = Colors.Green;
assert(color == Colors.Green);

let foo1 = Foo1.Const4;
assert(foo1 == 44);

let foo2 = Foo2.Const6;
assert(foo2 == "hi");

println("Finished running test script ", SRC_FILE_NAME);

