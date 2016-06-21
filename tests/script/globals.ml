
// ----------------------------------------------------
// Global variables that will be edited by the C++ code
// ----------------------------------------------------

let an_integer = 42;
let a_float    = 2.5;
let a_range    = 0..9;
let a_string   = "hello from script";
let a_tid      = type_of(int);

func print_globals()
    println("int    = ", an_integer);
    println("float  = ", a_float);
    println("range  = ", a_range);
    println("string = ", a_string);
end

println("Finished running test script ", SRC_FILE_NAME);

