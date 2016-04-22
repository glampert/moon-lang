
// -----------------------------------------------
// A few basic tests for script-declared functions
// -----------------------------------------------

func void_function()
    println("I return nothing");
end

// -----------------------------------------------

func get_bool() -> bool
    return true;
end

assert(get_bool() == true);

// -----------------------------------------------

func get_int() -> int
    return 42;
end

assert(get_int() == 42);

// -----------------------------------------------

func get_float() -> float
    return 4.2;
end

assert(get_float() == 4.2);

// -----------------------------------------------

func get_str() -> str
    return "hello";
end

assert(get_str() == "hello");

// -----------------------------------------------

func get_typeid() -> tid
    return type_of(true);
end

assert(get_typeid() == type_of(true));

// -----------------------------------------------

func get_range() -> range
    return -2..2;
end

assert(get_range() == -2..2);

// -----------------------------------------------

func get_array() -> array
    return ["A", "B", "C", "D"];
end

assert(get_array()[0] == "A");
assert(get_array()[1] == "B");
assert(get_array()[2] == "C");
assert(get_array()[3] == "D");

// -----------------------------------------------

func get_array_index(idx: int) -> str
    let arr = ["A", "B", "C", "D"];
    return arr[idx];
end

assert(get_array_index(0) == "A");
assert(get_array_index(1) == "B");
assert(get_array_index(2) == "C");
assert(get_array_index(3) == "D");

// -----------------------------------------------

let g_number: long = 666;

func get_global() -> long
    return g_number;
end

assert(get_global() == g_number);
assert(get_global() == 666);

// -----------------------------------------------

func get_null_obj() -> object
    return null;
end

assert(get_null_obj() == null);

// -----------------------------------------------

func get_function() -> function
    return get_str;
end

let func_ref = get_function();

assert(get_function() == get_str);
assert(func_ref() == get_str());

// -----------------------------------------------

func get_any() -> any
    let str_val = "hello world";
    return str_val;
end

assert(type_of(get_any()) == type_of(str));
assert(get_any() == "hello world");

// -----------------------------------------------

type MyObject struct
    dummy: int,
end

func get_my_obj() -> MyObject
    return MyObject{ 42 };
end

func get_my_obj_indirect() -> MyObject
    return get_my_obj();
end

assert(type_of(get_my_obj()) == type_of(MyObject));
assert(type_of(get_my_obj_indirect()) == type_of(MyObject));

// -----------------------------------------------

func my_func1(arg: str)
    assert(arg == "indirect call test");
end

func my_func2(arg: int)
    assert(arg == 666);
end

func call_indirect(fn1: function, arg1: str, arg2: int)
    let fn2 = my_func2;
    fn1(arg1); // => can only be resolved at runtime
    fn2(arg2); // => statically resolved to my_func2
end

call_indirect(my_func1, "indirect call test", 666);

// -----------------------------------------------

func test_func_params(p0: int, p1: float, p2: str, p3: bool, p4: array, p5: tid, p6: tid, p7: function)
    assert(p0 == 11);
    assert(p1 == 1.1);
    assert(p2 == "foobar");
    assert(p3 == true);
    assert(p4[0] == 4);
    assert(p4[1] == 5);
    assert(p5 == type_of(array));
    assert(p6 == type_of(function));
    assert(p7 == my_func1);
end

test_func_params(11, 1.1, "foobar", (1 < 2), [4, 5], type_of(array), type_of(my_func1), my_func1);

// -----------------------------------------------

func var_args_func(args...)
    assert(args[0] == 33);
    assert(args[1] == 44);
    assert(args[2] == 55);
    assert(args[3] == "hello world");
    assert(args[4] == 3.14);
    assert(args[5] == my_func1);
end

var_args_func(33, 44, 55, "hello world", 3.14, my_func1);

// -----------------------------------------------

println("Finished running test script ", SRC_FILE_NAME);

