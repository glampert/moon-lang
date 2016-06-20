
// ----------------------------------
// Testing user-defined structures
// ----------------------------------

type SubObj struct
    my_str:   str,
    my_any:   any,
    my_array: array,
    my_func:  function,
end

type MyObj struct
    my_float:  float,
    my_int:    int,
    my_subobj: SubObj,
end

func get_num() -> int
    return 0;
end

let obj = MyObj{1.2, 22, SubObj{ "hi", "bye", [1, 2, 3], get_num }};

let f = obj.my_float;
let s = obj.my_subobj.my_str;
let i = obj.my_subobj.my_array[1];

assert(f == 1.2);
assert(s == "hi");
assert(i == 2);

obj.my_subobj.my_str = "3.14";
assert(obj.my_subobj.my_str == "3.14");

let temp: MyObj = obj;
assert(temp == obj); // Object are always references/pointers.

let fptr1: function = get_num;
let fptr2 = obj.my_subobj.my_func;
assert(fptr1 == fptr2);

obj.my_subobj.my_array[1] = 11;
assert(obj.my_subobj.my_array[1] == 11);

obj.my_subobj.my_array[get_num()] = 22;
assert(obj.my_subobj.my_array[get_num()] == 22);

obj.my_subobj.my_array[0] = get_num();
assert(obj.my_subobj.my_array[0] == get_num());

obj.my_int += 10;
assert(obj.my_int == 32);

obj.my_float = 5.0;
assert(obj.my_float == 5.0);

obj.my_float -= 5.0;
assert(obj.my_float == 0.0);

println("Finished running test script ", SRC_FILE_NAME);

