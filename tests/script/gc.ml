
// ----------------------------------
// Testing the Moon Garbage Collector
// ----------------------------------

type SubObj struct
    test: str,
end

type MyObj struct
    int_member:   int,
    float_member: float,
    str_member:   str,
    obj_member:   SubObj,
end

// Globals have program lifetime.
let g_foo0 = MyObj{ 0, 0.0, "global foo 0", SubObj{ "global sub-obj 0" } };
let g_null: MyObj = null;

// Array members live for as long as the parent array.
let g_array = [
    MyObj{ 1, 1.1, "global foo 1", SubObj{ "global sub-obj 1" } },
    MyObj{ 2, 2.2, "global foo 2", SubObj{ "global sub-obj 2" } },
    g_foo0,
    g_null
];

// Function-scope objects get reclaimed when the function exists.
func alloc_locals()
    let local_foo = MyObj{ 22, 3.3, "local foo", SubObj{ "local sub-obj" } };
    let local_array = [ SubObj{ "one" }, SubObj{ "two" }, SubObj{ "three" } ];

    if gc_need_to_collect() then
        println("GC should run.");
    else
        println("GC won't run.");
    end
end

for i in 0..100 do
    alloc_locals();
end

// Manually clear any left over objects from the function calls.
gc_collect();

println("Finished running test script ", SRC_FILE_NAME);

