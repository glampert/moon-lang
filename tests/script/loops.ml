
// ----------------------------------
// A few basic tests for script loops
// ----------------------------------

func test_loop_simple()
    let i = 0;
    loop
        if i == 10 then
            break;
        end
        println("looping; i = ", i);
        i += 1;
    end
    assert(i == 10);
    println(SRC_FUNC_NAME, "() - OK\n");
end

test_loop_simple();

// ----------------------------------

func test_while_loop()
    let i: int;

    // Loop with condition:
    i = 0;
    while i < 10 do
        println("looping; i = ", i);
        i += 1;
    end
    assert(i == 10);

    // Infinite loop with break condition:
    i = 0;
    while true do
        if i == 5 then
            break;
        end
        println("looping; i = ", i);
        i += 1;
    end
    assert(i == 5);

    // Test the 'continue' jump:
    let times_printed = 0;
    i = 0;
    while i != 10 do
        i += 1;
        if (i % 2) == 0 then
            println("looping; i = ", i);
            times_printed += 1;
            continue;
        end
    end
    assert(times_printed == 5);
    println(SRC_FUNC_NAME, "() - OK\n");
end

test_while_loop();

// ----------------------------------

func test_for_loop()
    // For on Range literal:
    for i in 0..10 do
//        println("looping; i = ", i);
    end
    println(SRC_FUNC_NAME, "() - OK\n");
end

test_for_loop();

// ----------------------------------

println("Finished running test script ", SRC_FILE_NAME);

