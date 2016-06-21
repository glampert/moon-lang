
// ------------------------------------------------------
// Script functions that will be called from the C++ code
// ------------------------------------------------------

func script_func0()
    println(SRC_FUNC_NAME);
    println("No arguments\n");
end

func script_func1(arg0: int) -> int
    println(SRC_FUNC_NAME);
    println("arg0 = ", arg0, "\n");
    return 321;
end

func script_func2(arg0: int, arg1: float) -> float
    println(SRC_FUNC_NAME);
    println("arg0 = ", arg0);
    println("arg1 = ", arg1, "\n");
    return -1.5;
end

func script_func3(arg0: int, arg1: float, arg2: str) -> str
    println(SRC_FUNC_NAME);
    println("arg0 = ", arg0);
    println("arg1 = ", arg1);
    println("arg2 = ", arg2, "\n");
    return "testing, 123";
end

func script_func_varargs(args...)
    println(SRC_FUNC_NAME);
    for arg in args do
        println("var arg: ", arg);
    end
    println();
end

println("Finished running test script ", SRC_FILE_NAME);

