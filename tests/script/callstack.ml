
// -----------------------------------------------
// Testing nested calls for VM::printStackTrace()
// -----------------------------------------------

func fun4()
    println("In ", SRC_FUNC_NAME);
    // This will throw and exception in the C++ side, so we
    // can call printStackTrace() in the exception handler.
    panic("Foobar");
end

func fun3()
    println("In ", SRC_FUNC_NAME);
    fun4();
end

func fun2()
    println("In ", SRC_FUNC_NAME);
    fun3();
end

func fun1()
    println("In ", SRC_FUNC_NAME);
    fun2();
end

func main()
    println("In ", SRC_FUNC_NAME);
    fun1();
end

main();

