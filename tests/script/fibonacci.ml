
// -----------------------------------------------

func fibonacci_recursive(n: int) -> int
    if n <= 1 then
        return n;
    end
    return fibonacci_recursive(n - 1) + fibonacci_recursive(n - 2);
end

let fib1 = fibonacci_recursive(15);
println("Fibonacci of 15 = ", fib1);
assert(fib1 == 610);

// -----------------------------------------------

func fibonacci_iterative(n: int) -> int
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

let fib2 = fibonacci_iterative(15);
println("Fibonacci of 15 = ", fib2);
assert(fib2 == 610);

// -----------------------------------------------

println("Finished running test script ", SRC_FILE_NAME);

