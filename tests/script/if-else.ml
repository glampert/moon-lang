
// -----------------------------------------------
// A few basic tests for if-else statements
// -----------------------------------------------

if 1 < 2 then
    println("This will print.");
elseif 1 != 0 then
    println("And this wont.");
else
    println("And neither will this.");
end

let a = null;
let b = "hello";
let c = "";

if a == null then // true
    println("\'a\' is null");
end
if b != null then // true
    println("\'b\' is not null");
end
if c == null then // false
    println("\'c\' is null");
end

if a or b then // true
    println("\'a\' or \'b\' not null");
end
if a and b then // false
    println("\'a\' and \'b\' not null");
end

if not a then // true
    println("not \'a\'");
end
if not b then // false
    println("not \'b\'");
end
if not c then // true
    println("not \'c\'");
end

let numbers = [0, 1, 2, 3, 4];
if numbers[0] != 0 then
    assert(false);
elseif numbers[1] != 1 then
    assert(false);
elseif numbers[2] != 2 then
    assert(false);
elseif numbers[3] != 3 then
    assert(false);
elseif numbers[4] != 4 then
    assert(false);
else
    println("OK");
end

println("Finished running test script ", SRC_FILE_NAME);

