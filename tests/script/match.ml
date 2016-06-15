
// -----------------------------------------------
// Testing the 'match with' statement
// -----------------------------------------------

let test1 = 1;
match test1 with
    case 0  -> println("zero"); end
    case 1  -> println("one");  end
    case 2  -> println("two");  end
    default -> println("default: ", test1); end
end

let test2 = "G";
match test2 with
    case "0" .. "9" -> println("0-9 digit");        end
    case "a" .. "z" -> println("a-z lowercase");    end
    case "A" .. "Z" -> println("A-Z uppercase");    end
    default         -> println("default: ", test2); end
end

let test3 = "hello";
match test3 with
    case "hello" -> println(test3);     end
    case "world" -> println("world");   end
    default      -> println("goodbye"); end
end

let test4 = 5;
match test4 with
    case [0, 1, 2, 3, 4, 5, 6, 7, 8, 9] ->
        println("zero to nine");
    end
    default ->
        println("unmatched");
    end
end

let test5 = "one";
let collection = ["zero", "one", "two", "three", "four"];
match test5 with
    case collection ->
        println("in the collection");
    end
    default ->
        println("not in the collection");
    end
end

println("Finished running test script ", SRC_FILE_NAME);

