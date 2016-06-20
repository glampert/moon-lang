
// --------------------------------------
// Test script that imports other scripts
// --------------------------------------

import "tests/script/test-import-1.ml";
import "tests/script/test-import-2.ml";

// Functions declared in the imported scripts:
import1_func();
import2_func();

func main()
    // Variables declared in the imported scripts:
    println("import1_var = ", import1_var);
    println("import2_var = ", import2_var);
    println("Finished running test script ", SRC_FILE_NAME);
end

main();

