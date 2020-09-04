#### Writing tests

The following three steps are demonstrated
using a file called `./programs/example.c`
(step 1), and 
its corresponding configuration (step 2) and 
executing code (step 3)
within `./Spec.hs`; just search for the string
`example`.

1. Write a C file with a unique name, and
   place it in the `./programs` directory.
2. Fill in its configuration options by opening
   the `./Spec.hs` file and writing 
   a case in the `trueTestCase` function.
   The test case name must match the above
   C file's base name, i.e., the file without
   the ".c" extension. (The `trueTestCase` 
   function uses a syntax called
   [pattern matching](http://learnyouahaskell.com/syntax-in-functions),
   which is similar to `switch` statements in other
   languages.)
3. Write the code that executes your test in
   the body of `main` (again, in `./Spec.hs`).
   This will be three lines of the form:
   
          describe "foo" $ do
	        it "<description of what foo is testing>" $ do
		      runTrueTest "foo"
		  
    Place your three lines below the commented line:
	
	      {- End-to-end tests with no oracle checking -}
	  
    Note that indentation is important, and so are the two
	`$ do` fragments.

#### Running all tests

You can run all tests and generate a coverage report by running
`stack test --fast --test --coverage aca-framework`.
At the end of the test run, Stack will report where it placed the report, 
in some very long path name, e.g.,
`/home/mgerrard/work/alpaca-dev/aca-framework/.stack-work/install/x86_64-linux/lts-9.17/8.0.2/hpc/aca-framework/aca-framework-test/hpc_index.html`.
(*If the tests can run, but no coverage report is 
generated, run* `stack clean`, *followed by the preceding
command.*)
The coverage report will be an HTML page viewable
by any browser, such as Firefox.

#### Running individual tests

Running all tests can take a while, and you may
just want to run a single one within `./Spec.hs`.
To do so (assuming the test you want to run is named `example`),
run the command 
`stack test --fast --test --coverage aca-framework --ta "-m example"`.