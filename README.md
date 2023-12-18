## P2C 

#### Project Description 
Conversion of Python files to C

#### Final Product 
##### How to Run

Uncomment `run.sh` to test functionality and look at code coverage (be sure to uncomment portions that are required to run)

Uncomment `/src/testPythonFiles` to test functionalty for compiling python files into c, and running those files in C. 

#### File Structure

##### `src`

`ast.ml/mli` AST Tree Declaration

`parse.ml/mli` Parses the python file

`lex.ml/mli` Converts the parsed elements into an AST tree

`codegen.ml/mli` Converts AST into C equivalent (including header and source files)

`codegenutil.ml/mli` Helper functions for converting a python file to c

`convert.ml/mli` Entry point for our CLI.

##### `test`

`codegenTests.ml` - tests for code generation

`endtoendtests.ml` - tests for end to end pipeline testing

`lextests.ml` - testing for lexing

`parsetests.ml` - testing for parsing 

`tests.ml` - unused - place holder for additional tests


#### Checkpoint 
**What's been Implemented** 

We've implemented functionality for lexing, parsing, and converting the AST trees to C code. 

To test, run `dune test` (all tests should pass).

Or, run `run.sh` to automatically test, get coverage, and build the project.

**Next steps** 

End to end testing 

Writing functions for file I/O

Header files
