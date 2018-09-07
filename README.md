# SML With Assertions #

In this project, we implement native support for function contracts in the form of requires and ensures statements (prerequisites and postrequisites) for SML-NJ. To achieve this goal, we recreated the SML Parser as a code translater which accomodates a new syntax for preconditions and postconditions and maps this to syntax which is coherent to the SML GRAMMAR. 


## Getting Started ##

Enter the main folder and run the parse.py file on the command prompt as follows:

python parse.py -f file1 file2... [-c sources_file] 

The files given using the -f flag should include all the files compiled in the sources file.

The file given using the -c flag should be the sources.cm file necessary to compile the above files.

* However the [-c] flag is optional.


## Prerequisites for Usage ##

Standard ML installation:
http://smlnj.org/

Python 3 installation:
https://www.python.org/


### Usage Instructions ###

To run the program, you need to use the following commands to test you files:

* To run the parser open and terminal and navigate to the filepath of this folder. Add the sml files you want to test into this folder and run the following command. 

filepath>> python parse.py -f sample1.sml sample2.sml ...  -c sources.cm

* This indicates that the compilation was successful

filepath>> val it = true : bool

### Representing Assertions in SML ###

To represent assertions in your .sml file, adhere to the following template:

    (*! 
      REQUIRES: (boolean exp #1) andalso (boolean exp #2) andalso ... 
      ENSURES: (boolean exp #1) andalso (boolean exp #2) andalso ...
    !*)
    fun foo (...) = ... 

where: 

    "(*!"       - This token opens an assertion block for the function foo.

    "REQUIRES"  - This token is used to initiate a 'requires' assertion.

    "ENSURES"   - This token is used to initiate a 'ensures' assertion.

    "!*)"       - This token is used to close the assertion block for the function foo.


To refer to the output of the function in your ENSURES boolean expressions, use the variable name  'result'. For example:

    (*!
      REQUIRES: n >= 5 andalso b > 0
      ENSURES: result >= 5 
    !*)
    fun pow (n: int, b as 0: int) = 1
      | pow (n, b) = n * pow (n, b - 1)


For mutually recursive functions, the assertion block for second function must be placed after the "and" token. For example:

    (*!
      REQUIRES: n >= 0
      ENSURES: true
    !*)
    fun even (n as 0: int) = true
      | even n = odd (n-1)
    and 
    (*!
      REQUIRES: n >= 0
      ENSURES: true
    !*)
        odd (n as 0: int) = false
      | odd n = even (n-1)

For nested functions, the assertion block must be placed right before the definiton of the local function.

    (*!
      REQUIRES: ...
      ENSURES: ...
    !*)
    fun OuterNestedFun (...) =
       let
           (*!
             REQUIRES: ...
             ENSURES: ...
           !*)
           fun InnerNestedFun (...) = ...
           val (...) = ...
       in
           ...
       end

If there is an assertion failure, the Compilation Manager will raise the following FAIL error:

Fail [fun (function name) error: (Requires/Ensures) failure on line (line no. of assertion tag)]


## Versioning ##

We use [GitHub](http://github.com/) for versioning. For the versions available, check out our project repository [SML-With-Assertions](https://github.com/Julian-Sam/SML-with-Assertions). 

## Authors ##

* **Julian Sam**   - [Julian-Sam](https://github.com/Julian-Sam)
* **Sameer Ahmad** - [SameerAhmad2](https://github.com/SameerAhmad2)

## Acknowledgments ##

* We would like to acknowledge Professor Giselle Reis for guiding us through the project.
