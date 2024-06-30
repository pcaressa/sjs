# S.js - Self JavaScript

## (c) 2024 by Paolo Caressa

## Introduction

### Foreword

S.js (Self JavaScript) is a didactic compiler/interpreter for a classic subset of JavaScript, written in JavaScript itself, namely in the subset it implements, so that S.js can compile itself.

The project was the subject of my talk at the first Coderful conference in Catania (june 2024).

It displays some classic techniques that may be worth to know and to use elsewhere, although the main purpose of the program was to let its author to have some fun.

Enjoy finding and fixing its bugs ;-)

P

### Interpreter, compiler or what?

S.js can run in a browser (or it can be launched by node.js for what that matters, see below) and it does the following:

- it takes a source text (string) and produces a list of all tokens parsed from it;
- it takes a token list and compile it as a JavaScript program into a runtime object;
- it takes the runtime object and execute it.

The runtime object contains an array `code` where the object code is stored: such a code is just a sequence of JavaScript functions that are executed by the runtime engine, the last step in the previous list.

The front-end and back-end of the compiler are collapsed in a single step: one could disentangle them so to make it possible to compile to real virtual machine (say to produce Web Assembly code) but the purpose here is to to everything within a single JavaScript version.

So, S.js is a compiler since it takes a source code and produces a corresponding object code for a runtime engine; it is an interpreter since the object code is not for a native processor but for a virtual one.

### How to use it?

The compiler code is contained in the [S.js](S.js) file that defines a bunch of functions: if you use node.js you need to modify the [S.js](S.js) file to make it a module (by exporting variables `sjs_scan`, `sjs_compile_block` and `sjs_run`) before including it into a node.js module to use it.

Next, to compile a source text, use

```javascript
sjs_run(sjs_compile_block(sjs_scan(source_text)));
```

On a browser, just launch the [sjs.html](sjs.html) file in the distribution, that displays an old-fashioned vax-like terminal in the browser with some easy to understand buttons on its top to load, run, debug (in the console), parse and compile a program with the compiler.


## The implemented language

This project selects a subset of Javascript, grosso modo corresponding to the earlier versions of the language, that provides all the basic control and data structures needed to write non trivial programs. Indeed, the compiler itself is written in this subset, thus it is able to compile itself.

The main implemented features are:

- block scoping and implicit use strict.
- basic control structures: `do{...}while`, `while{...}`, `if-else`. The only allowed jump instruction is `return`.
- Numbers, strings, Boolean, null, undefined and objects are the built-in data types. Objects include arrays and functions.
- Function definitions, inside expressions, are actually closures, as expected.

Functions can only be defined as the result of an expression, thus as

```javascript
let f = function(x1,...,xn) {...};
```

There's no `function f(x1,...,xn) {...}` definition nor arrow function definition `(x1,...,xn)=>{...}`.

The Symbol object is not implemented.

### Accepted syntax

In this section I will formally describe the syntax the interpreter accepts. As meta-language I use the W3C variant of the classic EBNF metalanguage by Niklaus Wirth, see [Extensible Markup Language (XML) 1.0 (Fifth Edition), section 6](https://www.w3.org/TR/xml/#sec-notation).


#### Tokens of the language

```
/* S means a sequence of spacelike characters. */
S       ::= (#x20 | #x9 | #xD | #xA)+
Digit   ::= [0-9]
Dot     ::= "."
Quote   ::= #x27
DQuote  ::= #x22
Number  ::= Digit+ | Digit* Dot Digit+
String  ::= Quote [^']* Quote | DQuote [^"]* DQuote
Alpha   ::= [A-Z] | [a-z] | "$" | "_"
Alnum   ::= Alpha | Digit
Name    ::= Alpha Alnum*
```

Notice that S.js doesn't allow back-quote string and that numbers are only in decimal fixed point notation, not exponential one.

#### Expressions

JavaScript stems from C the use of expressions not only to produce values but also to alter the computational state somewhat. The S.js implementation of expression is quite complete even if it not fullfil the complete JavaScript standard.

The syntax alone does not give information about execution priorities, see the table below.

```
Expression  ::= S* (Prefix S)* Operand (S* Postfix)* (S* Operator Expression)? S*
Prefix      ::= "-" | "!" | "--" | "++" | "new" | "typeof"
Postfix     ::= Dot S* Name | "[" Expression "]" | "(" (Expression ("," Expression)*)+ ")"
Operand     ::= "false" | "true" | "null" | "undefined"
             | Number | String | Object | Array
             | "(" Expression ")"
             | "function" S* "(" S* (Name (S* "," S* name)*)+ S* ")" S* block
Operator    ::= "=" | "+=" | "-=" | "&&" | "||" | "==" | "!=" | "<" | ">=" | ">" | "<=" | "in" | "+" | "-" | "*" | "/" | "**"
```

There are no postfix operators (they would be "++" and "--") even if it would be easy to add them.

Operators with higher priority number are executed before one with lesser priority: priorities are, from the lowest to the highest, as follws

```
"=", "+=", "-=" have lower priority than
"||" that has lower priority than
"&&" that has lower priority than
"==", "!=", "<", ">", "<=", ">=", "in" that have lower priority than
"+", "-" that have lower priority than
"*", "/" that have lower priority than
"**" that has lower priority than
"new" that has lower priority than
"-" (negation), "!", "++", "--", "typeof" that have lower priority than
"." (membership), "[]" (object member), "()" (function application)
```

#### Instructions

A S.js program is a text file that is interpreted as a block: that means that the compiler adds an initial "{" and final "}" tokens to the text. Then the following syntax is applied

```
Block       ::= S* "{" *S (Instruction *S)* "}" S*
Instruction ::= "do" Block "while" S* "(" Expression ")" S* ";"
              | "if" S* "(" Expression ")" Block ("else" Block)+
              | "for" S* "(" (S* "let" S* Name S* "=" S* Expression)+ S* ";" Expression+ S* ";" Expression+ S* ")" Block
              | "for" S* "(" S* "let" S* Name S* "in" Expression ")" Block
              | "let" *S Name (*S "=" Expression)+ (*S "," *S Name (*S "=" Expression)+)* *S ";"
              | "return" Expression+ *S ";"
              | "throw" Expression ";"
              | "while" S* "(" Expression ")" Block
              | Expression ";"
              | S* ";"
```

There are no `break` nor `continue`, use Boolean variables instead along with conditions in `while` or `for`.

### Builtin objects

Some built-in objects are available, and they are:

- Array.
- Math.
- Number.
- Object.
- String.
- console.

The compiler recognizes a number, string, array or object as belonging to those classes. Thus, the snippet

```javascript
console.log("abc".includes("c"));
console.log([1,2,3].at(1));
console.log(123.456789.toPrecision(4));
```

will work as expected. One can add more built-in objects in the `rt.env.$_outer_$` assignment at the beginning of `sjs_run` function.

The names `null`, `undefined` and `this` will work as expected (up to bugs;-).


## The compiler

The S.js compiler is a single file containing a bunch of functions organized in three groups:

- lexical analyzer
- syntactical analyzer and compiler
- virtual machine engine

## The lexical analyzer

The lexical analyzer reduces to a single function `sjs_scan` to be used as

```javascript
let token_list = sjs_scan(text);
```

Text is any string containing the text of a Javascript program: this text is enclosed between braces to make a block out of it and then tokenized into a token list returned as value. The token list is an array of objects of the form

```javascript
{
    s: // the token: a string unless the token is a number
    t: // token's type: "number", "string", "name" or s in other cases
    l: // number of the line in text where the token occurs
    c: // number of the column in text where the token occurs
}
```

For example `let l = "let";` gives rise to the token list:

```javascript
[
    {s: "{",   t: "{",      l: 0, c: 0},
    {s: "let", t: "let",    l: 1, c: 0},
    {s: "l",   t: "name",   l: 1, c: 4},
    {s: "=",   t: "=",      l: 1, c: 6},
    {s: "let", t: "string", l: 1, c: 9},
    {s: ";",   t: ";",      l: 1, c: 13},
    {s: "}",   t: "}",      l: 0, c: 0}
]
```

So the first `let` token is a keyword (its type is not `name` nor `string`) while the fourth one is a string.

## The syntactical analyzer/code generator

Once the source text has been tokenized into a token list, the latter can be parsed and compiled by the `sjs_compile_block` function, that works as:

```javascript
let runtime = sjs_compile_block(token_list);
```

If the compilation succeeds, a "runtime object" is returned that can be run by the virtual machine engine (otherwise an exception with an error message is thrown).

The runtime object is returned by `sjs_compile_block` containing at least the following fields, needed during compilation (actually, the runtime object contains all opcode definitions, thus functions that implement machine code instructions, such as `PUSH` etc. A runtime object is created via the `sjs_rtlib`):

```javascript
{
    code: // an array that contains the compiled code
    ic: // (instruction counter) a number used to scan array code
}
```

At runtime, the execution of a runtime object `rt` runs as follows:

```javascript
rt.ic = 0;
while (rt.ic < rt.code.length) {
    rt.code[rt.ic++]();
}
```

(the actual code is more complicated also because `code` elements contain debug information but in these explanations I'll ignore that).

Thus `code` is parsed element-wise and each of its elements is executed. Notice that the `ic` index is advanced before executing the function supposed to be the element of `rt.code[ic]`.

The elements of the `code` array are JavaScript functions implementing a single instruction of the J.js virtual machine: they are executed at run time and they use a stack to hold temporary values and to return values to subsequent functions. Sometimes, they also read the next `code` element.

For example, the `PUSH` instruction reads the object following it in `code` and pushes it on the stack advancing `ic` by 1, so to prevent the execution loop to try to execute a number as if it is a function; also, the `JP` instruction performs an unconditioned jump to another instruction I, which is identified by the offset between the index in `code` of I and of the current one and increases `ic`, too.

So, suppose `code` to be:

```
code = [PUSH, 1, JP, -3]
```

This is a crazy infinite loop that pushes 1 on the stack and repeat from the first index the execution of code: indeed the `JP` instruction reads the number located at `code[ic]` that is the number following it in `code` and sum that number to `ic`: of course, changing `ic` has the effect to set which is the next instruction to execute, thus a jump.

### Compiling instructions

The `sjs_compile_block` function uses a top-down approach to parse instructions, thus a simple algorithm in which each grammar class is dealt with by a function, such as in the following pseudo-code:

```
let token = first token of the instruction;
case token:
    when "do": sjs_compile_do
    when "for": sjs_compile_for
    when "if": sjs_compile_if
    when "let": sjs_compile_let
    when "return": sjs_compile_return
    when "throw": sjs_compile_throw
    when "while": sjs_compile_while
    else: sjs_compile_expression.
```

Each of the subfunctions deal with the specific syntax of the instruction it needs to compile. This is iterater for all instructions parsed from the token list. The actual JavaScript code is a bit more complicated but it is essentially as the pseudocode above.

To compile JavaScript instructions into machine code instructions, the latter need to be available by the compiler: that is done by inserting them as keys into the runtime object. Indeed, the code compiled in the code key of that object it is not an opcode but directly a JavaScript object, typically the function implementing the machine code instruction.

The compilation of the single instructions is straightforward and uses machine language instructions. At runtime, the runtime objects contains also two stacks `stack` and `dump` (a hommage to the classical Peter Landin SECD machine) and an object `env` that contains all variable associations at current scope.

Let me describe briefly as each JavaScript instruction is compiled.

#### `do-while`

The instruction

```Javascript
do {I1; ...; In} while (E);
```

is compiled as follows (instructions preceded by a `label:` are marked by that label, so that a jump instruction can resume execution from that point)

```
i0: I1
    ...
    In
    E
    JPNZ i0
```

Thus the sequence of instructions is compiled, then the expression is compiled: the latter leaves in the stack its value, and the `JPNZ` function pops it and, if it is non zero (or non `null`, `undefined`, `false`) perform a jump to location `i0`, else execution continues (actually, in the `code` element following `JPNZ` there's not `i0` but the offset between that element and `i0`).

#### `for`

#### `for-in`

#### `if-else`

Consider for example the dummy snippet

```javascript
if (false) {0;} else {1;}
```

This is compiled as (numbers in brackets on the left are indexes in the `code` array, thus memory addresses of machine language instructions)

```
[0] PUSH false
[2] JPZ 8
[4] PUSHENV
[5] PUSH 0
[7] DROP
[8] POPENV
[9] JP 6
[11] PUSHENV
[12] PUSH 1
[14] DROP
[15] POPENV
```

When executed at run-time, this code executes the `PUSH` JavaScript function (a member of the runtime object) that parses from 

#### `let`

#### `return`

#### `throw`

#### `while`

### Compiling expressions


## The virtual machine engine


### Variables

The runtime environment object contains an attribute `env` which is used to store symbols defined in the current scope. Each variable definition `let v = x` corresponds to `env.v = x`.

When a variable's name v is parsed, the compiler dumps the code

    REF v

At runtime, the `REF` instruction looks for the name `v` in the environment: if it finds it, the object `r = {env: e, at: [v]}` is pushed on the stack, where `e` is the environment containing the variable and `v` the variable's name.

Although when a variable is quoted in the text the compiled code creates a reference for it at runtime, often only the value of the reference is needed, for example on the RHS of an assignment, so that the virtual machine uses the `VAL` instruction to convert the reference `r` on the stack into its value `r.env[at[0]]`.

An example of usage of references is the following: consider

    x[1] = x[0];

This is compiled as

    REF "x"
    PUSH 1
    DEREF
    REF "x"
    PUSH 0
    DEREF
    SET
<
The `REF` instruction looks for the attribute `x` inside `rt.env` and push the reference `{env: {..., x:v, ...}, at: [x]}` on the stack. Next `PUSH` pushes `1` and the `DEREF` instruction modifies the reference to `{env: {..., x:v, ...}, at: [x,1]}`. The second sequence `REF-PUSH-DEREF` does the same leaving `{env: {..., x:v, ...}, at: [x,0]}` on the stack.

Finally the `SET` operator pops two references and assigns the value of the topmost to the one below, in this case the final result will be that the attribute `1` of the object `v` will be set to the value of `v[0]`.

This device is not efficient, since operators that just need the value of a variable will perform taking the reference and then the value, but it is simple. One could get rid of it by defining a new instruction `REFVAL` that retrieve the value of a variable and pushes it on the stack, introducing an optinization in the code by converting sequences `REF s VAL` into `REFVAL s`.



## Opcodes

The object code is just a list whose items can be:

    - Strings.
    - Numbers.
    - True, false, null or undefined.
    - Functions.

Values such as strings or null are pushed on the stack when they are parsed from the object code, while functions are executed. Most functions implements primitive operations, and they are follows:

In the following description, stack operations are indended on the data stack, while parse operations are intended on the code.

    - LET() pop s, pop v, creates a new variable with name s and value v.
    - VAR() pop s, looks for s inside the current environment, pushing the value on the stack.
    - JP() parse n, set ic = n.
    - JPZ() pop v: if v in [0, false, null, undefined] then perform JP(), else parse n
    - JPNZ() pop v: if v not in [0, false, null, undefined] then perform JP(), else parse n

    - ADD() pop n1, pop n2, push n2 + n1
    - SUB() pop n1, pop n2, push n2 - n1
    - MUL() pop n1, pop n2, push n2 * n1
    - DIV() pop n1, pop n2, push n2 * n1
    - EQ() pop v1, pop v2, push v2 == v1
    - NE() pop v1, pop v2, push v2 != v1
    - LT() pop v1, pop v2, push v2 < v1
    - GT() pop v1, pop v2, push v2 > v1
    - LE() pop v1, pop v2, push v2 <= v1
    - GE() pop v1, pop v2, push v2 >= v1


## Appendix A

EBNF (Extended Backus-Naur Form) is a classic metalanguage used by Niklaus Wirth to describe the syntax of his programming languages. A variant of it was used by Kernighan and Ritchie to describe C syntax and adopted by many authors who borrowed from them.

Metasymbols are just identifiers (letters and underscores), spaces are ignored, and metasymbol definitions are written as Javascript assignment:

    metasymbol = definition

The definition may be:

- a metasymbol
- the `character` metasymbol that denotes any single character symbol of the underlying alphabet
- the 
- a symbol (thus a symbol of the described language) enclosed between " and ", ' and ' 