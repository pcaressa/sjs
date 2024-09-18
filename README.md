# S.js - Self JavaScript

## (c) 2024 by Paolo Caressa

## Introduction

S.js (Self JavaScript) is a didactic compiler/interpreter for a classic subset of JavaScript, written in JavaScript itself, namely in the subset it implements, so that S.js can compile itself.

The project was the subject of my talk at the first *Coderful* conference held in Catania (28th june 2024), slide (Coderful_Paolo_2024.pdf)[Coderful_Paolo_2024.pdf]. More recent slides of the same talk, held in september 2024 @ JR Roma meetup are: (JS Roma 2024.pdf)[JS Roma 2024.pdf].

Although being a quite useless projects, as typical of mine, this program features some classic techniques that may be worth to know and to use elsewhere.

Anyway, enjoy,
Paolo

### Interpreter, compiler or what?

S.js can run in a browser (or it can be launched by node.js for what that matters, see below) and it can do the following:

- to get a source text (string) and to return a list of all tokens parsed from it;
- to get a token list and to compile it as a JavaScript program into a runtime object;
- to get the runtime object and execute it.

A runtime object `rt` contains an array `rt.code` where the object code, produced by the compiler, is stored: such a code is just a sequence of JavaScript functions that are executed by the runtime engine, the last step in the previous list.

The front-end and back-end of the compiler are collapsed in a single step: one could disentangle them so to make it possible to compile to real virtual machine (say to produce Web Assembly code) but the purpose here is to to everything within a single JavaScript version.

So, S.js is a compiler since it takes a source code and produces a corresponding object code for a runtime engine; it is an interpreter since the object code is not for a native processor but for a virtual one.

### How to use it?

The compiler code is contained in the [S.js](S.js) file that defines a bunch of functions: if you use node.js you need to modify the [S.js](S.js) file to make it a module (by exporting variables `sjs_scan`, `sjs_compile_block` and `sjs_run`) before including it into a node.js module to use it.

Next, to compile and execute a source text, use

```javascript
sjs_run(sjs_compile_block(sjs_scan(source_text)), debug);
```

(the `debug` flag is used by `sjs_run` to decide if log information on the browser console when executing any machine code instruction).

On a browser, just launch the [sjs.html](sjs.html) file in the distribution, that displays an old-fashioned VaX-like terminal in the browser with some easy to understand buttons on its top to load, run, debug (in the console), parse and compile a program with the compiler.

Use this [sjs.html](sjs.html) playground, with the browser console opened, to follow these notes and experiment with the compiler: the `Compile` button will compile the text in the playground text area and show in a separate window the machine language produced by the compiler. The `Debug` button runs the program by logging on the console each machine language instruction that is executed, along with the value of the stack and of this. Not an actual debugger but it helps.

Enjoy finding and fixing its bugs ;-)

P

## The implemented language

This project selects a subset of JavaScript, grosso modo corresponding to the earlier versions of the language, that provides all the basic control and data structures needed to write non trivial programs. Indeed, the compiler itself is written in this subset, thus it is able to compile itself. Actually, the subset of Javascript has been chosen by including only features needed to code the compiler.

The main implemented features are:

- implicit "use strict";
- block scoping and basic control structures: `do-while`, `for`, `if-else`, `while` whose corresponding instruction need to be a block (thus no `while (x > 0) -- x;`);
- the only allowed jump instruction is `return`;
- expressions implementation is fairly complete;
- function definitions, inside expressions, are actually closures, as expected;
- All built-in objects are available;

Functions can only be defined as the result of an expression, e.g.

```javascript
let f = function(x1,...,xn) {...};
```

There's no `function f(x1,...,xn) {...}` definition nor arrow function definition `(x1,...,xn)=>{...}` inside an expression.

### Accepted syntax

In this section I will formally describe the syntax the interpreter accepts. As meta-language I use the W3C variant of the classic EBNF metalanguage by Niklaus Wirth, see [Extensible Markup Language (XML) 1.0 (Fifth Edition), section 6](https://www.w3.org/TR/xml/#sec-notation).

#### Tokens of the language

```
/* S means a sequence of spacelike characters. */
S       ::= (#x20 | #x9 | #xD | #xA)+
Digit   ::= [0-9]
Dot     ::= "."
Quote   ::= #x27    /* ' */
DQuote  ::= #x22    /* " */
Number  ::= Digit+ | Digit+ Dot Digit*
String  ::= Quote [^']* Quote | DQuote [^"]* DQuote
Alpha   ::= [A-Z] | [a-z] | "$" | "_"
Alnum   ::= Alpha | Digit
Name    ::= Alpha Alnum*
```

Notice that S.js doesn't allow back-quote string and that numbers are only in decimal fixed point notation, not exponential one.

#### Expressions

JavaScript stems from C the use of expressions not only to produce values but also to alter the computational state somewhat. The S.js implementation of expression is quite complete even if it doesn't fullfil the complete JavaScript standard.

The syntax alone does not give information about execution priorities, see the table below. Recall that `S*` means an optional sequence of spacelike characters.

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

Operators with higher priority number are executed before one with lesser priority: as it is well nkown, priorities are, from the lowest to the highest, as follows

- "=", "+=", "-=" have lower priority than
- "||" that has lower priority than
- "&&" that has lower priority than
- "==", "!=", "<", ">", "<=", ">=", "in" that have lower priority than
- "+", "-" that have lower priority than
- "*", "/" that have lower priority than
- "**" that has lower priority than
- "new" that has lower priority than
- "-" (negation), "!", "++", "--", "typeof" that have lower priority than
- "." (membership), "[ ]" (object member), "( )" (function application)

#### Instructions

A S.js program is a text file that is interpreted as a block: that means that the compiler adds an initial "{" and final "}" token to the text. Then the following syntax is applied

```
Block       ::= S* "{" *S (Instruction *S)* "}" S*
Instruction ::= "do" Block "while" S* "(" Expression ")" S* ";"
              | "if" S* "(" Expression ")" Block ("else" Block)+
              | "for" S* "(" (S* "let" S* Name S* "=")+ S* Expression+ S* ";" Expression+ S* ";" Expression+ S* ")" Block
              | "for" S* "(" S* "let" S* Name S* "in" Expression ")" Block
              | "let" *S Name (*S "=" Expression)+ (*S "," *S Name (*S "=" Expression)+)* *S ";"
              | "return" Expression+ *S ";"
              | "throw" Expression ";"
              | "while" S* "(" Expression ")" Block
              | Expression ";"
              | S* ";"
```

Sorry but there are no `break` nor `continue`, use variables or objects attributes instead along with conditions in `while` or `for`.

### Builtin objects

S.js assumes to run in a browser and, as a such, it considers the `window` object as defined and uses it as initial outer environment (see below). Thus you have access inside S.js to all standard objects, such as `alert`, `Array`, `Math` etc.

Moreover, the S.js compiler recognizes a number, string, array or object as belonging to those classes. Thus, the snippet

```javascript
console.log("abc".includes("c"));
console.log([1,2,3].at(1));
console.log(123.456789.toPrecision(4));
```

will work as expected.

You can also use the `false`, `true`, `null`, `undefined` and `this` objects with the expected behavior (up to bugs;-).

## The compiler

**WARNING** from now on this document is still work in progress.

The S.js compiler is a single file containing a bunch of functions arranged in three groups:

- lexical analysis functions;
- syntactical analysis and code generating functions;
- virtual machine engine;

### The lexical analyzer

The lexical analyzer consists in a single function `sjs_scan` to be used as

```javascript
let token_list = sjs_scan(text);
```

Text is any string containing the text of a JavaScript program: `sjs_scan` always encloses this text between braces to make a block out of it and then tokenizes it into a token list returned as value. The token list is an array of objects of the form

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

The first `let` token is a keyword (its type is not `name` nor `string` but the keyword itself) while the fourth one is a string. Keywords are intercepted by the`sjs_scan` function so that they cannot be used as names. Therefore, if you crazily type `let let = "let";` and run, S.js will stop the execution with a *Error: 'name' expected at 1:4* error message.

### The runtime object

Once the source text has been tokenized into a token list, the latter can be parsed and compiled by the `sjs_compile_block` function, that works as:

```javascript
let rt = sjs_compile_block(token_list);
```

If the compilation succeeds, a "runtime object" is returned that can be run by the virtual machine engine (otherwise an exception with an error message is thrown). It is important to understand this object before describing both the compilation process and the execution engine.

#### The code

A runtime object `rt` returned by `sjs_compile_block` contains at least the following fields, needed during compilation (actually, `rt` contains more stuff, e.g. all opcode definitions, thus the functions that implement machine code instructions, such as `PUSH` etc. A runtime object is created via the `sjs_runtime`):

```javascript
rt = {
    code: // an array that contains the compiled code
    ic: // (instruction counter) a number used to scan array code
    ...
}
```

Each instruction, thus an element of `rt.code`, is an object of the form

```javascript
rt.code[ic] = {
    i:  // function executed when the instruction is executed 
    l:  // line of the source file corresponding to the instruction
    c:  // column of the source file corresponding to the instruction
}
```

Therefore, `l` and `c` keys are used for debug purposes while the actual "routine" called when an instruction is executed is `rt.code[ic].i`. Notice that most virtual machine uses *opcodes* thus numerical codes that correspond to instructions: the virtual machine engine performs this translation (or a just in time compilation). S.js instead has no opcodes at all but its opcodes are genuine JavaScript functions executed by the virtual machine engine.

At runtime, the execution of `rt` is a trivial matter: the array `rt.code` is scanned and its elements executed.

```javascript
rt.ic = 0;
while (rt.ic < rt.code.length) {
    rt.code[rt.ic++](rt).i;
}
```

(the actual code is more complicated also because `rt.code` elements contain debug information but in these explanations I'll ignore that).

Notice that the `rt.ic` index is advanced before executing the function supposed to be the value of `rt.code[rt.ic].i`. Each `rt.code[i].i` is a JavaScript function implementing a single instruction of the S.js virtual machine. This function takes as parameter the current (at runtime) runtime object `rt` and it uses a stack to hold temporary values and to return values to subsequent functions. Sometimes, such a function also reads the next `rt.code` element, that's why we need to store in the variable `code.rt` the index of the next instruction to execute.

For example, the `PUSH` instruction is implemented by a `PUSH(rt)` function that reads the object following it in `rt.code`, pointed by `rt.ic` which is next increased, and pushes it on the stack advancing `rt.ic` by 1, so to prevent the execution loop to try to execute a number as if it is a function; also, the `JP()` function implementing instruction `JP` performs an unconditioned jump to another instruction I, which is identified by the offset between the index in `rt.code` of I and of the current one and increases `ic`, too. It is easier to show the code indeed:

```javascript
function JP(rt) {
    rt.ic += rt.code[rt.ic].i;
};
```

So, suppose `rt.code` to be:

```
rt.code = [PUSH, "!", JP, -3]
```

This is a crazy infinite loop that pushes `"!"` on the stack and repeats from the first index the execution of code: indeed the `JP` instruction reads the number located after it in `rt.code` and adds it to `rt.ic`: in this case, when `JP` is executed, `rt.ic == 3` (since `rt.ic` points to the element following the instruction under execution) so that the effect of `JP` is to set `rt.ic = 0`, making the execution loop to repeat from the first element of `rt.code`.

For a detailed discussion of the instruction set of the S.js virtual machine, see the last chapter: here, let me describe the data it uses during its execution at runtime.

#### Runtime objects inside `rt`

To execute a runtime object `rt` use the S.js `sjs_run(rt, debug)` function, that gets two parameters, a runtime object to run and a debug flag: if the latter is true, then debug information will pollute the console of the browser. Before executing the `rt.code` object code, `sjs_run` adds the following attributes to `rt`:

```javascript
rt.env = {$_outer_$: window};   // scope environment
rt.$_this_$ = undefined;        // this object
rt.stack = [];                  // A stack used to store temporary values
rt.dump = [];                   // Another stack used to store temporary values
rt.debug = debug;               // if true debug information will be logged
```

The `rt.env` object is the *environment*, thus it contains all variables defined at the current scope. Therefore, an instruction

```javascript
let x = e
```

is equivalent to `rt.env.x = e`. A special value `rt.env.$_outer_$` points to the environment of the outer scope.

Indeed, each time a new scope is introduced (for example when evaluating a function or when executing a block of instructions) the `PUSHENV` machine code instruction is executed at runtime, that performs the operation `rt.env = {$_outer_$: rt.env};` defining a new environment as the current one and the current one as outer, so that new variable definitions will be stored in the new environment. When the current environment is no more needed, the `POPENV` instruction is executed, that restores `rt.env = rt.env.$_outer_$`. All variables introduced in the discarded environment are discarded, too lefting the hard job to the JS engine executing S.js.

When the interpreter looks for a variable `x`, which is done at runtime by the `REF x` instruction, the key `x` is looked for in `rt.env`: if not found, the search continues in `rt.env.$_outer_$` and so on until the outermost scope is reached (whose `$_outer_$` is `null`).

The `rt.$_this_$` object is the `this` of the runtime interpreter: the name is cumbersome not to be confused with the `this` of the JavaScript engine executing S.js.

The `rt.dump` array is used to store old environments that need to be restored after a while, for example when invoking a function.

The `rt.stack` array is used to contain parameters of machine language instructions such as `ADD` that pops the two topmost elements on the stack, sums them and pushes the result on the stack.

#### Stack, environment and references

During runtime execution, the compiled code uses the stack to perform operations among data: such operations are implemented as machine language instructions that retrieve their parameters from the stack and leave on it the result of their computation.

For example consider the expression

```javascript
1 + 2 * 3;
```

That should be evaluated by firstly performing `2 * 3`, resulting in `6` that next should be added to `1` to get the final result `7`. This is compiled as

```
    PUSH 1      // Push 1 on the stack
    PUSH 2      // Push 2 on the stack
    PUSH 3      // Push 3 on the stack
    MUL         // Pop two numbers, get 3 and 2, and push their product 6
    ADD         // Pop two numbers, get 6 and 1, and push their sum 7
```

Usually variables are involved in such computations: consider for example

```javascript
1 + 2 * x;
```

When computing this expression (thus when running the machine code that compiles it) we need to use the value of `x` at the moment of evaluation. The not so efficient choice of S.js is to retrieve the value from the environment when needed. Consider the compiled code corresponding to the above expression:

```
    PUSH 1      // Push 1 on the stack
    PUSH 2      // Push 2 on the stack
    REF x       // Retrieve the value of x and push it on the stack
    MUL         // Pop two numbers and push their product
    ADD         // Pop two numbers and push their sum
```

The `REF` instruction gets its parameter not from the stack but from the next `rt.code` element, just as `PUSH` and `JP`. This element should be a string and this string is checked to be a key in the current environment object `rt.env`. It that is the case, then the value `rt.env.x` is pushed on the stack. Else the search is performed in the `env.$_outer_$` environment and so on until the key is found in some outer environment or not found in any of them (in which case an error is raised).

But now consider

```javascript
x = 1 + 2 * x;
```

In this case the expression has a side effect: it changes the value of `x` to a new value which is the result of the expression `1 + 2 * x` (using the old value of `x`). Therefore, the `x` on the RHS refers to the value of the variable `x`, while the `x` on the LHS refers to the "address" of the variable `x`. Since one cannot take the address of a variable in JavaScript, S.js uses a device to deal with references to variables.
Notice that we could, with some pain, deduce that the `x` on the right is different from the `x` on the left and compile them differently.

The solution adopted by S.js is simpler, instead: the `REF x` instruction, once it finds a key `x` in the current environment (or in some outer environment) pushes on the stack an object

```javascript
{
    $_ref_$: env,
    $_at_$: "x"
}
```

where `env` is the environment in which the variable has been found and `x` the name of the variable.

Suppose such a reference `r` is on top of the stack: if an instruction needs just the value of the variable, then it pops the reference and retrieves the valu as `r.$_ref_$[r.$_at_$]`. This is the most common case.

Consider instead the JavaScript assignment

```javascript
let x = 0;
```

This is translated as

```
    REF x
    PUSH 0
    SET
```

The `SET` instruction pops a value `0` and a reference, `r = {$_ref_$: rt.env, $_at_$: "x"}` in this case, next performs the assignment `r.$_ref_$[r.$_at_$] = 0`, thus `rt.env["x"] = 0` in this case.

When a value is popped from the stack, it is considered a reference if it is an object and it has the `$_ref_$` key, else it is considered as a value, such as the ones pushed by `PUSH` on the stack.

Notice that an object in the LHS of an assignment can be dereferentiated, thus one can refers to a key of it. Consider, for example, the following snippet:

```javascript
let obj = {a:[1,2,3], b:{a:1, b:2, c:3}};
obj.a[0] = obj.b.a;
console.log(obj.a[0]);
```

Its execution will print `1` in the console, indeed try to execute it with S.js and you'll get this result. Now look at the compiled code corresponding to the second instruction, the assignment (we use symbolic labels for the addresses of each instructions):

```
i0: REF obj
i1: PUSH a
i2: DEREF
i3: PUSHTHIS
i4: PUSH 0
i5: POPTHIS
i6: DEREF
i7: REF obj
i8: PUSH b
i9: DEREF
iA: PUSH a
iB: DEREF
iC: SET
```

The first two instructions push `{$_ref_$: env, $_at_$: "obj"}` and `"a"` on the stack. Next the `DEREF` instruction is executed, that does the following:

1. pop a value `v`, the string `"a"` in this case;
2. pop an object `r` from the stack;
3. if `r` is a reference `{$_ref_$: e, $_at_$: "i"}` then a new reference `{$_ref_$: r.$_ref_$[r.$_at_$], $_at_$: v}` is pushed on the stack;
4. else, if `r` is an object then a new reference `{$_ref_$: r, $_at_$: v}` is pushed on the stack;
5. else, `undefined` is pushed on the stack.

In each case, `rt.$_this_$` thus the `this` object is settled accordingly (to `r.$_ref_$` in cases 3 or 4, to `undefined` in case 5). Thus dereferencing creates an object that refers to a part of another object.

Coming back to the previous example, when the `DEREF` at `i2` is executed, it leaves `{$_ref_$: rt.env.obj, $_at_$: "a"}` on the stack. Next `$_this_$` is saved (because inside brackets new objects may appear) and `0` is pushed on the stack, so that the `DEREF` at `i6` will pop the `0` and `{$_ref_$: rt.env.obj, $_at_$: "a"}`, pushing a new reference `{$_ref_$: rt.env.obj.a, $_at_$: 0}` on the stack.

Analogously, instructions from `i7` to `iB` leave on the stack the reference `{$_ref_$: rt.env.obj.b, $_at_$: "a"}`.

Finally, the `SET` instruction pops a value, thus `rt.env.obj.b.a` and the reference `{$_ref_$: rt.env.obj.a, $_at_$: 0}`, assigning to the latter the former value, which amounts to execute `rt.env.obj.a[0] = rt.env.b.a`.

This may appear to be cumbersome (it is!) but it is a fairly straightforward way to handle assignments.

### Code generation

Let us come back to the `sjs_compile_block` function that gets a token list and returns a runtime environment, and see some of its internals.

#### Compiling instructions

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

Each of the subfunctions deals with the specific syntax of the instruction it ought to compile. This is repeated for all instructions parsed from the token list. The actual JavaScript code is a bit more complicated but it is essentially as the pseudocode above.

To compile JavaScript instructions into machine code instructions, the latter need to be available by the compiler: that is done by inserting them as keys into the runtime object. Indeed, the code compiled in `rt.code` it is not an opcode but directly a JavaScript object, typically the function implementing the machine code instruction.

The compilation of the single instructions is straightforward and uses machine language instructions. At runtime, the runtime object `rt` contains also two stacks `rt.stack` and `rt.dump` (a hommage to the classical Peter Landin SECD machine) and an object `rt.env` that contains all variable associations at current scope. More on that later

Let me describe briefly as each JavaScript instruction is compiled.

##### Compiling `do-while`

The instruction

```javascript
do {I1; ...; In} while (E);
```

is compiled as follows (I show machine language code in a symbolic way, an instruction per line: instructions preceded by a `label:` are marked by that label, so that a jump instruction can resume execution from that point)

```
i0: compiled code for I1
    ...
    compiled code for In
    compiled code for E
    JPNZ i0
```

Thus the sequence of instructions is compiled, then the expression is compiled: the latter leaves in the stack its value, and the `JPNZ` function pops it and, if it is non zero (or non `null`, `undefined`, `false`) perform a jump to location `i0`, else execution continues (actually, in the `code` element following `JPNZ` there's not `i0` but the offset between that element and `i0`).

##### Compiling `for (E1; E2; E3) {...}`

The instruction

```javascript
for (E1; E2; E3) {I1; ... In;}
```

is compiled as follows:

```
    PUSHENV
    compiled code for E1
    DROP
i0: compiled code for E2
    JPZ i1
    compiled code for I1
    ...
    compiled code for In
    compiled code for E3
    DROP
    JP i0
i1: POPENV
```

First of all a new environment is defined, to host variables defined inside the loop. Next, expression E1 is compiled, followed by the DROP instruction: the latter, at runtime, just pop the top of the stack discarding its value. Indeed, since an expression leaves always a result on the stack (perhaps `undefined`) and since we don't need the value of E1 which is used only for its side effect, we drop that value.

Next, the address of instruction E2 is marked as i0 and E2 is compiled, followed by a `JPZ i1` instruction, that pops the top of the stack (the result of the evaluation of E2) and, if zero, perform a jump, else continues the execution. So, if the E2 condition is false, the block of instructions is skipped and the execution continues after the for-loop.

Else the I1, ..., In instructions are executed and also the E3 expression, whose value is discarded since, as for E1, E3 is used only for its side effect. The`JP i0` repeats the loop from the evaluation of condition E2.

Finally, at `i1`, the loop environment is discarded.

##### Compiling `for (let V = E1; E2; E3) {...}`

The following variant of the for-loop

```javascript
for (let V = E1; E2; E3) {I1; ... In;}
```

is essentially the same, but for the fact that a new variable is inserted into the loop environment by means of the `let` statement:

```
    PUSHENV
    REF V
    compiled code for E1
    LET
i0: compiled code for E2
    JPZ i1
    compiled code for I1
    ...
    compiled code for In
    compiled code for E3
    DROP
    JP i0
i1: POPENV
```

Notice that, since the `LET` instruction does not leave any value on the stack, no `DROP` is needed after E1. Indeed, `LET` pops a value `v` and a string  `s` and sets `rt.env[s] = v`.

##### Compiling `for (let V in E) {...}`



##### Compiling `if-else`

This is also straightforward: the instruction

```javascript
if (E) {
    I1; ... ; In
} else {
    J1; ...; Jm
}
```

is compiled as

```
    PUSHENV
    compiled code for E
    JPZ i1
    compiled code for I1
    ...
    compiled code for In
    JP i2
i1: compiled code for J1
    ...
    compiled code for Jn
i2: POPENV
    
```

Of course, if the `else` part is omitted, then the compiled code simplifies to

```
    PUSHENV
    compiled code for E
    JPZ i2
    compiled code for I1
    ...
    compiled code for In
i2: POPENV
```

##### `let`

I have already discussed how

```javascript
let V1 = E1, ..., Vn = En;
```

is implemented, since the `LET` machine code instruction does the all job, by popping a value, a reference and assigning the value to the reference:

```
    REF V1
    compiled code for E1
    LET
    ...
    REF Vn
    compiled code for En
    LET
```

##### `return`

To discuss `return` I have to discuss how functions are called in the first place. A function can only be defined via the `function(x1,...,xn) {...}` syntax inside an expression. When such a syntax is parsed, the code needed to allocate at runtime the closure corresponding to the function is compiled.



##### `throw`

##### `while`

#### Compiling expressions


## The virtual machine


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

    - LET pop s, pop v, creates a new variable with name s and value v.
    - JP parse n, set ic = n.
    - JPZ pop v: if v in [0, false, null, undefined] then perform JP, else parse n
    - JPNZ pop v: if v not in [0, false, null, undefined] then perform JP, else parse n

    - ADD pop n1, pop n2, push n2 + n1
    - SUB pop n1, pop n2, push n2 - n1
    - MUL pop n1, pop n2, push n2 * n1
    - DIV pop n1, pop n2, push n2 * n1
    - EQ pop v1, pop v2, push v2 == v1
    - NE pop v1, pop v2, push v2 != v1
    - LT pop v1, pop v2, push v2 < v1
    - GT pop v1, pop v2, push v2 > v1
    - LE pop v1, pop v2, push v2 <= v1
    - GE pop v1, pop v2, push v2 >= v1
