# SJS - Self JavaScript

A toy JavaScript self-implementation.

## Implemented features

### Accepted syntax

    program     = {statement}
    statement   = expression ";"
                | "break" ";"
                | "continue" ";"
                | "do" "{" program "}" "while" "(" expression ")" ";"
                | "for" "(" [initialize] ";" [expression] ";" [initialize] ")" "{" program "}"
                | "for" "(" "let" name "in" expression ")" "{" program "}"
                | "if" "(" expression ")" "{" program "}" ["else" "{" program "}"]
                | "let" name ["=" expression] {"," name ["=" expression]} ";"
                | "return" [expression] ";"
                | "throw" expression ";"
                | "try" "{" program "}" catch "(" name ")" "{" program "}"
                | "while" "(" expression ")" "{" program "}"
    initialize  = "let" name "=" expression
                | expression
    expression  = {prefix_op} value
                | expression binary_op expression
    value       = constant
                | object
                | variable
                | "(" expression ")"
    constant    = digit{digit}["."{digit}]
                | "null"
                | "undefined"
                | "true"
                | "false"
                | "'"{character}"'"
                | '"'{character}'"'
    object      = "{" name ":" expression {"," name ":" expression} "}"
                | "[" [expression {"," expression}] "]"
                | "function" "(" [name {"," name}] ")" "{" program "}"
                | "new" value
    variable    = name
                | variable "." name
                | variable "[" expression "]"
                | variable "(" [expression {"," expression}] ")"
    prefix_op   = "-" | "++" | "--" | "!"
    binary_op   = "+" | "-" | "*" | "/" | "==" | "<" | ">" | "!=" | ">=" | "<=" | "&&" | "||" | "=" | "+=" | "-="

I tipi di dato predefiniti sono:

    - null
    - undefined
    - numeri
    - stringhe

## The virtual machine

### The virtual memory of the machine

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
