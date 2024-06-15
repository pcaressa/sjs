# SJS - Self JavaScript

A toy JavaScript self-implementation.

## Accepted syntax

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
