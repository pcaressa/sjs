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

### Variables

The runtime environment object contains an attribute `env` which is used to store symbols defined in the current scope. Each symbol defined by a `let v = x` is stored as an object `env.v = {value: x, type: t}`.

When a variable's name appears inside an expression, it is compiled as

    REF v

At runtime, the `REF` instruction parses the name `v` and looks for it in the environment: then the object `{ref: r, at: []}` is pushed on the stack, where `r` is a reference to the `{value: x, type: t}` object in `env.v`. If the value of the object is needed, then `r.value` is pushed on the stack, else if the reference is needed, the reference is used.

For example consider:

    x[1] = x[0];

This is compiled as

    REF "x"
    PUSH 1
    DEREF
    REF "x"
    PUSH 0
    DEREF
    SET

The `REF` instruction looks for the attribute `x` inside `rt.env` and push the reference `{ref: {value: v, type: t}, at: []}` on the stack. Next `PUSH` pushes the object `{ref: {value: 1, type: "number"}, at: []}` and the `DEREF` instruction modifies the reference to `{ref: {value: v, type: t}, at: [1]}`. The second sequence `REF-PUSH-DEREF` does the same leaving `{ref: {value: v, type: t}, at: [0]}` on the stack.

Finally the `SET` operator pops two references and assigns the value of the topmost to the one below, in this case the final result will be that the attribute `1` of the object `v` will be set to the value of `v[0]`.

This device is not efficient, since operators that just need the value of a variable will always use the `value` attribute discarding the rest, but it is simple.


env = {a: {value: [1,2,3], type: "object"}}

    REF a
    PUSH 1
    DEREF
    REF a
    PUSH 0
    DEREF
    SET

REF a   s: (r)  dove r = {ref:{value: [1,2,3], type: "object"}, at:[]}
PUSH 1  s: (r {ref:{value:1, type: "number"}, at:[]}
DEREF   s: (s)  dove s = {ref:{value: [1,2,3], type: "object"}, at:[1]})
REF a   s: (s r)  dove r = {ref:{value: [1,2,3], type: "object"}, at:[]}
PUSH 1  s: (s r {ref:{value:1, type: "number"}, at:[]}
DEREF   s: (s t)  dove s = {ref:{value: [1,2,3], type: "object"}, at:[0]})
SET     pone s.ref.value[s.at[0]] = t.ref.value[t.at[0]]


### References

When a variable is mentioned in a JavaScript expression, an object is pushed on the stack of the form:

    { ref: v, at: s }

where `v` is the 

### Objects

The SJS runtime deals only with objects: that means that even numbers are created as objects. However the standard internals of the JS environment are not implemented in detail.

According to SJS an object contains always:

- A "value" attribute which is the JS value of the object.
- A "type" attribute which is a string denoting the type of the object: the built-in function "typeof" returns this value.
- A "is_builtin" attribute which true if the object is a builtin function, else undefined.
- A "is_user" attribute which true if the object is a user function, else undefined.

When a variable is defined, as in

    let x = 1;

an attribute with name x and value {"value": 1, "type": "number"} is added to the rt.env object.

When a variable is referenced in an expression, the object corresponding to it is pushed on the stack: if only the value of the variable is needed, that is retrieved, else if a reference to the variable is needed, then the complete object is considered.

For example,

    x = x + 1;

is compiled as

    VAR "x"
    VAR "x"
    PUSH 1
    ADD
    SET

The VAR instruction parse a string, looks for it in the current environment and, if it founds it, push the {"value", "type"} object on the stack. When the ADD instruction is executed, is pops the {"value": 1, "type": "number"} object just pushed, next pops {"value": v, "type":t} which is the object corresponding to "x". Then checks against types and performs the sum of the two values resulting in an object {"value": x + 1, "type": "number"}.

When SET is executed, it pops this object and the one below it, thus a reference to the {"value", "type"} definition of "x". But this time SET assigns to the "value?" attribute of this object the new value. Therefore, the variable's value is changed.

Consider also

    x[1] = x[0];

This is compiled as

    VAR "x"
    PUSH 1
    MEMBER
    VAR "x"
    PUSH 0
    MEMBER
    SET

The MEMBER instruction creates a new object {at: 1, of: {...}} where the value of the "of" attribute is the object associated to the variable "x", thus {"value": value of x, "type": type of x}. When a function such as ADD has a member object among its operands, it uses the "at" and "of" information to retrieve the value of the object, while a function such as SET uses those information to retrieve a reference to the value to be changed.

For example

    x[0].y = 1;

is compiled as

    VAR "x"
    PUSH 0
    MEMBER
    PUSH "y"
    MEMBER
    PUSH 1
    SET
    
The SET instruction pops the value 1 to assign and the object to be assigned, which is {at: "y", of: {at: 0, of: {value:x, type:"object"}}}.


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
