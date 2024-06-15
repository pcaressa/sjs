/*
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
*/

/** sjs_scan(text) accetta una stringa e ne scandisce tutti i token
    JS, tornandoli in una lista i cui elementi sono oggetti con i
    seguenti attributi: {"s": s, "t": t} dove s e' la rappresentazione
    stringa del token, t il tipo di token, r la riga e c la colonna
    dove il token e' ubicato nella stringa. */
let sjs_scan = function(text)
{
    let ALPHA = "QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm$_";
    let ALNUM = "QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm$_1234567890";
    let DIGIT = "1234567890";
    
    let toklist = [{s: "{", t: "delimiter", l:0, c:0}]; // list of all scanned tokens
    let line = 1;       // current line number
    let i_line = 1;     // Index of 1st character of the current line

    /*  skip_until(i, delims) = least j>i such that text[j] is in delims.
        If no such j exists then return text.length. */
    let skip_until = function(i, delims) {
        for (++ i; i < text.length && !delims.includes(text[i]); ++ i) {
            if (text[i] == "\n") {
                ++ line;
                i_line = i + 1;
            }
        }
        return i;
    }
    /*  skip_while(i, delims) = least j>i such that text[j] is not
        in delims. If no such j exists then return text.length. */
    let skip_while = function(i, delims) {
        for (++ i; i < text.length && delims.includes(text[i]); ++ i) {
            if (text[i] == "\n") {
                ++ line;
                i_line = i + 1;
            }
        }
        return i;
    }
    let i = 0;
    while (i < text.length) {
        let c = text[i], c1 = text[i+1];
        if (c == "\n") {    // Skip newlines
            ++ line;
            i_line = ++ i;
        } else
        if (" \r\t\f\v".includes(c)) {  // Skip spaces
            ++ i;
        } else
        if (c == "/" && c1 == "/") {    // Skip line comments
            i = skip_until(i, "\n");
            ++ line;
            i_line = ++ i;
        } else
        if (c == "/" && c1 == "*") {    // Skip comments
            let i0 = i + 2;
            do {
                i = 1 + skip_until(i, "*");
            } while (text[i] != "/");
            ++ i;
        } else            
        if (ALPHA.includes(c)) {        // Names
            tok = c;
            let i0 = i;
            i = skip_while(i0, ALNUM);
            toklist.push({s: text.slice(i0, i), t: "name", l: line, c: i0 - i_line});
        } else
        if (DIGIT.includes(c)) {        // Numbers
            let i0 = i;
            i = skip_while(i0, DIGIT);
            if (text[i] == ".") {
                i = skip_while(i + 1, DIGIT);
            }
            toklist.push({s: Number(text.slice(i0, i)), t: "number", l: line, c: i0 - i_line});
        } else
        if (c == "'" || c == '"') {     // Strings
            i0 = ++ i;
            i = skip_until(i, c);
            toklist.push({s: text.slice(i0, i), t: "string", l: line, c: i0 - i_line});
            ++ i;
        } else
        if (c == '<' && c1 == '=' || c == '>' && c1 == '=' || c == '=' && c1 == '=' || c == '!' && c1 == '=' || c == '+' && c1 == '=' || c == '-' && c1 == '=' || c == '&' && c1 == '&' || c == '|' && c1 == '|' || c == '+' && c1 == '+' || c == '-' && c1 == '-') {
            toklist.push({s: c + c1, t: "delimiter", l: line, c: i - i_line});
            i += 2;
        } else
        if ("-+*/%<>=!.,;:()[]{}".includes(c)) {
            toklist.push({s: c, t: "delimiter", l: line, c: i - i_line});
            ++ i;
        } else {
            alert(line + ":" + (i - i_line) + ": Syntax error: " + c);
            return null;
        }
    }
    toklist.push({s: "}", t: "delimiter", l:0, c:0});
    return toklist;
}

// sjs_compile_error(cond, msg) raises an error with message msg if cond is true,
// else it does nothing.
let sjs_compile_error = function(cond, msg, token)
{
    if (cond) {
        alert(token.l + ":" + token.c + ": " + msg);
        throw null;
    }
};

// Add opcode functions to object rt
let sjs_stdlib = function(rt) {

    // ADD() pop y, pop x, push x + y
    rt.ADD = function() {
        let y = rt.stack.pop();
        let x = rt.stack.pop();
        stack.push(x + y);
    };
    
    /** ARRADD() pop v, pop a, { a[a.length] = v; ++ a.length; } push a */
    rt.ARRADD = function() {
    };
    
    /** CLOSURE() parse x, parse y, creates a closure with parameters the
        elements of x (a list of strings), body the code list y and environment
        the environment active when CLOSURE() is executed. */
    rt.CLOSURE = function() {
    };

    // DUPJPZ() pop x, if x == 0 push 0 and perform JP, else perform NOP.
    rt.JPZ = function() {
    };

    // JP() get a number and set this.ic to it.
    rt.JP = function() { rt.ic = rt.code[rt.ic]; };
    
    // JPZ() pop x, if x == 0 perform JP, else perform NOP.
    rt.JPZ = function() {
    };
    
    /* LET() pop v, pop s and creates a variable with name s and value v.
        Notice that a variable's value is always an object, with attribute "value"
        the actual value. In this way we can refer to the variable when assigning
        a value to it. */
    rt.LET = function() {
        let v = rt.stack.pop();
        let s = rt.stack.pop();
        rt.env.s = {value: v};
    };
    
    // NOP() does nothing at all
    rt.NOP = function() {};

    // OBJADD() pop v, pop n, pop a, { a[n] = v; } push a
    rt.OBJADD = function() {
    };
    
    // PUSH() parse v, push v
    rt.PUSH = function() {
        rt.stack.push(rt.code[rt.ic]);
        ++ rt.ic;
    };
    
    // SET() pop x, pop v, { v = x; }, push x
    rt.SET = function() {
        let x = rt.stack.pop();
        let v = rt.stack.pop();
        sjs_rt_error(v.value == undefined, "RT Error: Invalid LHS in assignment!");
        stack.push(v.value = x);
    };
    
    /* VAR() parse s, look for variable s in the environment, push on the
        stack the corresponding value if any or undefined. */
    rt.VAR = function() {
        let s = rt.code[rt.ic];
        let e = rt.env;
        while (e != null) {
            if (e.s != undefined) {
                rt.stack.push(e.s.value);
                return;
            }
            e = e._prev;
        }
        rt.stack.push(undefined);
    };

    return rt;
};

/** sjs_compile_array(tl, rt, endchr) compiles a list of expressions into
    an array which push itself on the stack at run time. It can be used
    with lists [x1,...,xn] or (x1,...,xn), the endchr is the one checked
    against the end of the list. */
let sjs_compile_array = function(tl, rt, endchr)
{
    // An empty array is created and elements are added to it
    // [v1, ..., vk] is implemented as [] v1 ARRADD ... vk ARRADD
    rt.code.push(rt.PUSH);  // PUSH parse a value and push it on the stack
    rt.code.push([]);
    while (tl[0].s != endchr) {
        token = sjs_compile_expression(tl, rt);
        rt.code.push(rt.ARRADD);    // a v ARRADD -> a[a.length] = v and a on the stack
        // Token should be ',' or ']'
        if (token.s == ",") {
            token = tl.shift();
        } else {
            sjs_compile_error(token.s != endchr, "'" + endchr + "' expected", token);
        }
    }
};

/** sjs_compile_operator(opt, tl, rt) compile the operator. */

/** sjs_compile_expression(tl, rt) compiles an expression into rt.code
    parsing it from the token list tl, which is consumed doing so.
    The first token following the expression is returned as value.
    To implement operator precedence the function uses a classic bottom-up
    technique: each operator has a priority number associated. When an
    operator is parsed, if the operator on the stack had higher priority
    then the latter is compiled and the former pushed on the stack; else
    the former is pushed on the stack. When the expression has completely
    been parsed, the stack is unwinded by compiling each element until
    it is empty.
*/
let sjs_compile_expression = function(tl, rt)
{
    console.log("> sjs_compile_expression")

    let stack = []; // stack where operators are pushed before being compiled
    /* compile_stack(opt) compile all operators in the stack with
        priority > the priority of opt. */
    let PRIORITIES = {
        "fake": -1, // fake operator, needed to make the while condition in compile_stack always true
        "=": 0, "+=": 0, "-=": 0, "||": 10, "&&": 15, "==": 20, "!=": 20,
        "<": 21, ">": 21, "<=": 21, ">=": 21, "+": 30, "-": 30, "*": 40,
        "/": 40, "NEG": 50, "!": 50, "++": 50, "--": 50, "new": 60, ".": 70
    };
    let OPERATORS = {
        "=": rt.SET, "+=": rt.SETADD, "-=": rt.SETSUB, "==": rt.EQ, "!=": rt.NE,
        "<": rt.LT, ">": rt.GT, "<=": rt.LE, ">=": rt.GE, "+": rt.ADD, "-": rt.SUB,
        "*": rt.MUL, "/": rt.DIV, ".": rt.MEMBER
    };
    let compile_stack = function(opt) {
        while (stack.length > 0 && PRIORITIES[stack[stack.length-1]] >= PRIORITIES[opt]) {
            let opt_to_dump = stack.pop();
            if (opt_to_dump == "&&" || opt_to_dump == "||") {
                // pop from cstack where to write the current code position.
                rt.code[cstack.pop()] = rt.code.length;
            } else {
                rt.code.push(OPERATORS[opt_to_dump]);
        }}
    };
    
    let cstack = [];    // Stack used for forward reference in && and ||
    let token;  // last token parsed from tl inside the do{...} loop
    let again;  // do{...} loop iteration condition, defined inside the loop
    do {
        // Prefix operator or operand expected
        token = tl.shift();
        
        // Deal with possible prefixes
        // A minus token means a negation
        if (token.s == "-") { token.s = "NEG"; }
        
        /////////////// TODO!!!!!!!!!!!!!!!!!!!!!!
        
        // Deal with possible operands
        // Constants, variables and objects are compiled immediately
        
        console.log("token = " + token.s + " [" + token.t + "]")
        
        if (token.t == "number" || token.t == "string") {
            rt.code.push(rt.PUSH);
            rt.code.push(token.s);
        } else
        if (token.s == "true" || token.s == "false") {
            rt.code.push(rt.PUSH);
            rt.code.push(token.s == "true");
        } else
        if (token.s == "null" || token.s == "undefined") {
            rt.code.push(rt.PUSH);
            rt.code.push(null);
        } else
        if (token.s == "undefined") {
            rt.code.push(rt.PUSH);
            rt.code.push(undefined);
        } else
        if (token.t == "name") {
            rt.code.push(rt.VAR);
            rt.code.push(token.s);
        } else
        if (token.s == "{") {   // Object
            // An empty object is created and attributes are added to it
            // {n1:v1, ..., nk:vk} is implemented as {} n1 v1 OBJADD ... nk vk OBJADD
            rt.code.push(rt.PUSH);
            rt.code.push({});
            token = tl.shift(); // name or '}'
            while (token.s != "}") {
                sjs_compile_error(token.t != "name", "'}' or 'name:value' expected", token);                
                rt.code.push(rt.PUSH);
                rt.code.push(token.s);
                token = sjs_compile_expression(tl, rt);
                rt.code.push(rt.OBJADD);    // obj name value OBJADD -> obj[name] = value and obj on the stack
                // Token should be ',' or '}'
                if (token.s == ",") {
                    token = tl.shift();
                } else {
                    sjs_compile_error(token.s != "}", "'}' expected", token);
                }
            }
        } else
        if (token.s == "[") {
            sjs_compile_array(tl, rt, "]");
        } else
        if (token.s == "function") {    // Function
            /*  A function(params) { body} is compiled as a sequence of
                three values: [CLOSURE(), params, body]. The runtime CLOSURE
                routine allocates the closure by defining its environmente as
                the runtime environment at the time when CLOSURE is executed.
                The result is then put on the stack. */
            // Parameters.
            token = tl.shift();
            sjs_compile_error(token.s != "(", "'(' expected", token);
            let parameters = [];
            token = tl.shift();
            while (token.s != ")") {
                sjs_compile_error(token.t != "name", "name expected", token);
                parameters.push(token.s);
                token = tl.shift();
                if (token.s == ",") {
                    token = tl.shift();
                }
            }
            // Function's body
            rt.code.push(rt.CLOSURE);
            rt.code.push(parameters);
            sjs_compile_error(tl[0].s != "{", "'{' expected", token);
            rt.code.push(sjs_compile(tl).code);
        } else
        if (token.s == "(") {    // Subexpression
            rt = sjs_compile_expression(tl, rt);
            sjs_compile_error(tl[0].s != ")", "')' expected", tl[0]);
        } else {
            sjs_compile_error(true, "Syntax error: " + token.s, token);
        }
        // Parse a token: an operator may follow, else the expression
        // is ended. Notice that an actual parameter list (x1,...,xn)
        // is considered to be an operator.
        token = tl.shift();
        
        console.log("token = " + token.s + " [" + token.t + "]")
        
        // In this context, '(', '[' and '.' have different meaning,
        // they are binary operators.
        while (token.s == "(") {
            // Compile a list of expressions
            sjs_compile_array(tl, rt, ")");
            rt.code.push(rt.APPLY);
        }
        while (token.s == "[") {
            // Subscript operator
            
            sjs_compile_error(true, "TODO");
            
        }
        again = token.s in PRIORITIES;  // operator so we need an operand, hence iterate
        if (again) {
            // Compile elements in the stack with higher priorities
            compile_stack(token.s);
            stack.push(token.s);
            /* Shortcuts operator need more work: they also compile a JUMP
                and save the index of the element of code containing the
                jump address, to be written after the second operand will
                be compiled. */
            if (token.s == "&&" || token.s == "||") {
                rt.code.push(token.s == "&&" && rt.DUPJPZ || rt.DUPJNZ);
                /* The index where to jump shall be written at
                    rt.code[rt.code.length] after the second operand will
                    be compiled. Now save the position where to store it
                    in the "control stack". */
                cstack.push(rt.code.length);
                rt.code.push(null); // placeholder
            }
        }
    } while (again);

    // If there are operators on the stack, compile them.
    compile_stack("fake");

    console.log("< sjs_compile_expression " + stack.length + ":" + stack)
    return token;
};

/** sjs_compile(tl) accetta una token list, cioè una lista prodotta da sjs_scan,
    e compila il testo Javascript in un oggetto di tipo "rutime".

    The tokenlist should start by "{" and end by "}": this is needed to let the
    function to be recursively callable.

    The returned object contains just an attribute "code" which is a list of

        - un numero
        - una stringa
        - una funzione

    Il numero e la stringa, durante l'esecuzione, sono premuti sullo stack.
    La funzione viene eseguita.
    Una variabile è rappresentata dal suo nome seguito dalla funzione VAR()
    che si aspetta una stringa sullo stack, cerca la variabile corrispondente
    e sostituisce il suo riferimento sullo stack.

    Alcune funzioni fanno riferimento all'elemento successivo nell'array
    code, come JP() che si aspetta un numero e pone this.next pari a quel
    numero implementando un salto.
    
    A runtime, l'ambiente viene arricchito delle risorse usate durante
    l'esecuzione del programma: l'ambiente che contiene le variabili, lo stack
    con i valori temporanei e uno stack per gli ambienti.
*/
let sjs_compile = function(tl)
{
    console.log("> sjs_compile")
    
    let rt = sjs_stdlib({code: []});    // Add the runtime library to the code
    
    let token = tl.shift();
    sjs_compile_error(token.s != "{", "Bad token list", token);

    // Brutally consume the token list as far as it is parsed
    while ((token = tl.shift()).s != "}") {
        //token = tl.shift();
        if (token.s == "break") {
            alert("TODO");
        } else
        if (token.s == "continue") {
            alert("TODO");
        } else
        if (token.s == "do") {
            alert("TODO");
        } else
        if (token.s == "for") {
            alert("TODO");
        } else
        if (token.s == "if") {
            alert("TODO");
        } else
        if (token.s == "let") {
            do {
                token = tl.shift();
                sjs_compile_error(token.t != "name", "Name expected", token);
                rt.code.push(rt.PUSH);
                rt.code.push(token.s);
                token = tl.shift();
                if (token.s == "=") {
                    token = sjs_compile_expression(tl, rt);
                } else {
                    rt.code.push(rt.PUSH);
                    rt.code.push(undefined);    // variable default value
                }
                rt.code.push(rt.LET);
            } while (token.s == ",");
            sjs_compile_error(token.s != ";", "';' expected", token);
        } else
        if (token.s == "return") {
            alert("TODO");
        } else
        if (token.s == "throw") {
            alert("TODO");
        } else
        if (token.s == "try") {
            alert("TODO");
        } else
        if (token.s == "while") {
            alert("TODO");
        } else {
            tl.unshift(token);
            token = sjs_compile_expression(tl, rt);
            sjs_compile_error(token.s != ";", "';' expected", token);
        }
    }
    console.log("< sjs_compile")
    return rt;
}

/** sjs_run(rt) accetta un oggetto di tipo runtime, prodotto da sjs_compile,
    e lo esegue. Durante l'esecuzione utilizza uno stack per i valori, un
    ambiente per le associazioni delle variabili. */
function sjs_run(rt)
{
    rt.ic = 0;
    rt.stack = [];
    
    // rt.env.prev = environment at outer scope.
    rt.env = {console: console, alert: alert, Number: Number, _prev: null};
    
    while (rt.ic < rt.code.length) {
        let ic = rt.ic;
        ++ rt.ic;
        rt.code[ic]();
    }
}
