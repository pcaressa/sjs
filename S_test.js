/// \file S.js - Self Javascript

"use strict";

/* _________________________________________________________________________

    HOUSEHOLD FUNCTIONS
   _________________________________________________________________________ */

/** Raises an error with message msg if cond is true, else it does nothing.
    If token is not omitted, then it is used to refer to line and column
    where the error occurred (see the sjs_scan() function. */
let sjs_error = function(cond, msg, token)
{
    if (cond) {
        msg = "Error: " + msg;
        if (token) { msg += " at " + token.l + ":" + token.c; }
        alert(msg);
        throw msg;
    }
};

/// Raise an error on token.t != expected_token.
let sjs_expected = function(token, expected_token)
{
    sjs_error(token.t != expected_token, "'" + expected_token + "' expected", token);
};

/** Return a string with a shallow representation of an object o.
    Since an env can have cyclic references, we omit it when
    parsing it in an object. */
let sjs_object2string = function(o) {
    let s = o;
    if (o == null) { s = "null"; }
    else if (o == undefined) { s = "undefined"; }
    else {
        if (typeof(o) == "object" || typeof(o) == "function") {
            if (o && o["$name"]) { s = o.$name; }
            else {
                if (o.length) {
                    s = "[ ";
                    for (let x in o) {
                        // Prevent some possible infinite loop
                        if (x != "$_ref_$" && x != "env" && x != "$_outer_$") {
                            s += sjs_object2string(o[x]) + " ";
                        } else {
                            s += "{...} ";
                    }}
                    s += "]";
                } else {
                    s = "{ ";
                    for (let x in o) {
                        // Prevent some possible infinite loop
                        if (x != "$_ref_$" && x != "env" && x != "$_outer_$") {
                            s += x + ":" + sjs_object2string(o[x]) + " ";
                        } else {
                            s += x + ":{...} ";
                    }}
                    s += "}";
    }}}}
    return s;
};

/* ****************************************************************************
    L E X I C A L   A N A L Y Z E R
**************************************************************************** */

/** Scans the JS sequence of tokens from the string text and returns it as an
    array of objects {"s": s, "t": t, "l": l, "c": c} where s is the string
    representation of the token, t the token type (number, string or the token
    itself in other cases), l the line number and c the colum number where
    the token occurs inside text. */
let sjs_scan = function(text)
{
    let ALPHA = "QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm_$";
    let DIGIT = "1234567890";
    let ALNUM = ALPHA + DIGIT;
    let KEYWORDS = ["do", "else", "false", "for", "function", "if", "in",
                    "let", "new", "null", "return", "this", "throw",
                    "true", "typeof", "undefined", "while"];
    let OPERATORS = ["**", "<=", ">=", "==", "!=", "+=", "-=",
                     "&&", "||", "++", "--"];

    // Enclose the text to scan between "{" and "}".
    let toklist = [{s: "{", t: "{", l:0, c:0}]; // list of all scanned tokens
    
    let line = 1;   // current line number
    let i_line = 0; // Index of 1st character of the current line

    /*  skip_until(i, delims) = least j>i such that text[j] is in delims.
        If no such j exists then return text.length. Keep track of the
        newlines inside the text so to count line numbers correctly. */
    let skip_until = function(i, delims) {
        for (; i < text.length && !delims.includes(text[i]); ++ i) {
            if (text[i] == "\n") {
                ++ line;
                i_line = i + 1;
        }}
        return i;
    };
    /*  skip_while(i, delims) = least j>i such that text[j] is not in delims.
        If no such j exists then return text.length. Keep track of the
        newlines inside the text so to count line numbers correctly. */
    let skip_while = function(i, delims) {
        for (; i < text.length && delims.includes(text[i]); ++ i) {
            if (text[i] == "\n") {
                ++ line;
                i_line = i + 1;
        }}
        return i;
    };
    let i = 0;
    while (i < text.length) {
        let c = text[i];
        let c1 = text[i+1];
        let cc1 = c + c1;
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
                i = 1 + skip_until(i + 1, "*");
            } while (text[i] != "/");
            ++ i;
        } else            
        if (ALPHA.includes(c)) {        // Names
            let i0 = i;
            i = skip_while(i0 + 1, ALNUM);
            let name = text.slice(i0, i);
            if (KEYWORDS.includes(name)) {
                toklist.push({s: name, t: name, l: line, c: i0 - i_line});
            } else {
                toklist.push({s: text.slice(i0, i), t: "name", l: line, c: i0 - i_line});
            }
        } else
        if (DIGIT.includes(c)) {        // Numbers
            let i0 = i;
            i = skip_while(i0 + 1, DIGIT);
            if (text[i] == ".") {
                i = skip_while(i + 1, DIGIT);
            }
            toklist.push({s: Number(text.slice(i0, i)), t: "number", l: line, c: i0 - i_line});
        } else
        if (c == "'" || c == '"') {     // Strings
            let i0 = ++ i;
            i = skip_until(i, c);
            toklist.push({s: text.slice(i0, i), t: "string", l: line, c: i0 - i_line});
            ++ i;
        } else
        if (OPERATORS.includes(cc1)) {
            toklist.push({s: cc1, t: cc1, l: line, c: i - i_line});
            i += 2;
        } else
        if ("-+*/<>=!.,;:()[]{}".includes(c)) {
            toklist.push({s: c, t: c, l: line, c: i - i_line});
            ++ i;
        } else {
            sjs_error(true, ": Syntax error: '" + c + "'", {l:line, c:(i - i_line)});
    }}
    // Enclose the text to scan between "{" and "}".
    toklist.push({s: "}", t: "}", l:0, c:0});

    return toklist;
};

/* ****************************************************************************
    S Y N T A C T I C   A N A L Y Z E R   A N D   C O D E   G E N E R A T O R
**************************************************************************** */

/** Compiler entry point: accepts a token list as produced by sjs_scan and
    returns a runtime environment with the compiled code and the stuff needed
    by the sjs_run function to actually run the code. On error an exception
    is thrown. */
let sjs_compile = function(tl)
{
    let rt = sjs_runtime();
    sjs_compile_block(tl, rt);
    return rt;
};

/// Compile a sequence s of objects into rt.code.
let sjs_compile_code = function(rt, s, t)
{
    for (let i = 0; i < s.length; ++ i) {
        rt.code.push({i: s[i], l: t.l, c:t.c});
    }
};

/** Compile a value to be pushed on the stack at runtime. */
let sjs_compile_value = function(rt, v, t)
{
    rt.code.push({i: rt.PUSH, l: t.l, c:t.c});
    rt.code.push({i: v, l: t.l, c:t.c});
};

/** \note

    The following functions are used to analyze and compile JS expressions:
    during the compilation. Therein
    - the rt.code array is extended with the code generated;
    - the rt.ic value is increased each time a new function definition is
        parsed (so that rt.ic == 0 means we are not inside any function, so
        that "return" will raise an error).
*/

/** Compile a list of expressions from tl into an array which is pushed on the
    stack at runtime; the endchr is the one checked against the end of the list.
    This function is used with lists [x1,...,xn] or (x1,...,xn). */
let sjs_compile_array = function(tl, rt, endchr)
{
    /*  An empty array is created and elements are added to it
        [v1, ..., vk] is compiled as [] v1 ARRPUSH ... vk ARRPUSH */
    let token = tl[0];                  // probe but not scan the next token
    sjs_compile_value(rt, [], token);   // compile "PUSH []"
    if (token.t == endchr) {
        // Empty list: drop the end of list token and return
        tl.shift();
    } else {
        do {
            token = sjs_compile_expression(tl, rt);
            // a v ARRPUSH -> a[a.length] = v and a on the stack
            sjs_compile_code(rt, [rt.ARRPUSH], token);
        } while (token.t == ",");
        sjs_expected(token, endchr, token);
    }
};

/** Compile a list of pairs key:value from tl into an object which is pushed
    on the stack at runtime. */
let sjs_compile_object = function(tl, rt)
{
    /*  An empty object is created and keys are added to it.
        {n1:v1, ..., nk:vk} is compiled as {} n1 v1 OBJADD ... nk vk OBJADD */
    let token = tl[0];                  // probe but not scan the next token
    sjs_compile_value(rt, {}, token);   // compile "PUSH {}"
    if (token.t == "}") {
        // Empty object: drop the "}" token and return
        tl.shift();
    } else {
        do {
            token = tl.shift();
            // We expect a key: name or string
            sjs_error(token.t != "name" && token.t != "string",
                      "Invalid object key: " + token.s,
                      token);
            sjs_compile_value(rt, token.s, token);  // OBJADD expect this
            sjs_expected(tl.shift(), ":");
            token = sjs_compile_expression(tl, rt);
            // obj name value OBJADD -> obj[name] = value and obj on the stack
            sjs_compile_code(rt, [rt.OBJADD], token);
        } while (token.t == ",");
        sjs_expected(token, "}", token);
    }
};

/** Compile a function() {...} into an object which is pushed on the stack
    at runtime. */
let sjs_compile_function = function(tl, rt)
{
    /*  A function(params) { body} is compiled as a sequence of three values:
        [CLOSURE(), params, body] where
    
        - CLOSURE is the function to be executed at runtime to create the
            closure and push it on the stack.
        - params is the array [x1,...,xn] of formal parameters
        - body is an array containing the compiled code of the closure body
        
        During runtime, the CLOSURE instruction creates a new environment,
        inserts into it the formal parameters as variables whose initial
        values are actual parameters and execute the closure body. */

    // Parse the formal parameter list "(name, ..., name)"
    let parameters = [];
    sjs_expected(tl.shift(), "(");
    let token = tl.shift();
    while (token.t != ")") {
        sjs_expected(token, "name");
        parameters.push(token.s);
        token = tl.shift();
        if (token.t == ",") {
            token = tl.shift();
        } else {
            sjs_expected(token, ")");
    }}
    // Parse and compile the function's body
    ++ rt.ic;   // this means "inside a nested function"
    // Save the current code since we want to compile the body from scratch
    let code_saved = rt.code;
    rt.code = [];
    sjs_compile_block(tl, rt);
    // Save the compiled body and restore the code
    let body = rt.code;
    rt.code = code_saved;
    // If there's no final "return" add it! Needed at runtime.
    if (body[body.length - 1].i != rt.RET) {
        body.push({i: rt.PUSH, l: tl[0].l, c: tl[0].c});
        body.push({i: undefined, l: tl[0].l, c: tl[0].c});
        body.push({i: rt.RET, l: tl[0].l, c: tl[0].c});
    }
    -- rt.ic;
    sjs_compile_code(rt, [rt.CLOSURE, parameters, body], token);
};

/** Compile an operand into rt.code parsing it from the token list tl, which is
    consumed doing so. The first token of the operand is passed in the first
    argument, while the first token following the expression is returned as
    value. */
let sjs_compile_operand = function(token, tl, rt)
{
    // Check against constants
    if (token.t == "number") { sjs_compile_value(rt, token.s, token); }
    else if (token.t == "string") { sjs_compile_value(rt, token.s, token); }
    else if (token.t == "false") { sjs_compile_value(rt, false, token); }
    else if (token.t == "true") { sjs_compile_value(rt, true, token); }
    else if (token.t == "null") { sjs_compile_value(rt, null, token); }
    else if (token.t == "undefined") { sjs_compile_value(rt, undefined, token); }
    // Check against literal objects
    else if (token.t == "function") { sjs_compile_function(tl, rt); }
    else if (token.t == "{") {
        sjs_compile_code(rt, [rt.PUSHTHIS], token);
        sjs_compile_object(tl, rt);
        sjs_compile_code(rt, [rt.POPTHIS], token);
    }
    else if (token.t == "[") {
        sjs_compile_code(rt, [rt.PUSHTHIS], token);
        sjs_compile_array(tl, rt, "]");
        sjs_compile_code(rt, [rt.POPTHIS], token);
    }
    // Check against variable names
    else if (token.t == "name") { sjs_compile_code(rt, [rt.REF, token.s], token); }
    // If all else fails, we expect a subexpression "(expr)"
    else if (token.t == "(") {
        sjs_compile_code(rt, [rt.PUSHTHIS], token);
        sjs_expected(sjs_compile_expression(tl, rt), ")");
        sjs_compile_code(rt, [rt.POPTHIS], token);
    }
    else { sjs_error(true, "Syntax error: " + token.s, token); }
    return tl.shift();
};

/** Compiles an expression into rt.code parsing it from the token list tl,
    which is consumed doing so. The first token following the expression is
    returned as value. To implement operator precedence the function uses a
    classic bottom-up technique: each operator has a priority number
    associated. When an operator is parsed, if the operator on the stack had
    higher priority then the latter is compiled and the former pushed on the
    stack; else the former is pushed on the stack. When the expression has
    completely been parsed, the stack is unwinded by compiling each element
    until it is empty. */
let sjs_compile_expression = function(tl, rt)
{
    let stack = []; // stack where operators are pushed before being compiled

    let PRIORITIES = {
        "fake": 0, // fake operator, needed in while condition in sjs_compile_stack.
        "=": 5, "+=": 5, "-=": 5, "||": 10, "&&": 15, "==": 20, "!=": 20,
        "<": 25, ">": 25, "<=": 25, ">=": 25, "in":25, "+": 30, "-": 30,
        "*": 40, "/": 40, "**": 42, "new": 45, "-NEG-": 50, "!": 50, "++": 50, "--": 50, "typeof": 50
    };
    let OPERATORS = {
        "=": rt.SET, "+=": rt.SETADD, "-=": rt.SETSUB, "==": rt.EQ, "!=": rt.NE,
        "<": rt.LT, ">": rt.GT, "<=": rt.LE, ">=": rt.GE, "in": rt.IN,
        "+": rt.ADD, "-": rt.SUB, "*": rt.MUL, "/": rt.DIV, "**": rt.POW, "new": rt.NEW,
        "-NEG-": rt.NEG, "!": rt.NOT, "++": rt.INC, "--": rt.DEC, "typeof": rt.TYPEOF
    };

    /// Compile all operators in the stack with priority > the priority of opt.
    let compile_stack = function(opt) {
        while (stack.length > 0 && PRIORITIES[stack[stack.length-1]] >= PRIORITIES[opt]) {
            let opt_to_dump = stack.pop();
            if (opt_to_dump == "&&" || opt_to_dump == "||") {
                // pop from cstack where to write the jump.
                let i = cstack.pop();
                rt.code[i].i = rt.code.length - i;
            } else {
                sjs_compile_code(rt, [OPERATORS[opt_to_dump]], tl[0]);
    }}};
    
    let cstack = [];    // Stack used for forward reference in && and ||
    let token;  // last parsed token from tl inside the do{...} loop
    let again;  // do{...} loop iteration condition, defined inside the loop
    
    do {
        token = tl.shift(); // Prefix operator or operand expected
        
        // Is token a prefix operator?
        // (Notice: check tokens via token.t unless is a name, string, number).
        while (token.t == "!" || token.t == "-" || token.t == "--" || token.t == "++" || token.t == "typeof" || token.t == "new") {
            // A minus token means a negation
            let t = token.t == "-" && "-NEG-" || token.t;
            compile_stack(t);
            stack.push(t);
            token = tl.shift();
        }
        // Operand expected in any case.
        token = sjs_compile_operand(token, tl, rt);
        
        // Is token a postfix operator?
        while (token.t == "." || token.t == "(" || token.t == "[") {
            if (token.t == "(") {   // Actual parameter list
                // First of all takes the value of the function
                sjs_compile_code(rt, [rt.PUSHTHIS], token);
                sjs_compile_array(tl, rt, ")");
                sjs_compile_code(rt, [rt.POPTHIS, rt.APPLY], token);
            } else
            if (token.t == ".") {   // Member operator x.y
                token = tl.shift();
                sjs_expected(token, "name");
                sjs_compile_value(rt, token.s, token);
                sjs_compile_code(rt, [rt.DEREF], token);
            } else {
                // Assert token.t == "[";  Member operator x[y]
                sjs_compile_code(rt, [rt.PUSHTHIS], token);
                token = sjs_compile_expression(tl, rt);
                sjs_expected(token, "]");
                sjs_compile_code(rt, [rt.POPTHIS, rt.DEREF], token);
            }
            token = tl.shift();
        }
        // Is there a binary operator?
        again = PRIORITIES[token.t];    // if undefined no!
        if (again) {
            // Compile elements in the stack with higher priorities
            compile_stack(token.t);
            stack.push(token.t);
            /* Shortcuts operator need more work: they also compile a JUMP
                and save the index of the element of code containing the
                jump address, to be written after the second operand will
                be compiled. */
            if (token.t == "&&" || token.t == "||") {
                sjs_compile_code(rt, [token.t == "&&" && rt.DUPJPZ || rt.DUPJPNZ], token);
                /* The index where to jump shall be written at
                    rt.code[rt.code.length] after the second operand will
                    be compiled. Now save the position where to store it
                    in the "control stack". */
                cstack.push(rt.code.length);
                sjs_compile_code(rt, [0], token);    // 0 overwritten in compile_stack.
        }}
    } while (again);

    // If there are operators on the stack, compile them all.
    compile_stack("fake");

    return token;
};

/**
    \note Analyze and compile JS statements.
*/

/** Compile the do {p} while (c) statement. Assume the "do" token
    to be already shifted from tl. */
let sjs_compile_do = function(tl, rt)
{
    /*  "do {p} while(c)" is compiled as "again: p c JPNZ again". */
    sjs_compile_code(rt, [rt.PUSHENV], tl[0]);   // New scope at runtime
    let again = rt.code.length; // where to jump to repeat the loop
    // Compile {p} appending it to rt.code
    sjs_compile_block(tl, rt);  // compile {p}
    sjs_error(tl.shift().s != "while", "'while' expected", rt.code[rt.code.length-1]);
    sjs_expected(sjs_compile_expression(tl, rt), ";");
    // Compile a JPNZ to again to repeat the loop (notice the -1, is correct)
    sjs_compile_code(rt, [rt.JPNZ, again - rt.code.length - 1], tl[0]);
    sjs_compile_code(rt, [rt.POPENV], tl[0]);   // Restore scope at runtime
};

/** Compile the for (let s in e) {p} statement.
    Assume the "for (" tokens to be already shifted from tl. */
let sjs_compile_for_in = function(tl, rt)
{
    /*  for (let s in e) {p} is compiled as:
                PUSH s PUSH null LET        Create variable s
                e OBJKEYS                   Push the array of e keys
        again:  DUP ARRLEN JPZ leave        If the array is empty then finish
                ARRSHIFT                    shift the first key from the array
                REF s SWAP SET DROP         s = array[0]
                p                           execute body
                JP again                    repeat loop
        leave:  DROP                        array no more needed    */
    tl.shift();             // Skip "let"
    let s = tl.shift().s;   // skip s and take note of it
    tl.shift();             // skip "in"
    sjs_compile_code(rt, [rt.PUSHENV,
                          rt.PUSH, s,
                          rt.PUSH, undefined,
                          rt.LET], tl[0]);
    sjs_expected(sjs_compile_expression(tl, rt), ")");
    sjs_compile_code(rt, [rt.OBJKEYS], tl[0]);
    let again = rt.code.length;
    let leave = again + 3;  // index of the 0 placeholder after JPZ
    sjs_compile_code(rt,
                    [rt.DUP,        // DUP since JPZ destroys it
                     rt.ARRLEN,     // Lenght of remained keys to loop on
                     rt.JPZ, 0,     // JP after the loop, 0 is a placeholder
                     rt.ARRSHIFT,   // array -> its shifted 1st element
                     rt.REF, s,
                     rt.SWAP,       // SET needs (... ref val) on the stack
                     rt.SET,
                     rt.DROP        // SET leaves a value, discard it
                     ], tl[0]);
    sjs_compile_block(tl, rt);  // compile {p}
    // Compile a JP to again to repeat the loop (notice the -1, is correct)
    sjs_compile_code(rt, [rt.JP, again - rt.code.length - 1], tl[0]);
    rt.code[leave].i = rt.code.length - leave;
    sjs_compile_code(rt, [rt.DROP, rt.POPENV], tl[0]);
};

/** Compile the for (a; c; i) {p} statement.
    Assume the "for (" tokens to be already shifted from tl. */
let sjs_compile_for = function(tl, rt)
{
    /*  for (a; c; i) {p} is compiled as:
        a again:  c JPZ leave p i JP again leave:   */
    sjs_compile_code(rt, [rt.PUSHENV], tl[0]);
    // Compile a: it can be empty, an expression or a let statement
    if (tl[0].s == "let") {
        tl.shift();     // Skip "let" since compile_let expect this.
        sjs_compile_let(tl, rt);
    } else
    if (tl[0].s == ";") {   // Empty assignment: skip the ";"
        tl.shift();
    } else {
        sjs_expected(sjs_compile_expression(tl, rt), ";");
        sjs_compile_code(rt, [rt.DROP], tl[0]);  // discard the value of the expression
    }
    // Compile c: it can be empty
    let again = rt.code.length;         // where to jump to repeat the loop
    if (tl[0].s == ";") {   // Empty expression: skip the ";"
        tl.shift();
    } else {
        sjs_expected(sjs_compile_expression(tl, rt), ";");
    }
    // Compile JPZ leave
    sjs_compile_code(rt, [rt.JPZ], tl[0]);
    let leave = rt.code.length;         // where to jump to leave the loop
    sjs_compile_code(rt, [0], tl[0]);   // 0 is a placeholder here
    /* Now compile i that eventually will appear after p: to do that,
        mark the beginning of i (which is at leave + 1) and then insert
        the compiled body before it. */
    if (tl[0].s == ")") {   // Empty expression: nothing to do
        tl.shift();
        sjs_compile_block(tl, rt);      // compile {p}
    } else {
        // Compile i, cut it from rt.code, compile p and paste i after p
        sjs_expected(sjs_compile_expression(tl, rt), ")");  // compile i
        let i = rt.code.slice(leave + 1, rt.code.length);   // save i
        rt.code = rt.code.slice(0, leave + 1);              // drop i from code
        sjs_compile_block(tl, rt);      // compile {p}
        // Append the increment/decrement part
        rt.code = rt.code.concat(i);
        // Expression c figures only for side effects: discard its value
        sjs_compile_code(rt, [rt.DROP], tl[0]);
    }
    // Compile the jump to repeat the loop
    sjs_compile_code(rt, [rt.JP, again - rt.code.length - 1], tl[0]);
    // Compile the offset for the JPZ leave here.
    rt.code[leave].i = rt.code.length - leave;
    // Back to the old scope
    sjs_compile_code(rt, [rt.POPENV], tl[0]);
};

/** Compile if (e) {p1} else {p2}. Assume the "if" token to be
    already shifted from tl. */
let sjs_compile_if = function(tl, rt)
{
    /*  if (c) {p1} else {p2} is compiled as
            c JPZ other p1 JP after other: p2 after:
        if (c) {p1} is compiled as
            c JPZ other p1 other: */
    // Compile the (c) condition
    sjs_expected(tl.shift(), "(");
    sjs_expected(sjs_compile_expression(tl, rt), ")");
    /*  Now compile a JPZ to the first instruction after the if:
        that'll be determined after compiling the if-else, so we keep
        track of the index of the code element where this information
        will be stored after the if-else has been compiled. */
    let other = rt.code.length + 1; // index of 0 in the following s
    // 0 after JPZ is a placeholder
    sjs_compile_code(rt, [rt.JPZ, 0, rt.PUSHENV], tl[0]);
    sjs_compile_block(tl, rt);  // compile {p1}
    sjs_compile_code(rt,[rt.POPENV], tl[0]);
    if (tl[0].s != "else") {
        // The if (c) {p1} has been compiled: let JPZ jump here.
        rt.code[other].i = rt.code.length - other;
    } else {
        tl.shift();     // skip "else"
        let after = rt.code.length + 1; // index of 0 in the following s
        sjs_compile_code(rt, [rt.JP, 0], tl[0]);    // to be overwritten later!
        // Let JPZ (after if) jump here.
        rt.code[other].i = rt.code.length - other;
        /*  Now compile the p2 appending inside a new environment, appending
            it to rt.code. Instead of {p2} if may appear a if (...). */
        if (tl[0].s == "if") {
            tl.shift();
            sjs_compile_if(tl, rt, tl[0]);
        } else {
            sjs_compile_code(rt,[rt.PUSHENV], tl[0]);
            sjs_compile_block(tl, rt);
            sjs_compile_code(rt,[rt.POPENV], tl[0]);
        }
        // The if (c) {p1} else {p2} has been compiled: let JP jump here.
        rt.code[after].i = rt.code.length - after;
    }
};

/** Compile a "let x1 = v1,...,xn=vn;" instruction from tl at rt.
    Assume the "let" token to be already shifted from tl. */
let sjs_compile_let = function(tl, rt)
{
    let token;
    do {
        token = tl.shift();
        sjs_expected(token, "name");
        sjs_compile_value(rt, token.s, token);
        token = tl.shift();
        if (token.t == "=") {
            token = sjs_compile_expression(tl, rt);
        } else {
            // variable default value
            sjs_compile_value(rt, undefined, token);
        }
        sjs_compile_code(rt,[rt.LET], tl[0]);
    } while (token.t == ",");
    sjs_expected(token, ";");
};

/** Compile while (c) {p}. Assume the "while" token to be
    already shifted from tl. */
let sjs_compile_while = function(tl, rt)
{
    /*  while (c) {p} is compiled as
        again: (c) JPZ leave p JP again leave:  */
    // New scope introduced at runtime
    sjs_compile_code(rt,[rt.PUSHENV], tl[0]);
    // Compile the (c) condition
    let again = rt.code.length; // jump here to repeat the loop
    sjs_expected(tl.shift(), "(");
    sjs_expected(sjs_compile_expression(tl, rt), ")");
    /*  Now compile a JPZ to the first instruction after the while:
        that'll be determined after compiling the loop, so we keep
        track of the index of the code element where this information
        will be stored after the loop has been compiled. */
    let leave = rt.code.length + 1; // index of 0 in the following s
    sjs_compile_code(rt, [rt.JPZ, 0], tl[0]);   // 0 to be overwritten later!
    sjs_compile_block(tl, rt);  // compile {p}
    // Compile a JP to again to repeat the loop (notice the -1, is correct)
    sjs_compile_code(rt, [rt.JP, again - rt.code.length - 1], tl[0]);
    // Compile the offset for the JPZ leave here.
    rt.code[leave].i = rt.code.length - leave;
    // Back to the old scope
    sjs_compile_code(rt,[rt.POPENV], tl[0]);
};

/** Given a token list produce a runtime object containing the compiled code.
    The token list should ALWAYS be a block {...}. */
let sjs_compile_block = function(tl, rt)
{
    let token = tl.shift();
    sjs_expected(token, "{");

    // Consume the token list as far as it is parsed
    while ((token = tl.shift()).s != "}") {
        if (token.t == ";") { /* Empty statement, nothing to compile! */ }
        else if (token.t == "do") { sjs_compile_do(tl, rt);}
        else if (token.t == "for") {
            sjs_expected(tl.shift(), "(");
            if (tl[0].t == "let" && tl[1].t == "name" && tl[2].t == "in") {
                sjs_compile_for_in(tl, rt);
            } else {
                sjs_compile_for(tl, rt);
            }
        }
        else if (token.t == "if") { sjs_compile_if(tl, rt); }
        else if (token.t == "let") { sjs_compile_let(tl, rt); }
        else if (token.t == "return") {
            sjs_error(rt.ic == 0, "Illegal return statement", token);
            if (tl[0].t != ";") { token = sjs_compile_expression(tl, rt); }
            else {
                sjs_compile_value(rt, undefined, token);
                token = tl.shift();
            }
            sjs_compile_code(rt,[rt.RET], token);
            sjs_expected(token, ";");
        }
        else if (token.t == "throw") {
            token = sjs_compile_expression(tl, rt);
            sjs_compile_code(rt,[rt.THROW], token);
            sjs_expected(token, ";");
        }
        else if (token.t == "while") { sjs_compile_while(tl, rt); }
        else {
            // Expression statement
            tl.unshift(token);
            sjs_expected(sjs_compile_expression(tl, rt), ";");
            sjs_compile_code(rt,[rt.DROP], tl[0]);  // discard the result of the expression
    }}
    sjs_expected(token, "}");
};

/* ****************************************************************************
    R U N T I M E   S T U F F
**************************************************************************** */

/** A runtime object rt, as returned by sjs_compile_block, is executed.
    If debug is not null, 0 nor undefined then execution is dumped on the
    console step by step. On error throws an exception, via sjs_error. */
let sjs_run = function(rt, debug)
{
    // rt.env.$_outer_$ = environment at outer scope.
    /*  The outmost scope is the window object of the JS engine executing
        executing the compiler ;-). */
    rt.env = { $_outer_$: window };
    rt.env.$_outer_$.$_outer_$ = null;
    rt.stack = [];
    rt.dump = [];
    rt.debug = debug;
    rt.$_this_$ = undefined;
    sjs_execute(rt.code, rt);
};

/// Executes code in runtime environment rt
let sjs_execute = function(code, rt)
{
    /// Returns a string representing the stack contents.
    let stackdump = function(rt) {            
        let s = "Stack: [";
        for (let i = 0; i < rt.stack.length; ++ i) {
            s += sjs_object2string(rt.stack[i]) + ", ";
        }
        return s + "]";
    };
    
    // Saves code and ic since sjs_execute can be recursively called.
    let code_saved = rt.code;
    let ic_saved = rt.ic;
    rt.code = code;
    rt.ic = 0;

    while (rt.ic < rt.code.length) {
        let ic = rt.ic;
        ++ rt.ic;
        if (rt.debug) {
            console.log(stackdump(rt));
            console.log("this = " + sjs_object2string(rt.$_this_$));
            if (rt.code[ic].i.$name) {
                console.log("[" + ic + "|" + rt.code[ic].l + ":" + rt.code[ic].c + "] " + rt.code[ic].i.$name);
            } else {
                console.log("[" + ic + "|" + rt.code[ic].l + ":" + rt.code[ic].c + "] " + rt.code[ic].i);
        }}
        rt.code[ic].i(rt);
    }
    // Restores original values
    rt.code = code_saved;
    rt.ic = ic_saved;
};

// Create a runtime environment
let sjs_runtime = function()
{    
    let rt = {code:[], ic:0, stack:[], dump:[]};

    //  AUXILIARY FUNCTIONS
    /** Pop a reference r, deference it and increases its value by v: used by
        DEC, INC, SETADD, SETSUB. */
    rt.increase = function(v) {
        let r = rt.stack.pop();
        sjs_error(!(r && r.$_ref_$), "Invalid LHS in assignment", rt.code[rt.ic]);
        r.$_ref_$[r.$_at_$] += v;
        rt.stack.push(r.$_ref_$[r.$_at_$]);
    };

    /** Pop a value v: if v = {$_ref_$:r, $_at_$:a} then return r[a],
        else return v. */
    rt.popval = function() {
        let v = rt.stack.pop();
        if (v && v.$_ref_$) {   // reference
            v = v.$_ref_$[v.$_at_$];
        }
        return v;
    };
    
    /*
        VM INSTRUCTION IMPLEMENTATIONS
    
        Notice that they have rt as parameter: indeed it may be different
        from the rt defined in this function rt_runtime().
    */

    /// ADD() pop y, pop x, push x + y
    rt.ADD = function(rt) {
        let y = rt.popval();
        rt.stack.push(rt.popval() + y);
    };
    rt.ADD.$name = "ADD";
    
    /// APPLY() pop a, pop f, apply function f to list a.
    rt.APPLY = function(rt) {
        let a = rt.popval();
        let f = rt.popval();
        
        if (f && f.apply) { // Native JS function
            rt.stack.push(f.apply(rt.$_this_$, a));
        } else {
            /*  User defined function: saves rt.env, rt.code, rt.ic on the
                dump stack, next set rt.env to the closure environment, rc.code
                to the closure code, set the values of actual parameters for the
                formal ones and execute the code. */
            // f = {code: [...], env: e, length: 0, parameters: [x1,..,xn]}
            sjs_error(!(f && "parameters" in f), f + " is not a function", rt.code[rt.ic]);
            // Prepare the environment rt.env for the function call
            rt.dump.push(rt.env);
            rt.env = {$_outer_$:f.env}; // closure environment outer to new one
            // Assign actual parameters to formal parameters
            for (let i = 0; i < f.parameters.length; ++ i) {
                rt.env[f.parameters[i]] = a[i]; // undefined if i >= a.length
            }
            sjs_execute(f.code, rt);    // Execute function body.
            // The return value, if any, is on top of rt.stack.
            rt.env = rt.dump.pop();     // Restore the environment from rt.dump.
        }
        rt.$_this_$ = undefined;
    };
    rt.APPLY.$name = "APPLY";

    /// pop a, push a.length
    rt.ARRLEN = function(rt) { rt.stack.push(rt.popval().length); };
    rt.ARRLEN.$name = "ARRLEN";

    /** pop v, pop a, { a[a.length] = v; ++ a.length; } push a */
    rt.ARRPUSH = function(rt) {
        let v = rt.popval();
        let a = rt.popval();
        a.push(v);
        rt.stack.push(a.slice());
    };
    rt.ARRPUSH.$name = "ARRPUSH";

    /// pop a, x = a.shift(), push a, push x
    rt.ARRSHIFT = function(rt) {
        let a = rt.stack[rt.stack.length - 1];
        let x = a.shift();
        rt.stack.push(x);
    };
    rt.ARRSHIFT.$name = "ARRSHIFT";
    
    /** parse x, parse y, creates a closure with parameters the elements of x
        (a list of strings), body the code list y and environment a new one
        containing the parameters and the current environment as $_outer_$. */
    rt.CLOSURE = function(rt) {
        let closure = {length:0,
                       env: {$_outer_$: rt.env},
                       parameters: rt.code[rt.ic].i};
        ++ rt.ic;
        closure.code = rt.code[rt.ic].i;
        ++ rt.ic;
        rt.stack.push(closure);
    };
    rt.CLOSURE.$name = "CLOSURE";

    /// pop v, pop x, { x = v; }, push v
    rt.DEC = function(rt) { rt.increase(-1); };
    rt.DEC.$name = "DEC";

    /*  pop i, pop a value or reference and add to the "at" key the value of i.
        The effect of DEREF is to dereferentiate an object by one of its keys,
        the result still is a referece. Thus, if on the stack there are
        {ref: r, at: a} i then after DEREF it'll be {ref: a, at: i} and
        rt.$_this_$ (thus the "this" variable of the runtime) will be set
        to a. */
    rt.DEREF = function(rt) {
        let i = rt.popval();
        let r = rt.stack.pop();
        if (r["$_ref_$"]) { // reference?
            /* Notice: we have r = {ref:e, at:s} and we want to change
                it int {ref:e[s], at:i}. */
            r = {$_ref_$:r.$_ref_$[r.$_at_$], $_at_$:i};
            rt.$_this_$ = r.$_ref_$;
        } else if (r[i]) {  // object?
            r = {$_ref_$: r, $_at_$:i};
            rt.$_this_$ = r.$_ref_$;
        } else {
            r = undefined;
            rt.$_this_$ = undefined;
        }
        rt.stack.push(r);
    };
    rt.DEREF.$name = "DEREF";

    /// pop y, pop x, push x / y
    rt.DIV = function(rt) {
        let y = rt.popval();
        rt.stack.push(rt.popval() / y);
    };
    rt.DIV.$name = "MUL";
    
    /// pop x, discard the value
    rt.DROP = function(rt) { rt.stack.pop(); };
    rt.DROP.$name = "DROP";

    /// pop x, push x, push x
    rt.DUP = function(rt) { rt.stack.push(rt.stack[rt.stack.length - 1]); };
    rt.DUP.$name = "DUP";

    /// pop x, if x != 0 push x and perform JP, else perform NOP.
    rt.DUPJPNZ = function(rt) {
        let r = rt.popval();
        if (r) {
            rt.stack.push(r);
            rt.JP(rt);
        } else {
            ++ rt.ic;   // skip the operand of DUPJPNZ
        }
    };
    rt.DUPJPNZ.$name = "DUPJPNZ";

    /// pop x, if x == 0 push x and perform JP, else perform NOP.
    rt.DUPJPZ = function(rt) {
        let r = rt.popval();
        if (!r) {
            rt.stack.push(r);
            rt.JP(rt);
        } else {
            ++ rt.ic;   // skip the operand of DUPJPNZ
        }
    };
    rt.DUPJPZ.$name = "DUPJPZ";

    /// pop y, pop x, push x == y
    rt.EQ = function(rt) {
        let y = rt.popval();
        rt.stack.push(rt.popval() == y);
    };
    rt.EQ.$name = "EQ";
    
    /// pop y, pop x, push x >= y
    rt.GE = function(rt) {
        let y = rt.popval();
        rt.stack.push(rt.popval() >= y);
    };
    rt.GE.$name = "GE";

    /// pop y, pop x, push x > y
    rt.GT = function(rt) {
        let y = rt.popval();
        rt.stack.push(rt.popval() > y);
    };
    rt.GT.$name = "GT";

    /// pop x, { ++ x; }, push x
    rt.INC = function(rt) { rt.increase(1); };
    rt.INC.$name = "INC";

    /// pop y, pop x, push (x in y)
    rt.IN = function(rt) {
        let y = rt.popval(), x = rt.popval();
        rt.stack.push(x in y);
    };
    rt.IN.$name = "IN";

    /// parse n, set rt.ic = n.
    rt.JP = function(rt) { rt.ic += rt.code[rt.ic].i; };
    rt.JP.$name = "JP";
    
    /// parse n, pop x, if x != 0 perform JP n.
    rt.JPNZ = function(rt) {
        if (rt.popval()) {
            rt.ic += rt.code[rt.ic].i;
        } else {
            ++ rt.ic;
    }};
    rt.JPNZ.$name = "JPNZ";

    /// parse n, pop x, if x == 0 perform JP n.
    rt.JPZ = function(rt) {
        if (rt.popval()) {
            ++ rt.ic;
        } else {
            rt.ic += rt.code[rt.ic].i;
    }};
    rt.JPZ.$name = "JPZ";

    /// pop y, pop x, push x <= y
    rt.LE = function(rt) {
        let y = rt.popval();
        rt.stack.push(rt.popval() <= y);
    };
    rt.LE.$name = "LE";

    /** pop v, pop s and creates a variable with name s and value v. Notice that
        a variable's value is always an object, with key "value" the actual value.
        In this way we can refer to the variable when assigning a value to it. */
    rt.LET = function(rt) {
        let v = rt.popval();
        let s = rt.stack.pop(); // string expected
        rt.env[s] = v;
    };
    rt.LET.$name = "LET";
    
    /// pop y, pop x, push x < y
    rt.LT = function(rt) {
        let y = rt.popval();
        rt.stack.push(rt.popval() < y);
    };
    rt.LT.$name = "LT";

    /// pop y, pop x, push x * y
    rt.MUL = function(rt) {
        let y = rt.popval();
        rt.stack.push(rt.popval() * y);
    };
    rt.MUL.$name = "MUL";

    /// pop y, pop x, push x != y
    rt.NE = function(rt) {
        let y = rt.popval();
        rt.stack.push(rt.popval() != y);
    };
    rt.NE.$name = "NE";

    /// pop x, push -x
    rt.NEG = function(rt) { rt.stack.push(-rt.popval()); };
    rt.NEG.$name = "NEG";
    
    /// pop a, pop f, push new f(a).
    rt.NEW = function(rt) {
        rt.$_this_$ = Object({});
        rt.APPLY();
        rt.stack.push(rt.$_this_$);
        rt.$_this_$ = undefined;
    };
    rt.NEW.$name = "NEW";

    /// pop x, push !x
    rt.NOT = function(rt) { rt.stack.push(!rt.popval()); };
    rt.NOT.$name = "NOT";

    /// pop v, pop n, pop a, { a[n] = v; } push a
    rt.OBJADD = function(rt) {
        let v = rt.popval();
        let n = rt.popval();
        let a = rt.popval();
        a[n] = v;
        rt.stack.push(a);
    };
    rt.OBJADD.$name = "OBJADD";
    
    /// pop x, push the list of keys of object x.
    rt.OBJKEYS = function(rt) {
        let x = rt.popval();
        rt.stack.push(Object.keys(x));
    };
    rt.OBJKEYS.$name = "OBJKEYS";
    
    /** pop x, parse s, parse c, define a variable s, for each key a in x
        set s = a and executes c. */
    rt.OBJSCAN = function(rt) {
        let x = rt.popval();
        let s = rt.code[rt.ic].i;
        ++ rt.ic;
        let c = rt.code[rt.ic].i;
        ++ rt.ic;
        rt.env[s] = undefined;  // in case x = {}
        rt.dump.push({code:rt.code, ic:rt.ic});
        rt.code = c;
        for (let a in x) {
            rt.env[s] = a;
            rt.ic = 0;
            sjs_execute(rt);
        }
        let saved = rt.dump.pop();
        rt.code = saved.code;
        rt.ic = saved.ic;
    };

    // restore old environment
    rt.POPENV = function(rt) { rt.env = rt.env.$_outer_$; };
    rt.POPENV.$name = "POPENV";
    
    // Restore a previous rt.$_this_$ value.
    rt.POPTHIS = function(rt) { rt.$_this_$ = rt.dump.pop(); };
    rt.POPTHIS.$name = "POPTHIS";
    
    rt.POW = function(rt) {
        let x = rt.popval(), y= rt.popval();
        rt.stack.push(y**x);
    };
    rt.POW.$name = "POW";

    /// parse v, push v
    rt.PUSH = function(rt) {
        let v = rt.code[rt.ic].i;
        ++ rt.ic;
        /*  WARNING: objects need to be cloned, else other instructions
            referring to them will actually change the object following
            rt.PUSH in rt.code, modifying the code itself!!! */
        if (v && v.constructor == Array) {
            rt.stack.push(v.slice());
        } else
        if (v && v.constructor == Object) {
            rt.stack.push(Object.assign({}, v));
        } else {
            rt.stack.push(v);
        }
    };
    rt.PUSH.$name = "PUSH";
    
    // create a new environment
    rt.PUSHENV = function(rt) { rt.env = {$_outer_$: rt.env}; };
    rt.PUSHENV.$name = "PUSHENV";
    
    // Saves the current rt.$_this_$ value.
    rt.PUSHTHIS = function(rt) { rt.dump.push(rt.$_this_$); };
    rt.PUSHTHIS.$name = "PUSHTHIS";
    
    /** parse s, look for variable s in the environment, push on the stack
        a reference to the value, thus an object {ref:e, at:[a} . */
    rt.REF = function(rt) {
        let s = rt.code[rt.ic].i;
        ++ rt.ic;
        // Looks in the runtime environment
        for (let e = rt.env; e != null; e = e.$_outer_$) {
            if (s in e) {
                rt.stack.push({$_ref_$: e, $_at_$:s});
                return;
        }}
        sjs_error(true, s + " is not defined", rt.code[rt.ic]);
    };
    rt.REF.$name = "REF";

    /// Ends the execution of the current rt.code
    rt.RET = function(rt) { rt.ic = rt.code.length; };
    rt.RET.$name = "RET";
    
    /// pop v, pop x, { x = v; }, push v
    rt.SET = function(rt) {
        let v = rt.popval();
        let r = rt.stack.pop();
        sjs_error(!(r && r.$_ref_$), "Invalid LHS in assignment", rt.code[rt.ic]);
        r.$_ref_$[r.$_at_$] = v;
        rt.stack.push(v);
        rt.$_this_$ = undefined;
    };
    rt.SET.$name = "SET";

    /// pop v, pop x, { x += v; }, push v
    rt.SETADD = function(rt) { rt.increase(rt.popval()); };
    rt.SETADD.$name = "SETADD";

    /// pop v, pop x, { x -= v; }, push v
    rt.SETSUB = function(rt) { rt.increase(-rt.popval()); };
    rt.SETSUB.$name = "SETSUB";

    /// pop y, pop x, push x - y
    rt.SUB = function(rt) {
        let y = rt.popval();
        rt.stack.push(rt.popval() - y);
    };
    rt.SUB.$name = "SUB";

    /// pop x, pop y, push x, push y
    rt.SWAP = function(rt) {
        let x = rt.stack.pop(), y = rt.stack.pop();
        rt.stack.push(x);
        rt.stack.push(y);
    };
    rt.SWAP.$name = "SWAP";

    /// pop x, throw exception x
    rt.THROW = function(rt) { throw rt.popval(); };
    rt.THROW.$name = "THROW";

    /// pop v, push the string corresponding to the type of v
    rt.TYPEOF = function(rt) { rt.stack.push(typeof(rt.popval())); };
    rt.TYPEOF.$name = "TYPEOF";
    
    return rt;
};

let tl = sjs_scan("console.log(1+2*3);");
console.log(tl);
let rt = sjs_compile(tl);
console.log(rt);
sjs_run(rt, true);
