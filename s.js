/// \file s.js - Self Javascript

"use strict";

/// Return a string representing an object o.
let sjs_print_object = function(o) {
    let s = "";
    if (typeof(o) == "object" || typeof(o) == "function") {
        s += "{";
        for (let x in o) {
            s += x + ":" + sjs_print_object(o[x]) + ",";
        }
        s += "}";
    } else {
        s = o;
    }
    return s;
};

/* ****** LEXICAL ANALYZER ****** */

/** Scans the JS sequence of tokens from string text and returns it as a
    tornandoli in una lista i cui elementi sono oggetti con i
    seguenti attributi: {"s": s, "t": t} dove s e' la rappresentazione
    stringa del token, t il tipo di token, r la riga e c la colonna
    dove il token e' ubicato nella stringa. */
let sjs_scan = function(text)
{
    let ALPHA = "QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm_$";
    let DIGIT = "1234567890";
    let ALNUM = ALPHA + "$_";
    
    // Enclose the text to scan between "{" and "}".
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
        }}
        return i;
    }
    /*  skip_while(i, delims) = least j>i such that text[j] is not
        in delims. If no such j exists then return text.length. */
    let skip_while = function(i, delims) {
        for (++ i; i < text.length && delims.includes(text[i]); ++ i) {
            if (text[i] == "\n") {
                ++ line;
                i_line = i + 1;
        }}
        return i;
    }
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
                i = 1 + skip_until(i, "*");
            } while (text[i] != "/");
            ++ i;
        } else            
        if (ALPHA.includes(c)) {        // Names
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
            let i0 = ++ i;
            i = skip_until(i, c);
            toklist.push({s: text.slice(i0, i), t: "string", l: line, c: i0 - i_line});
            ++ i;
        } else
        if (cc1 == '<=' || cc1 == '>=' || cc1 == '==' || cc1 == '!=' || cc1 == '+='
        || cc1 == '-=' || cc1 == '&&' || cc1 == '||' || cc1 == '++' || cc1 == '--') {
            toklist.push({s: cc1, t: "delimiter", l: line, c: i - i_line});
            i += 2;
        } else
        if ("-+*/%<>=!.,;:()[]{}".includes(c)) {
            toklist.push({s: c, t: "delimiter", l: line, c: i - i_line});
            ++ i;
        } else {
            alert(line + ":" + (i - i_line) + ": Syntax error: " + c);
            return null;
    }}
    // Enclose the text to scan between "{" and "}".
    toklist.push({s: "}", t: "delimiter", l:0, c:0});

    return toklist;
}

/* ****** CODE GENERATOR ****** */

/** Raises an error with message msg if cond is true, else it does nothing.
    If token is not omitted, then it is used to refer to line and column
    where the error occurred. */
let sjs_error = function(cond, msg, token = null)
{
    if (cond) {
        msg = "Error: " + msg;
        if (token != null) { msg += " at " + token.l + ":" + token.c; }
        alert(msg);
        throw null;
    }
};

/** Compile a value to be pushed on the stack at runtime. */
let sjs_compile_value = function(rt, v)
{
    rt.code.push(rt.PUSH);
    rt.code.push(v);
};

/** Compile a list of expressions from tl into an array which is pushed on the
    stack at runtime; the endchr is the one checked against the end of the list.
    This function is used with lists [x1,...,xn] or (x1,...,xn). */
let sjs_compile_array = function(tl, rt, endchr)
{
    console.log("> sjs_compile_array(tl, rt, " + endchr + ")")

    if (endchr == "}") {
        /* An empty object is created and attributes are added to it
            {n1:v1, ..., nk:vk} is implemented as
            {} n1 v1 OBJADD ... nk vk OBJADD */
        sjs_compile_value(rt, {});
        let token = tl[0];  // expect name or '}'
        while (token.s != "}") {
            token = tl.shift();
            sjs_error(token.t != "name", "'}' or 'name:value' expected in literal object " + token.s, token);
            sjs_compile_value(rt, token.s); // bare string, OBJADD expect this
            token = tl.shift(); // this ought to be ':'
            sjs_error(token.s != ":", "':' expected in literal object", token);
            token = sjs_compile_expression(tl, rt);
            rt.code.push(rt.OBJADD);    // obj name value OBJADD -> obj[name] = value and obj on the stack
            sjs_error(token.s != "}" && token.s != ",", "'}' or ',' expected", token);
        }
    } else {
        // An empty array is created and elements are added to it
        // [v1, ..., vk] is implemented as [] v1 ARRADD ... vk ARRADD
        sjs_compile_value(rt, []);
        let token = tl[0];
        while (token.s != endchr) {
            token = sjs_compile_expression(tl, rt);
            rt.code.push(rt.ARRADD);    // a v ARRADD -> a[a.length] = v and a on the stack
            sjs_error(token.s != endchr && token.s != ",", "'" + endchr + "' or ',' expected", token);
        }
    }
    console.log("< sjs_compile_array")
};

/** Compile a list of pairs key:value from tl into an object which is pushed
    on the stack at runtime. */
let sjs_compile_object = function(tl, rt)
{
    console.log("> sjs_compile_object");

    /* An empty object is created and attributes are added to it
        {n1:v1, ..., nk:vk} is implemented as
        {} n1 v1 OBJADD ... nk vk OBJADD */
    sjs_compile_value(rt, {});
    let token = tl[0];  // expect name or '}'
    while (token.s != "}") {
        token = tl.shift();
        sjs_error(token.t != "name", "'}' or 'name:value' expected in literal object " + token.s, token);
        sjs_compile_value(rt, token.s); // bare string, OBJADD expect this
        token = tl.shift(); // this ought to be ':'
        sjs_error(token.s != ":", "':' expected in literal object", token);
        token = sjs_compile_expression(tl, rt);
        rt.code.push(rt.OBJADD);    // obj name value OBJADD -> obj[name] = value and obj on the stack
        sjs_error(token.s != "}" && token.s != ",", "'}' or ',' expected", token);
    }
    console.log("< sjs_compile_object");
};

/** Compile a function() {...} into an object which is pushed on the stack
    at runtime. */
let sjs_compile_function = function(tl, rt)
{
    console.log("> sjs_compile_function");

    /*  A function(params) { body} is compiled as a sequence of three values:
        [CLOSURE(), params, body]. The runtime CLOSURE routine allocates the
        closure by defining its environmente as the runtime environment at the
        time when CLOSURE is executed. The result is then put on the stack. */
    // Parameters.
    let token = tl.shift();
    sjs_error(token.s != "(", "'(' expected in function definition", token);
    let parameters = [];
    token = tl.shift();
    while (token.s != ")") {
        sjs_error(token.t != "name", "name expected in function definition", token);
        parameters.push(token.s);
        token = tl.shift();
        if (token.s == ",") {
            token = tl.shift()
        } else {
            sjs_error(token.s != ")", "')' expected in function definition", token);
        }
    }
    // Function's body
    rt.code.push(rt.CLOSURE);
    rt.code.push(parameters);
    sjs_error(tl[0].s != "{", "'{' expected in function definition", token);
    rt.code.push(sjs_compile(tl).code);
    token = tl.shift();
    sjs_error(tl[0].s != "}", "'}' expected in function definition", token);

    console.log("< sjs_compile_function");
};

/** Compile an operand into rt.code parsing it from the token list tl, which is
    consumed doing so. The first token of the operand is passed in the first
    argument, while the first token following the expression is returned as
    value. */
let sjs_compile_operand = function(token, tl, rt)
{
    console.log("> sjs_compile_operand");
    
    // Check against constants
    if (token.t == "number" || token.t == "string") { sjs_compile_value(rt, token.s); }
    else if (token.s == "true" || token.s == "false") { sjs_compile_value(rt, token.s == "true"); }
    else if (token.s == "null") { sjs_compile_value(rt, null); }
    else if (token.s == "undefined") { sjs_compile_value(rt, undefined); }
    // Check against literal objects
    else if (token.s == "function") { sjs_compile_function(tl, rt); }
    else if (token.s == "{") { sjs_compile_object(tl, rt); }
    else if (token.s == "[") { sjs_compile_array(tl, rt, "]"); }
    // Check against variable name
    else if (token.t == "name") { rt.code.push(rt.REF); rt.code.push(token.s); }
    else {
        // If all else fails, we expect a subexpression "(expr)"
        sjs_error(token.s != "(", "Syntax error: " + token.s, token);
        rt = sjs_compile_expression(tl, rt);
        token = tl.shift();
        sjs_error(token.s != ")", "')' expected", token);
    }
    token = tl.shift();
    
    console.log("< sjs_compile_operand returns '" + token.s + "'");
    
    return token;
}

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
    console.log("> sjs_compile_expression")

    let stack = []; // stack where operators are pushed before being compiled

    let PRIORITIES = {
        "fake": -1, // fake operator, needed in while condition in sjs_compile_stack.
        "=": 0, "+=": 0, "-=": 0, "||": 10, "&&": 15, "==": 20, "!=": 20,
        "<": 21, ">": 21, "<=": 21, ">=": 21, "+": 30, "-": 30, "*": 40,
        "/": 40, "-NEG-": 50, "!": 50, "++": 50, "--": 50, "new": 60
    };
    let OPERATORS = {
        "=": rt.SET, "+=": rt.SETADD, "-=": rt.SETSUB, "==": rt.EQ, "!=": rt.NE,
        "<": rt.LT, ">": rt.GT, "<=": rt.LE, ">=": rt.GE, "+": rt.ADD, "-": rt.SUB,
        "*": rt.MUL, "/": rt.DIV, "-NEG-": rt.NEG, "++": rt.INC, "--": rt.DEC
    };

    /// Compile all operators in the stack with priority > the priority of opt.
    let compile_stack = function(opt) {
        
        console.log("> compile_stack");

        while (stack.length > 0 && PRIORITIES[stack[stack.length-1]] >= PRIORITIES[opt]) {            
            let opt_to_dump = stack.pop();
            if (opt_to_dump == "&&" || opt_to_dump == "||") {
                // pop from cstack where to write the current code position.
                rt.code[cstack.pop()] = rt.code.length;
            } else {
                rt.code.push(OPERATORS[opt_to_dump]);
        }}
        
        console.log("< compile_stack");        
    };
    
    let cstack = [];    // Stack used for forward reference in && and ||
    let token;  // last token parsed from tl inside the do{...} loop
    let again;  // do{...} loop iteration condition, defined inside the loop
    do {
        // Prefix operator or operand expected
        token = tl.shift();
        
        // Deal with possible prefixes
        while (token.s == "!" || token.s == "-" || token.s == "--" || token.s == "++") {
            // A minus token means a negation
            compile_stack(token.s == "-" && "-NEG-" || token.s);
            stack.push(token.s);
            token = tl.shift();
        }
        
        token = sjs_compile_operand(token, tl, rt);
        
        // Now token contains the first token after the operand
        
        console.log("   operator " + token.s + " [" + token.t + "]");
        
        /*  Possible postfix operand: notice that in this context '.', '('
            and '[' have different meaning, they are postfix operators. */
        while (token.s == "." || token.s == "(" || token.s == "[") {
            if (token.s == "(") {   // Actual parameter list
                // First of all takes the value of the function
                sjs_compile_array(tl, rt, ")");
                rt.code.push(rt.APPLY);
            } else
            if (token.s == ".") {   // Member operator x.y
                token = tl.shift();
                sjs_error(token.t != "name", "Name expected as object attribute", token);
                sjs_compile_value(rt, token.s);
                rt.code.push(rt.DEREF);
            } else {
                // token.s == "["!  Member operator x[y]
                token = sjs_compile_expression(tl, rt);
                sjs_error(token.s != "]", "']' expected");
                rt.code.push(rt.DEREF);
            }
            token = tl.shift();
        }
        
        // Possible binary operand
        again = PRIORITIES[token.s] != undefined;   // is the token a bynary operator?
        if (again) {
            // Compile elements in the stack with higher priorities
            compile_stack(token.s);
            stack.push(token.s);
            /* Shortcuts operator need more work: they also compile a JUMP
                and save the index of the element of code containing the
                jump address, to be written after the second operand will
                be compiled. */
            if (token.s == "&&" || token.s == "||") {
                rt.code.push(token.s == "&&" && rt.DUPJPZ || rt.DUPJPNZ);
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

    console.log("< sjs_compile_expression returns '" + token.s + "'")

    return token;
};

/** Given a token list produces a runtime object containing the compiled
    code. */
let sjs_compile = function(tl)
{
    let rt = sjs_rtlib({code: []});
    
    let token = tl.shift();
    sjs_error(token.s != "{", "Bad token list", token);

    // Brutally consume the token list as far as it is parsed
    while ((token = tl.shift()).s != "}") {
        if (token.s == "break") { sjs_error(true, "TODO"); }
        else if (token.s == "catch") { sjs_error(true, "TODO"); }
        else if (token.s == "continue") { sjs_error(true, "TODO"); }
        else if (token.s == "do") { sjs_error(true, "TODO"); }
        else if (token.s == "for") { sjs_error(true, "TODO"); }
        else if (token.s == "if") { sjs_error(true, "TODO"); }
        else if (token.s == "let") {
            do {
                token = tl.shift();
                sjs_error(token.t != "name", "Name expected", token);
                sjs_compile_value(rt, token.s);
                token = tl.shift();
                if (token.s == "=") {
                    token = sjs_compile_expression(tl, rt);
                } else {
                    // variable default value
                    sjs_compile_value(rt, undefined);
                }
                rt.code.push(rt.LET);
            } while (token.s == ",");
            sjs_error(token.s != ";", "';' expected", token);
        }
        else if (token.s == "return") { sjs_error(true, "TODO"); }
        else if (token.s == "throw") { sjs_error(true, "TODO"); }
        else if (token.s == "try") { sjs_error(true, "TODO"); }
        else if (token.s == "while") { sjs_error(true, "TODO"); }
        else {
            // Expression statement
            tl.unshift(token);
            token = sjs_compile_expression(tl, rt);
            sjs_error(token.s != ";", "';' expected", token);
    }}
    return rt;
}

/* ****** RUNTIME VM IMPLEMENTATION ****** */

// Add opcode functions to object rt
let sjs_rtlib = function(rt) {

    /** Pop a reference r, evaluates it by deferenging all its indexed
        and return it. */
    rt.popval = function() {
        let r = rt.stack.pop();
        //~ console.log("r = " + sjs_print_object(r));
        if (r && r.env) {
            /*  To evaluate a reference just apply all dereferences
                that figure in its "at" attribute. */
            for (let i = 0; i < r.at.length; ++ i) {
                //~ console.log("Applico " + r.at[i] + " a " + sjs_print_object(r.env) + " ottenendo " + sjs_print_object(r.env[r.at[i]]) + " di tipo " + typeof(r.env[r.at[i]]));
                r.env = r.env[r.at[i]];
            }
            r = r.env;
        }
        return r;
    };

    /// ADD() pop y, pop x, push x + y
    rt.ADD = function() {
        let y = rt.popval();
        let x = rt.popval();
        rt.stack.push(x + y);
    };
    rt.ADD.$name = "ADD";
    
    /// APPLY() pop a, pop f, apply function f to list x.
    rt.APPLY = function() {
        let a = rt.popval();
        let f = rt.popval();
        
        //~ console.log(sjs_print_object(f) + " :: " + typeof(f));
        
        //sjs_error(typeof(f) != "function", "Apply arguments only to functions");
        
        if (f.apply) { // Native JS function
            rt.stack.push(f.apply(null, a));
        } else {
            // esegue la funzione definita dall'utente!!!
            alert("TODO");
        }
    };
    rt.APPLY.$name = "APPLY";
    
    /** ARRADD() pop v, pop a, { a[a.length] = v; ++ a.length; } push a */
    rt.ARRADD = function() {
        let v = rt.popval();
        let a = rt.popval();
        a.push(v);
        rt.stack.push(a);
    };
    rt.ARRADD.$name = "ARRADD";

    /** CLOSURE() parse x, parse y, creates a closure with parameters the
        elements of x (a list of strings), body the code list y and environment
        the environment active when CLOSURE() is executed. */
    rt.CLOSURE = function() {
        sjs_error(true, "TODO");
    };
    rt.CLOSURE.$name = "CLOSURE";

    /// DEC() pop v, pop x, { x = v; }, push v
    rt.DEC = function() {
        let r = rt.stack.pop();
        sjs_error(r.env == undefined, "Invalid LHS in assignment");
        /*  Apply all dereferences that figure in its "at" attribute. */
        for (let i = 0; i < r.at.length - 1; ++ i) {
            r.env = r.env[r.at[i]];
        }
        -- r.env[r.at[r.at.length - 1]];
        rt.stack.push(r.env[r.at[r.at.length - 1]]);
    };
    rt.DEC.$name = "DEC";

    /** DEREF() pop i, pop a value or reference and add to the
        "at" property the value of i. */
    rt.DEREF = function() {
        let i = rt.popval();
        let r = rt.stack.pop();
        if (r.env != undefined) { r.at.push(i); } else { r = undefined; }
        rt.stack.push(r);
    };
    rt.DEREF.$name = "DEREF";

    /// DIV() pop y, pop x, push x / y
    rt.DIV = function() {
        let y = rt.popval();
        let x = rt.popval();
        rt.stack.push(x / y);
    };
    rt.DIV.$name = "MUL";

    /// DUPJPNZ() pop x, if x != 0 push x and perform JP, else perform NOP.
    rt.DUPJPNZ = function() {
        let r = rt.popval();
        if (r) {
            rt.stack.push(r);
            rt.JP();
        } else {
            ++ rt.ic;   // skip the operand of DUPJPNZ
        }
    };
    rt.DUPJPNZ.$name = "DUPJPNZ";

    /// DUPJPZ() pop x, if x == 0 push x and perform JP, else perform NOP.
    rt.DUPJPZ = function() {
        let r = rt.popval();
        if (!r) {
            rt.stack.push(r);
            rt.JP();
        } else {
            ++ rt.ic;   // skip the operand of DUPJPNZ
        }
    };
    rt.DUPJPZ.$name = "DUPJPZ";

    /// INC() pop v, pop x, { x = v; }, push v
    rt.INC = function() {
        let r = rt.stack.pop();
        sjs_error(r.env == undefined, "Invalid LHS in assignment");
        /*  Apply all dereferences that figure in its "at" attribute. */
        for (let i = 0; i < r.at.length - 1; ++ i) {
            r.env = r.env[r.at[i]];
        }
        ++ r.env[r.at[r.at.length - 1]];
        rt.stack.push(r.env[r.at[r.at.length - 1]]);
    };
    rt.INC.$name = "INC";

    /// JP() get a number and set this.ic to it.
    rt.JP = function() {
        rt.ic = rt.code[rt.ic];
    };
    rt.JP.$name = "JP";
    
    /// JPNZ() pop x, if x == 0 perform JP, else perform NOP.
    rt.JPNZ = function() {
        let r = rt.popval();
        if (r) {
            rt.ic = rt.code[rt.ic];
        } else {
            ++ rt.ic;   // skip operand of the JPZ instruction
        }
    };
    rt.JPNZ.$name = "JPNZ";

    /// JPZ() pop x, if x == 0 perform JP, else perform NOP.
    rt.JPZ = function() {
        let r = rt.popval();
        if (!r) {
            rt.ic = rt.code[rt.ic];
        } else {
            ++ rt.ic;   // skip operand of the JPZ instruction
        }
    };
    rt.JPZ.$name = "JPZ";

    /** LET() pop v, pop s and creates a variable with name s and value v.
        Notice that a variable's value is always an object, with attribute "value"
        the actual value. In this way we can refer to the variable when assigning
        a value to it. */
    rt.LET = function() {
        let v = rt.popval();
        let s = rt.stack.pop(); // string expected
        rt.env[s] = v;
    };
    rt.LET.$name = "LET";
    
    /// MUL() pop y, pop x, push x * y
    rt.MUL = function() {
        let y = rt.popval();
        let x = rt.popval();
        rt.stack.push(x * y);
    };
    rt.MUL.$name = "MUL";

    /// NOP() does nothing at all
    rt.NOP = function() {};
    rt.NOP.$name = "NOP";

    /// OBJADD() pop v, pop n, pop a, { a[n] = v; } push a
    rt.OBJADD = function() {
        let v = rt.popval();
        let n = rt.popval();
        let a = rt.popval();
        a[n] = v;
        rt.stack.push(a);
    };
    rt.OBJADD.$name = "OBJADD";
    
    /// PUSH() parse v, push v
    rt.PUSH = function() {
        rt.stack.push(rt.code[rt.ic]);
        ++ rt.ic;
    };
    rt.PUSH.$name = "PUSH";
        
    /** REF() parse s, look for variable s in the environment, push on the
        stack a reference to the value, thus an object {env:e, . */
    rt.REF = function() {
        let s = rt.code[rt.ic];
        ++ rt.ic;
        for (let e = rt.env; e != null; e = e.$$outer$$) {
            if (e[s] != undefined) {
                rt.stack.push({env: e, at:[s]});
                return;
        }}
        alert("Symbol undefined: " + s);
        rt.stack.push(undefined);
    };
    rt.REF.$name = "REF";

    /// SET() pop v, pop x, { x = v; }, push v
    rt.SET = function() {
        let v = rt.popval();
        let r = rt.stack.pop();
        sjs_error(r.env == undefined, "Invalid LHS in assignment");
        /*  Apply all dereferences that figure in its "at" attribute. */
        for (let i = 0; i < r.at.length - 1; ++ i) {
            r.env = r.env[r.at[i]];
        }
        r.env[r.at[r.at.length - 1]] = v;
        rt.stack.push(v);
    };
    rt.SET.$name = "SET";

    return rt;
};

/** sjs_run(rt) accetta un oggetto di tipo runtime, prodotto da sjs_compile,
    e lo esegue. Durante l'esecuzione utilizza uno stack per i valori, un
    ambiente per le associazioni delle variabili. */
let sjs_run = function(rt, debug)
{
    // rt.env.prev = environment at outer scope.
    rt.env = {console: {log: console.log},
              alert: alert,
              Number: Number,
              $$outer$$: null};
    rt.ic = 0;
    rt.stack = [];
    rt.dump = [];
    
    let stackdump = function() {            
        let s = "Stack: [";
        for (let i = 0; i < rt.stack.length; ++ i) {
            s += sjs_print_object(rt.stack[i]) + " ";
        }
        return s + "]";
    };
    try {
        while (rt.ic < rt.code.length) {
            let ic = rt.ic;
            ++ rt.ic;
            if (debug) {
                console.log(stackdump());
                if (rt.code[ic].$name) {
                    console.log("[" + ic + "] " + rt.code[ic].$name)
                } else {
                    console.log("[" + ic + "] " + rt.code[ic])
                }
            }
            rt.code[ic]();
        }
    }
    catch (e) { alert(e); }
}
