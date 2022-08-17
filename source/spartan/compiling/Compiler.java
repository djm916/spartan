package spartan.compiling;

import spartan.runtime.*;
import spartan.data.*;
import spartan.parsing.SourceDatum;
import spartan.parsing.PositionMap;
import spartan.parsing.Position;
import spartan.errors.Error;
import spartan.errors.MalformedExpression;
import spartan.runtime.VirtualMachine;
import java.util.logging.Logger;

public class Compiler
{
  public Compiler(VirtualMachine vm)
  {
    this.vm = vm;
  }
  
  public Inst compile(SourceDatum sourceExp)
  {
    positionMap = sourceExp.positionMap;
    return compile(sourceExp.datum, null, false, null);
  }

  private MalformedExpression malformedExp(Datum exp)
  {
    return new MalformedExpression(positionMap.get(exp));
  }

  /* Determine if an expression is self-evaluating.
     The expression should be returned directly as a value.
  */
  private static boolean isSelfEval(Datum exp)
  {
    return exp.type() == Type.Int
        || exp.type() == Type.BigInt
        || exp.type() == Type.Ratio
        || exp.type() == Type.Real
        || exp.type() == Type.Complex
        || exp.type() == Type.Bool
        || exp.type() == Type.Text
        || exp == List.Empty
        || exp == Nil.Value;
  }
  
  /* Compile a self-evaluating expression. */
  
  private Inst compileSelfEval(Datum exp, Inst next)
  {
    return new LoadConst(exp, next);
  }

  /* Compile a variable reference.
     
     Syntax: an unquoted symbol
     
     If the symbol is a bound local variable, look up its DeBruijn index (a 
     depth, offset pair), and generate a load-local instruction.

     Otherwise, assume the symbol is a global variable, and generate a load-global instruction.
  */

  private Inst compileVarRef(Symbol symb, Scope scope, Inst next)
  {
    var index = (scope == null) ? null : scope.lookup(symb);

    if (index == null)
      return new LoadGlobal(symb, next);
    else
      return new LoadLocal(index.depth, index.offset, next);
  }

  /* Compile a variable assignment.

     Syntax: (set! symbol init)

     If the symbol is a bound local variable, look up its DeBruijn index (a depth, offset
     pair), and generate a store-local(depth, offset) instruction.

     Otherwise, assume the symbol is a global variable, and generate a store-global instruction.
  */

  private Inst compileSet(List exp, Scope scope, Inst next)
  {
    if (exp.length() != 3 || exp.cadr().type() != Type.Symbol)
      throw malformedExp(exp);

    var symb = (Symbol) exp.cadr();
    var init = exp.caddr();
    var index = (scope == null) ? null : scope.lookup(symb);

    if (index == null)
      return compile(init, scope, false,
                     new StoreGlobal(symb,
                     new LoadConst(Nil.Value, next)));
    else
      return compile(init, scope, false,
                     new StoreLocal(index.depth, index.offset,
                     new LoadConst(Nil.Value, next)));
  }

  /* Compile a definition. A definition either binds or mutates a global variable.
  
     Syntax: (def symb init)

     Compilation:

     <<init>>
     store-global symb
     load-const nil
     next: ...
  */

  private Inst compileDef(List exp, Scope scope, Inst next)
  {
    if (exp.length() != 3 || exp.cadr().type() != Type.Symbol)
      throw malformedExp(exp);

    var symb = (Symbol) exp.cadr();
    var init = exp.caddr();

    return compile(init, scope, false,
           new StoreGlobal(symb,
           new LoadConst(Nil.Value,
           next)));
  }

  /* Compile "defun" special form
  
     Syntax: (defun symbol (params...) body...)
  */
  
  private Inst compileDefun(List exp, Scope scope, Inst next)
  {
    if (exp.length() < 4)
      throw malformedExp(exp);
    
    var xform = transformDefun(exp);
    
    if (debug)
      log.info(() -> String.format("defun transform: %s => %s", exp.repr(), xform.repr()));
    
    try {
      return compile(xform, scope, false, next);
    }
    catch (Error err) {
      err.setPosition(positionMap.get(exp));
      throw err;
    }
  }
  
  /* Transform "defun" special form into equivalent equivalent "def" form:
     
     (defun symb (params...) body...) => (def symb (fun (params...) body...))
  */
  private List transformDefun(List exp)
  {
    var symb = exp.cadr();
    var params = exp.caddr();
    var body = exp.cdddr();
    return List.of(Symbols.Def, symb, List.cons(Symbols.Fun, List.cons(params, body)));
  }

  /* Compile the "if" special form.

     Syntax: (if pred sub alt)
             (if pred sub) => (if pred sub nil)

     Compilation:

           <<pred>>
           branch sub alt
     sub:  <<sub>>
           jump next
     alt:  <<alt>>
     next: ...
  */
  private Inst compileIf(List exp, Scope scope, boolean tail, Inst next)
  {
    int length = exp.length();

    if (length < 3 || length > 4)
      throw malformedExp(exp);

    var pred = exp.cadr();
    var sub = exp.caddr();
    var alt = length == 4 ? exp.cadddr() : Nil.Value;

    return compile(pred, scope, false,
           new Branch(compile(sub, scope, tail, next),
                      compile(alt, scope, tail, next)));
  }

  /* Compiles the "cond" special form.

     Syntax: (cond (pred1 body1)
                   ...
                   (predN bodyN))

     Compilation:

     pred1:  <<pred1>>
             branch body1 pred2
     body1:  <<body1>>
             jump next
     pred2:  ...
     predN:  <<predN>>
             branch bodyN none
     bodyN:  <<bodyN>>
             jump next
     none:   load-const nil
     next:   ...
  */
  private Inst compileCond(List exp, Scope scope, boolean tail, Inst next)
  {
    if (exp.length() < 2 || !checkClauseListForm(exp.cdr()))
      throw malformedExp(exp);

    return compileCondClauses(exp.cdr(), scope, tail, next);
  }

  private Inst compileCondClauses(List clauses, Scope scope, boolean tail, Inst next)
  {
    if (clauses == List.Empty)
      return new LoadConst(Nil.Value, next);

    var clause = (List) clauses.car();
    var test = clause.car();
    var body = clause.cdr();

    return compile(test, scope, false,
           new Branch(compileSequence(body, scope, tail, next),
                      compileCondClauses(clauses.cdr(), scope, tail, next)));
  }

  /* Compiles the "let" special form

     Syntax:  (let ((symb1 init1)
                    ...
                    (symbN initN))
                body...)

     Compilation:

     <<initN>>
     push-arg
     ...
     <<init1>>
     push-arg
     push-env N
     pop-arg
     store-local 0 0
     ...
     pop-arg
     store-local 0 N-1
     <<body>>
     pop-env
  */

  private Inst compileLet(List exp, Scope scope, boolean tail, Inst next)
  {
    if (exp.length() < 3 || exp.cadr().type() != Type.List)
      throw malformedExp(exp);

    var bindings = (List) exp.cadr();
    var body = exp.cddr();

    if (!checkBindingListForm(bindings))
      throw malformedExp(bindings);

    int numBindings = bindings.length();
    var vars = extractFirst(bindings);
    var inits = extractSecond(bindings);
    var extendedScope = new Scope(scope, vars);

    return compilePushArgs(inits, scope,
           new PushEnv(numBindings,
           compileBindLocals(0, numBindings,
           compileSequence(body, extendedScope, tail,
           new PopEnv(next)))));
  }

  /* Compiles the "letrec" special form

     Syntax:  (letrec ((symb1 init1)
                       ...
                       (symbN initN))
                body...)

     Compilation:

     push-env N
     <<init1>>
     store-local 0 0
     ...
     <<initN>>
     store-local 0 N-1
     <<body>>
     pop-env
  */
  private Inst compileLetRec(List exp, Scope scope, boolean tail, Inst next)
  {
    if (exp.length() < 3 || exp.cadr().type() != Type.List)
      throw malformedExp(exp);

    var bindings = (List) exp.cadr();
    var body = exp.cddr();

    if (!checkBindingListForm(bindings))
      throw malformedExp(exp);

    int numBindings = bindings.length();
    var vars = extractFirst(bindings);
    var inits = extractSecond(bindings);
    var extendedScope = new Scope(scope, vars);

    return new PushEnv(numBindings,
           compileRecursiveBindings(inits, 0, extendedScope,
           compileSequence(body, extendedScope, tail,
           new PopEnv(next))));
  }

  private Inst compileRecursiveBindings(List inits, int offset, Scope scope, Inst next)
  {
    if (inits == List.Empty)
      return next;

    return compile(inits.car(), scope, false,
                   new StoreLocal(0, offset,
                   compileRecursiveBindings(inits.cdr(), offset + 1, scope,
                   next)));
  }

  /* Compiles the "let*" special form

     Syntax:  (let* ((symb1 init1)
                     ...
                     (symbN initN))
                body...)

     Compilation:

     push-env N
     <<init1>>
     store-local 0 0
     ...
     <<initN>>
     store-local 0 N-1
     <<body>>
     pop-env
  */
  private Inst compileLetStar(List exp, Scope scope, boolean tail, Inst next)
  {
    if (exp.length() < 3 || exp.cadr().type() != Type.List)
      throw malformedExp(exp);

    var bindings = (List) exp.cadr();
    var body = exp.cddr();

    if (!checkBindingListForm(bindings))
      throw malformedExp(exp);

    int numBindings = bindings.length();
    var extendedScope = new Scope(scope, extractFirst(bindings));

    return new PushEnv(numBindings,
           compileSequentialBindings(bindings, 0, new Scope(scope),
           compileSequence(body, extendedScope, tail,
           new PopEnv(next))));
  }

  private Inst compileSequentialBindings(List bindings, int offset, Scope scope, Inst next)
  {
    if (bindings == List.Empty)
      return next;

    var binding = (List) bindings.car();
    var symb = (Symbol) binding.car();
    var init = binding.cadr();

    return compile(init, scope, false,
                   new StoreLocal(0, offset,
                   compileSequentialBindings(bindings.cdr(), offset + 1, scope.bind(symb),
                   next)));
  }

  private boolean checkBindingListForm(List bindings)
  {
    if (bindings == List.Empty)
      return false;

    for (; bindings != List.Empty; bindings = bindings.cdr())
      if (bindings.car().type() != Type.List || !checkBindingPairForm((List) bindings.car()))
        return false;

    return true;
  }

  private boolean checkBindingPairForm(List binding)
  {
    return binding.length() == 2 && binding.car().type() == Type.Symbol;
  }

  private boolean checkParamListForm(List params)
  {
    for (; params != List.Empty; params = params.cdr()) {
      if (params.car().type() != Type.Symbol)
        return false;
      // The symbol & appearing in the parameter list must occur immediately before the final parameter
      if (params.car() == Symbols.Ampersand && (params.cdr() == List.Empty || params.cddr() != List.Empty))
        return false;
    }
    return true;
  }

  private boolean checkClauseListForm(List clauses)
  {
    for (; clauses != List.Empty; clauses = clauses.cdr())
      if (clauses.car().type() != Type.List || ((List) clauses.car()).length() < 2)
        return false;

    return true;
  }

  /* Extract the 1st of each element in a list of pairs:

     ((x1 y1) (x2 y2)...) => (x1 x2...)
  */
  private List extractFirst(List pairs)
  {
    var result = new List.Builder();

    for (; pairs != List.Empty; pairs = pairs.cdr())
      result.add(((List)pairs.car()).car());

    return result.build();
  }

  /* Extract the 2nd of each element in a list of pairs:

     ((x1 y1) (x2 y2)...) => (y1 y2...)
  */
  private List extractSecond(List pairs)
  {
    var result = new List.Builder();

    for (; pairs != List.Empty; pairs = pairs.cdr())
      result.add(((List)pairs.car()).cadr());

    return result.build();
  }

  private Inst compilePushArgs(List args, Scope scope, Inst next)
  {
    if (args == List.Empty)
      return next;

    return compilePushArgs(args.cdr(), scope,
           compile(args.car(), scope, false,
           new PushArg(next)));
  }

  private Inst compilePushArgsUneval(List args, Inst next)
  {
    if (args == List.Empty)
      return next;

    return compilePushArgsUneval(args.cdr(),
           new LoadConst(args.car(),
           new PushArg(next)));
  }
  
  private Inst compileBindLocals(int offset, int numBindings, Inst next)
  {
    if (offset >= numBindings)
      return next;

    return new PopArg(
           new StoreLocal(0, offset,
           compileBindLocals(offset + 1, numBindings, next)));
  }
  
  /* Compiles a procedure application.

     Syntax: (f arg1 arg2 ... argN)

     Compilation:

     push-frame  // omit if in tail position
     <<argN>>
     push-arg
     ...
     <<arg1>>
     push-arg
     <<f>>
     apply
  */

  private Inst compileApply(List exp, Scope scope, boolean tail, Inst next)
  {
    int numArgs = exp.length() - 1;
    
    // Optimization: omit frame for call in tail position
    
    if (tail)
      return compilePushArgs(exp.cdr(), scope,
             compile(exp.car(), scope, false,
             new Apply(numArgs, positionMap.get(exp))));
    
    return new PushFrame(next, positionMap.get(exp),
           compilePushArgs(exp.cdr(), scope,
           compile(exp.car(), scope, false,
           new Apply(numArgs, positionMap.get(exp)))));
  }

  /* Compile the call/cc form.
  
     Syntax: (call/cc exp), where exp is required to be a "fun" expression.
     
     Compilation:
     
     push-frame  // omit if in tail position
     make-cont
     push-arg
     <<exp>>     
     apply
  */
  
  private Inst compileCallCC(List exp, Scope scope, boolean tail, Inst next)
  {
    if (exp.length() != 2)
      throw malformedExp(exp);
    
    // Optimization: omit frame for call in tail position
    
    if (tail)
      return new MakeCont(
             new PushArg(
             compile(exp.cadr(), scope, tail,           
             new Apply(1, positionMap.get(exp.cadr())))));
    
    return new PushFrame(next, positionMap.get(exp),
           new MakeCont(
           new PushArg(
           compile(exp.cadr(), scope, tail,           
           new Apply(1, positionMap.get(exp.cadr()))))));
  }
  
  /* Compiles a sequence of expressions.
     The sequence elements are evaluated in order.
     The value of the last element is the value of the sequence.
     The last expression occurs in tail context.

     Syntax: exp1 exp2 ... expN

     Compilation:

     <<exp1>>
     <<exp2>>
     ...
     <<expN>>
  */
  
  private Inst compileSequence(List exp, Scope scope, boolean tail, Inst next)
  {
    if (exp == List.Empty)
      return next;

    return compile(exp.car(), scope, (tail && exp.cdr() == List.Empty),
           compileSequence(exp.cdr(), scope, tail, next));
  }

  /* Compiles a procedure body. A procedure body consists of a sequence
     of expressions, optionally preceeded by a list of inner definitions.

     Inner definitions are transformed into an equivalent form using a
     nested letrec form, shown below.
  */
  private Inst compileBody(List body, Scope scope, Inst next)
  {
    if (isInnerDef(body.car())) {
      var xform = transformInnerDefs(body);
      if (debug)
        log.info(() -> String.format("inner defs transform: %s => %s", body.repr(), xform.repr()));
      return compile(xform, scope, true, next);
    }

    return compileSequence(body, scope, true, next);
  }
    
  /* Compiles a lambda expression (anonymous procedure)
  */
  private Inst compileFun(List exp, Scope scope, Inst next)
  {
    if (exp.length() < 3 || exp.cadr().type() != Type.List)
      throw malformedExp(exp);

    var params = (List) exp.cadr();
    var body = exp.cddr();
    
    if (!checkParamListForm(params))
      throw malformedExp(exp);
    
    return new MakeClosure(makeProcTemplate(params, body, scope), next);
  }

  /* Transforms a sequence of inner definitions at the beginning
     of a procedure body:

     (fun f params
       (def symb1 init1)
       ...
       (def symbN initN)
       body...)

     into an equivalent nested letrec form:

     (fun f params
       (letrec ((symb1 init1)
                ...
                (symbN initN))
         body...))

     Inner "defun" forms are first transformed:

     (defun symb params body) => (def symb (fun params body))
  */
  private List transformInnerDefs(List body)
  {
    List.Builder bindings = new List.Builder();

    while (body != List.Empty && isInnerDef(body.car())) {
      var exp = (List) body.car();
      if (exp.car() == Symbols.Defun)
        exp = transformDefun(exp);
      bindings.add(exp.cdr());
      body = body.cdr();
    }

    return List.cons(Symbols.LetRec, List.cons(bindings.build(), body));
  }

  /* Determine if an expression is an inner-define form, i.e. a list beginning
     with the symbol "def" or "defun".
  */
  private boolean isInnerDef(Datum exp)
  {
    if (exp.type() != Type.List || exp == List.Empty)
      return false;
    var car = ((List)exp).car();
    return car == Symbols.Def || car == Symbols.Defun;
  }

  /* Compiles the "or" special form, a logical disjunction.

     Syntax: (or exp1 ... expN)

     Compilation:

     test1:    <<exp1>>
               branch done test2
     ...
     testN:    <<expN>>
     done:     ...
  */

  private Inst compileOr(List exp, Scope scope, boolean tail, Inst next)
  {
    if (exp.length() < 2)
      throw malformedExp(exp);

    return compileDisjunction(exp.cdr(), scope, tail, next);
  }

  private Inst compileDisjunction(List exp, Scope scope, boolean tail, Inst next)
  {
    if (exp == List.Empty)
      return next;

    return compile(exp.car(), scope, (tail && exp.cdr() == List.Empty),
           new Branch(next,
                      compileDisjunction(exp.cdr(), scope, tail, next)));
  }

  /* Compiles the "and" special form, a logical conjunction.

     Syntax: (and exp1 ... expN)

     Compilation:

     test1:    <<exp1>>
               branch test2 done
     ...
     testN:    <<expN>>
     done:     ...
  */

  private Inst compileAnd(List exp, Scope scope, boolean tail, Inst next)
  {
    if (exp.length() < 2)
      throw malformedExp(exp);

    return compileConjuction(exp.cdr(), scope, tail, next);
  }

  private Inst compileConjuction(List exp, Scope scope, boolean tail, Inst next)
  {
    if (exp == List.Empty)
      return next;

    return compile(exp.car(), scope, (tail && exp.cdr() == List.Empty),
           new Branch(compileConjuction(exp.cdr(), scope, tail, next),
                      next));
  }

  /* Compiles the "do" special form.

     Syntax: (do exp...)
  */

  private Inst compileDo(List exp, Scope scope, boolean tail, Inst next)
  {
    if (exp.length() < 2)
      throw malformedExp(exp);

    return compileSequence(exp.cdr(), scope, tail, next);
  }

  /* Compiles the "while" special form.

     Syntax: (while pred body...)

     Compilation:

     loop: <<pred>>
           branch body next
     body: <<body>>
           jump loop
     next: load-const nil
           ...
  */

  private Inst compileWhile(List exp, Scope scope, boolean tail, Inst next)
  {
    if (exp.length() < 3)
      throw malformedExp(exp);

    var pred = exp.cadr();
    var body = exp.cddr();
    var jump = new Jump();
    var loop = compile(pred, scope, false,
               new Branch(compileSequence(body, scope, tail, jump),
                          new LoadConst(Nil.Value, next)));
    jump.setTarget(loop);
    return loop;
  }
  
  // (quote x)

  private Inst compileQuote(List exp, Inst next)
  {
    if (exp.length() != 2)
      throw malformedExp(exp);

    return new LoadConst(exp.cadr(), next);
  }

  // Compile a (quasiquote x) form by reducing it to an equivalent
  // list form and compiling the result.

  private Inst compileQuasiquote(List exp, Scope scope, Inst next)
  {
    if (exp.length() != 2)
      throw malformedExp(exp);

    var xform = transformQuasiquote(exp.cadr());
    
    if (debug)
      log.info(() -> String.format("quasiquote transform: %s => %s", exp.repr(), xform.repr()));
    
    try {
      return compile(xform, scope, false, next);
    }
    catch (Error err) {
      err.setPosition(positionMap.get(exp));
      throw err;
    }
  }

  // Reduce a quasiquote form (quasiquote x) to equivalent list form.

  private List transformQuasiquote(Datum exp)
  {
    // (quasiquote ()) => ()
    
    if (exp == List.Empty)
      return List.Empty;
    
    // (quasiquote x) => (quote x) for atomic (non-list) x
    
    if (exp.type() != Type.List)
      return List.of(Symbols.Quote, exp);

    var list = (List) exp;
    
    // Check for the unquote and unquote-splicing forms
    
    if (list.car().type() == Type.List) {
      
      var car = (List) list.car();
      
      // (quasiquote (unquote x) xs...) => (cons x (quasiquote xs...))
      
      if (car.car() == Symbols.Unquote) {
        if (car.length() != 2)
          throw malformedExp(exp);
        return List.cons(Symbols.Cons,
               List.cons(car.cadr(),
               List.cons(transformQuasiquote(list.cdr()),
               List.Empty)));
      }

      // (quasiquote (unquote-splicing x) xs...) => (concat x (quasiquote xs...))    
      
      if (car.car() == Symbols.UnquoteSplicing) {
        if (car.length() != 2)
          throw malformedExp(exp);
        return List.cons(Symbols.Concat,
               List.cons(car.cadr(),
               List.cons(transformQuasiquote(list.cdr()),
               List.Empty)));
      }
    }
    
    // General case: recursively transform the list
    
    // (quasiquote x xs...) => (cons (quasiquote x) (quasiquote xs...))

    return List.cons(Symbols.Cons,
           List.cons(transformQuasiquote(list.car()),
           List.cons(transformQuasiquote(list.cdr()),
           List.Empty)));
  }
  
  private ProcTemplate makeProcTemplate(List params, List body, Scope scope)
  {
    var numParams = params.length();
    var isVariadic = numParams > 1 && params.at(numParams - 2) == Symbols.Ampersand;
    var requiredArgs = isVariadic ? numParams - 2 : numParams;
    
    if (isVariadic)
      params = params.remove((x) -> x == Symbols.Ampersand);
    
    var extendedScope = new Scope(scope, params);
    
    var code = isVariadic ? compileVariadicProc(body, extendedScope, requiredArgs)
                          : compileFixedProc(body, extendedScope, requiredArgs);
    
    return new ProcTemplate(code, requiredArgs, isVariadic);
  }
  
  /* Compile the body of a procedure with a fixed number of arguments N

     Syntax: (fun (param...) body...)

     Compilation:

     push-env N          // extend environment for N arguments
     pop-arg             // bind arguments to parameters
     store-local 0 0
     ...
     pop-arg
     store-local 0 N-1
     <<body>>            // evaluate body
     pop-frame           // return to caller
  */
  
  private Inst compileFixedProc(List body, Scope scope, int requiredArgs)
  {
    return new PushEnv(requiredArgs,
           compileBindLocals(0, requiredArgs,
           compileBody(body, scope,
           new PopFrame())));
  }
  
  /* Compile the body of a variadic procedure with at least N arguments
     and optionally more

     Syntax: (fun (param... & rest) body...)

     Compilation:

     push-env N+1        // extend environment for N+1 arguments
     pop-arg             // bind all but last argument to parameters
     store-local 0 0
     ...
     pop-arg
     store-local 0 N-2
     pop-rest-args       // bind all additional arguments to rest parameter
     store-local 0 N-1
     <<body>>            // evaluate body
     pop-frame           // return to caller
  */
  
  private Inst compileVariadicProc(List body, Scope scope, int requiredArgs)
  {
    return new PushEnv(requiredArgs + 1,
               compileBindLocals(0, requiredArgs,
               new PopRestArgs(
               new StoreLocal(0, requiredArgs,
               compileBody(body, scope,
               new PopFrame())))));
  }

  /* Compile the "defmacro" special form.
     
     Syntax: (defmacro f (param...) body...)
             (defmacro f (param... & rest) body...)
     
     Creates a macro, which is essentially a regular procedure, but
     is intended to generate code rather than data. It receives
     unevaluated expressions as arguments and returns an expression
     which is then compiled and evaluated. 
  */

  private Inst compileDefMacro(List exp, Scope scope, Inst next)
  {
    if (exp.length() < 4 || exp.cadr().type() != Type.Symbol || exp.caddr().type() != Type.List)
      throw malformedExp(exp);

    var symb = (Symbol) exp.cadr();
    var params = (List) exp.caddr();
    var body = exp.cdddr();
    
    if (!checkParamListForm(params))
      throw malformedExp(exp);
    
    vm.globals.bind(symb, new Macro(makeProcTemplate(params, body, null)));
    return new LoadConst(Nil.Value, next);
  }
  
  /* Compile a macro application
  
     Syntax: (f args...)
     
       where f is a macro
     
     Applies the macro procedure to the list of (unevaluated) arguments,
     and compiles the code returned by the macro.
  */
  private Inst compileApplyMacro(List exp, Scope scope, boolean tail, Inst next)
  {
    var f = (Macro) vm.globals.lookup((Symbol) exp.car());
    var args = exp.cdr();
    
    try {
      var xform = applyMacroTransform(f, args);
      
      if (debug)
        log.info(() -> String.format("macro transform: %s => %s", exp.repr(), xform.repr()));
      
      return compile(xform, scope, tail, next);
    }
    catch (Error err) {
      err.setPosition(positionMap.get(exp));
      throw err;
    }
  }
  
  private Datum applyMacroTransform(Macro f, List args)
  {
    return vm.eval(new PushFrame(null, null,
                   compilePushArgsUneval(args,
                   new LoadConst(f,
                   new Apply(args.length(), null)))));
    //vm.pushFrame(null, null);
    //vm.result = f;
    //vm.args = args;
    //return vm.eval(new Apply(args.length(), null));
  }
  
  /* Determine if a symbol is bound to a macro in the global environment. */
  
  private boolean isMacro(Symbol symb)
  {
    return vm.globals.lookup(symb).type() == Type.Macro;
  }
  
  /* Compile a generic list expression, handling special forms and procedure/macro applications. */
  
  private Inst compileList(List exp, Scope scope, boolean tail, Inst next)
  {
    if (exp.car().type() == Type.Symbol) {
      var car = (Symbol) exp.car();
      if (car == Symbols.If)
        return compileIf(exp, scope, tail, next);
      if (car == Symbols.Let)
        return compileLet(exp, scope, tail, next);
      if (car == Symbols.LetStar)
        return compileLetStar(exp, scope, tail, next);
      if (car == Symbols.LetRec)
        return compileLetRec(exp, scope, tail, next);
      if (car == Symbols.Def)
        return compileDef(exp, scope, next);
      if (car == Symbols.Defun)
        return compileDefun(exp, scope, next);
      if (car == Symbols.Defmacro)
        return compileDefMacro(exp, scope, next);
      if (car == Symbols.Fun)
        return compileFun(exp, scope, next);
      if (car == Symbols.Quote)
        return compileQuote(exp, next);
      if (car == Symbols.Quasiquote)
        return compileQuasiquote(exp, scope, next);
      if (car == Symbols.Or)
        return compileOr(exp, scope, tail, next);
      if (car == Symbols.And)
        return compileAnd(exp, scope, tail, next);
      if (car == Symbols.Cond)
        return compileCond(exp, scope, tail, next);
      if (car == Symbols.Do)
        return compileDo(exp, scope, tail, next);
      if (car == Symbols.Set)
        return compileSet(exp, scope, next);
      if (car == Symbols.While)
        return compileWhile(exp, scope, tail, next);      
      if (car == Symbols.CallCC)
        return compileCallCC(exp, scope, tail, next);
      if (isMacro(car))
        return compileApplyMacro(exp, scope, tail, next);
    }
    
    return compileApply(exp, scope, tail, next);
  }

  /* This is the top-level compilation function that dispatches on the type of the expression. */
  
  private Inst compile(Datum exp, Scope scope, boolean tail, Inst next)
  {
    if (isSelfEval(exp))
      return compileSelfEval(exp, next);
    else if (exp.type() == Type.Symbol)
      return compileVarRef((Symbol)exp, scope, next);
    else
      return compileList((List)exp, scope, tail, next);
  }

  private PositionMap positionMap;
  private final VirtualMachine vm;
  private static final boolean debug = 
    Boolean.valueOf(System.getProperty("spartan.debug", "false"));
  private static final Logger log = 
    Logger.getLogger(Compiler.class.getName());
}
