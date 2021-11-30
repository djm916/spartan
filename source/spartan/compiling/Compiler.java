package spartan.compiling;

import spartan.runtime.*;
import spartan.data.*;
import spartan.parsing.SourceDatum;
import spartan.parsing.PositionMap;
import spartan.errors.CompileError;
import spartan.errors.MalformedExpression;
import spartan.errors.MultipleDefinition;
import spartan.errors.RuntimeError;
import spartan.runtime.VirtualMachine;

public class Compiler
{
  public Compiler(VirtualMachine vm)
  {
    this.vm = vm;
  }
  
  public Inst compile(SourceDatum sourceDatum) throws CompileError
  {
    positionMap = sourceDatum.positionMap;
    return compile(sourceDatum.datum, null, false, null);
  }

  private MalformedExpression malformedExp(Datum exp)
  {
    return new MalformedExpression(positionMap.get(exp));
  }

  // Determine if an expression is self-evaluating.
  
  private static boolean isSelfEval(Datum exp)
  {
    return exp.type() == Type.Int
        || exp.type() == Type.Real
        || exp.type() == Type.Complex
        || exp.type() == Type.Text
        || exp == List.Empty
        || exp == Nil.Instance;
  }
  
  // A self-evaluating expression evalutes to itself.
  
  private Inst compileSelfEval(Datum exp, Inst next)
  {
    return new LoadConst(exp, next);
  }

  // (quote x)

  private Inst compileQuote(List exp, Inst next) throws CompileError
  {
    if (exp.length() != 2)
      throw malformedExp(exp);

    return new LoadConst(exp.cadr(), next);
  }

  // Compile a (quasiquote x) form by reducing it to an equivalent
  // list form and compiling the result.

  private Inst compileQuasiquote(List exp, Scope scope, Inst next) throws CompileError
  {
    if (exp.length() != 2)
      throw malformedExp(exp);

    var transformed = transformQuasiquote(exp.cadr());
    //System.out.println("quasiquote transformation = " + transformed.repr());
    return compile(transformed, scope, false, next);
  }

  // Reduce a quasiquote form (quasiquote x) to equivalent list form.

  private List transformQuasiquote(Datum exp) throws CompileError
  {
    // (quasiquote ()) => ()
    
    if (exp == List.Empty)
      return List.Empty;
    
    // (quasiquote x) => (quote x) for atomic (non-list) x
    
    if (exp.type() != Type.List)
      return List.of(Symbol.get("quote"), exp);

    var list = (List) exp;
    
    // Check for the unquote and unquote-splicing forms
    
    if (list.car().type() == Type.List) {
      
      var car = (List) list.car();
      
      // (quasiquote (unquote x) xs...) => (cons x (quasiquote xs...))
      
      if (car.car() == Symbol.get("unquote")) {
        if (car.length() != 2)
          throw malformedExp(exp);
        return List.cons(Symbol.get("cons"),
               List.cons(car.cadr(),
               List.cons(transformQuasiquote(list.cdr()),
               List.Empty)));
      }

      // (quasiquote (unquote-splicing x) xs...) => (concat x (quasiquote xs...))    
      
      if (car.car() == Symbol.get("unquote-splicing")) {
        if (car.length() != 2)
          throw malformedExp(exp);
        return List.cons(Symbol.get("concat"),
               List.cons(car.cadr(),
               List.cons(transformQuasiquote(list.cdr()),
               List.Empty)));
      }
    }
    
    // General case: recursively transform the list
    
    // (quasiquote x xs...) => (cons (quasiquote x) (quasiquote xs...))

    return List.cons(Symbol.get("cons"),
           List.cons(transformQuasiquote(list.car()),
           List.cons(transformQuasiquote(list.cdr()),
           List.Empty)));
  }

  /* Compiles a variable reference
     
     Syntax: an unquoted symbol
     
     If the symbol is a bound local variable, look up its DeBruijn index (a 
     depth, offset pair), and generate a load-local instruction.

     Otherwise, assume the symbol is a global variable, and generate a load-global instruction.
  */

  private Inst compileVarRef(Symbol symb, Scope scope, Inst next)
  {
    var index = (scope == null) ? null : scope.lookup(symb);

    return (index == null) ? new LoadGlobal(symb, next)
                           : new LoadLocal(index.depth, index.offset, next);
  }

  /* Compiles a variable assignment.

     Syntax: (set! symbol init)

     If the symbol is a bound local variable, look up its DeBruijn index (a depth, offset
     pair), and generate a store-local(depth, offset) instruction.

     Otherwise, assume the symbol is a global variable, and generate a store-global instruction.
  */

  private Inst compileSet(List exp, Scope scope, Inst next) throws CompileError
  {
    if (exp.length() != 3 || exp.cadr().type() != Type.Symbol)
      throw malformedExp(exp);

    var symb = (Symbol) exp.cadr();
    var init = exp.caddr();
    var index = (scope == null) ? null : scope.lookup(symb);

    return compile(init, scope, false,
                   index == null ? new StoreGlobal(symb,
                                   new LoadConst(Nil.Instance, next))                                 
                                 : new StoreLocal(index.depth, index.offset,
                                   new LoadConst(Nil.Instance, next)));
  }

  /* A top-level definition either binds a new global variable or modifies an existing one.

     Syntax: (def symb init)

     Compilation:

       <<init>>
       store-global symb
       load-const nil
       next: ...
  */

  private Inst compileDef(List exp, Scope scope, Inst next) throws CompileError
  {
    if (exp.length() != 3 || exp.cadr().type() != Type.Symbol)
      throw malformedExp(exp);

    var symb = (Symbol) exp.cadr();
    var init = exp.caddr();

    return compile(init, scope, false,
           new StoreGlobal(symb,
           new LoadConst(Nil.Instance,
           next)));
  }

  /* Compile the "defun" special form for defining functions by transforming to
     the equivalent "def" form.
  */
  
  private Inst compileDefun(List exp, Scope scope, Inst next) throws CompileError
  {
    if (exp.length() < 4)
      throw malformedExp(exp);

    return compile(transformDefun(exp), scope, false, next);
  }
  
  // (defun symbol (params...) body...) => (def symbol (fun (params...) body...))
  
  private List transformDefun(List exp)
  {
    var symb = exp.cadr();
    var params = exp.caddr();
    var body = exp.cdddr();
    var result = List.of(Symbol.get("def"), symb, List.cons(Symbol.get("fun"), List.cons(params, body)));
    //System.out.println("defun xform: " + result.repr());
    return result;
  }

  /* Compile an "if" special form.

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
  private Inst compileIf(List exp, Scope scope, boolean tail, Inst next) throws CompileError
  {
    int length = exp.length();

    if (length < 3 || length > 4)
      throw malformedExp(exp);

    var pred = exp.cadr();
    var sub = exp.caddr();
    var alt = length == 4 ? exp.cadddr() : Nil.Instance;

    return compile(pred, scope, false,
           new Branch(compile(sub, scope, tail, next),
                      compile(alt, scope, tail, next)));
  }

  /* Compiles a "cond" special form.

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
  private Inst compileCond(List exp, Scope scope, boolean tail, Inst next) throws CompileError
  {
    if (exp.length() < 2 || !checkClauseListForm(exp.cdr()))
      throw malformedExp(exp);

    return compileCondClauses(exp.cdr(), scope, tail, next);
  }

  private boolean checkClauseListForm(List clauses)
  {
    for (; clauses != List.Empty; clauses = clauses.cdr())
      if (clauses.car().type() != Type.List || ((List) clauses.car()).length() < 2)
        return false;

    return true;
  }

  private Inst compileCondClauses(List clauses, Scope scope, boolean tail, Inst next) throws CompileError
  {
    if (clauses == List.Empty)
      return new LoadConst(Nil.Instance, next);

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

  private Inst compileLet(List exp, Scope scope, boolean tail, Inst next) throws CompileError
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

    return evalArgs(inits, scope,
           new PushEnv(numBindings,
           bindLocals(0, numBindings,
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
  private Inst compileLetRec(List exp, Scope scope, boolean tail, Inst next) throws CompileError
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

  private Inst compileRecursiveBindings(List inits, int offset, Scope scope, Inst next) throws CompileError
  {
    if (inits.empty())
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
  private Inst compileLetStar(List exp, Scope scope, boolean tail, Inst next) throws CompileError
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
           compileBindingsSequential(bindings, 0, new Scope(scope),
           compileSequence(body, extendedScope, tail,
           new PopEnv(next))));
  }

  private Inst compileBindingsSequential(List bindings, int offset, Scope scope, Inst next) throws CompileError
  {
    if (bindings.empty())
      return next;

    var binding = (List) bindings.car();
    var symb = (Symbol) binding.car();
    var init = binding.cadr();

    return compile(init, scope, false,
                   new StoreLocal(0, offset,
                   compileBindingsSequential(bindings.cdr(), offset + 1, scope.bind(symb),
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

  /* Extract the 1st of each element in a list of pairs:

     ((x1 y1) (x2 y2)...) => (x1 x2...)
  */
  private List extractFirst(List pairs)
  {
    var result = new List.Builder();

    for (; !pairs.empty(); pairs = pairs.cdr())
      result.add(((List)pairs.car()).car());

    return result.build();
  }

  /* Extract the 2nd of each element in a list of pairs:

     ((x1 y1) (x2 y2)...) => (y1 y2...)
  */
  private List extractSecond(List pairs)
  {
    var result = new List.Builder();

    for (; !pairs.empty(); pairs = pairs.cdr())
      result.add(((List)pairs.car()).cadr());

    return result.build();
  }

  private Inst evalArgs(List args, Scope scope, Inst next) throws CompileError
  {
    if (args == List.Empty)
      return next;

    return evalArgs(args.cdr(), scope,
           compile(args.car(), scope, false,
           new PushArg(next)));
  }

  private Inst bindLocals(int offset, int numBindings, Inst next)
  {
    if (offset >= numBindings)
      return next;

    return new PopArg(
           new StoreLocal(0, offset,
           bindLocals(offset + 1, numBindings, next)));
  }

  /* Compiles a function application.

     Syntax: (f arg1 arg2 ... argN)

     Compilation:

     push-frame  // note: frame ommitted when call appears in tail context
     <<argN>>
     push-arg
     ...
     <<arg1>>
     push-arg
     <<f>>
     apply
  */

  private Inst compileApply(List exp, Scope scope, boolean tail, Inst next) throws CompileError
  {
    int numArgs = exp.length() - 1;

    return tail ? evalArgs(exp.cdr(), scope,
                  compile(exp.car(), scope, false,
                  new Apply(numArgs, positionMap.get(exp))))

                : new PushFrame(next,
                  evalArgs(exp.cdr(), scope,
                  compile(exp.car(), scope, false,
                  new Apply(numArgs, positionMap.get(exp)))));
  }

  /* Compiles a lambda expression (anonymous function)

     Case 1: Fixed number of arguments

       Syntax: (fun (param...) body...)

       Compilation:

       push-env N          // extend environment for N parameters
       pop-arg             // bind arguments to parameters
       store-local 0 0
       ...
       pop-arg
       store-local 0 N-1
       <<body>>            // Evaluate body
       pop-frame           // Return

     Case 2: Variable number of arguments

       Syntax: (fun (param... &rest) body...)

       Compilation:

       push-env N          // extend environment for N parameters
       pop-arg             // bind all but last argument to parameters
       store-local 0 0
       ...
       pop-arg
       store-local 0 N-2
       pop-rest-args       // bind rest argument to
       store-local 0 N-1
       <<body>>
       pop-frame
  */
  private Inst compileFun(List exp, Scope scope, Inst next) throws CompileError
  {
    if (exp.length() < 3 || exp.cadr().type() != Type.List)
      throw malformedExp(exp);

    var params = (List) exp.cadr();
    var body = exp.cddr();

    if (!checkParamListForm(params))
      throw malformedExp(exp);

    var numParams = params.length();
    var isVariadic = numParams != 0 && isRestParam((Symbol) params.at(numParams - 1));
    var requiredArgs = isVariadic ? numParams - 1 : numParams;
    var extendedScope = new Scope(scope, params);

    var code = !isVariadic ? new PushEnv(numParams,
                             bindLocals(0, numParams,
                             compileBody(body, extendedScope,
                             new PopFrame())))

                           : new PushEnv(numParams,
                             bindLocals(0, numParams - 1,
                             new PopRestArgs(
                             new StoreLocal(0, numParams - 1,
                             compileBody(body, extendedScope,
                             new PopFrame())))));

    return new MakeClosure(code, requiredArgs, isVariadic, next);
  }

  /* Compiles a function body. A function body consists of a sequence
     of expressions, optionally preceeded by a list of inner definitions.

     Inner definitions are transformed into an equivalent form using a
     nested letrec form, shown below.
  */
  private Inst compileBody(List body, Scope scope, Inst next) throws CompileError
  {
    if (isInnerDef(body.car())) {
      var xform = transformInnerDefs(body);
      //System.out.println("inner defs xform = " + xform.repr());
      return compile(xform, scope, true, next);
    }

    return compileSequence(body, scope, true, next);
  }

  /* Transforms a sequence of inner definitions at the beginning
     of a function body:

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

    // TODO: check for empty list after inner defs to prevent NPE
    while (isInnerDef(body.car())) {
      var exp = (List) body.car();
      if (exp.car() == Symbol.get("defun"))
        exp = transformDefun(exp);
      bindings.add(exp.cdr());
      body = body.cdr();
    }

    return List.cons(Symbol.get("letrec"), List.cons(bindings.build(), body));
  }

  private boolean isInnerDef(Datum exp)
  {
    return exp.type() == Type.List &&
      (((List)exp).car() == Symbol.get("def") ||
       ((List)exp).car() == Symbol.get("defun"));
  }

  private boolean isRestParam(Symbol s)
  {
    return s.repr().charAt(0) == '&';
  }

  private boolean checkParamListForm(List params)
  {
    for (; params != List.Empty; params = params.cdr()) {
      if (params.car().type() != Type.Symbol)
        return false;
      if (isRestParam((Symbol) params.car()) && params.cdr() != List.Empty)
        return false;
    }
    return true;
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

  private Inst compileOr(List exp, Scope scope, boolean tail, Inst next) throws CompileError
  {
    if (exp.length() < 2)
      throw malformedExp(exp);

    return compileDisjunction(exp.cdr(), scope, tail, next);
  }

  private Inst compileDisjunction(List exp, Scope scope, boolean tail, Inst next) throws CompileError
  {
    if (exp.empty())
      return next;

    return compile(exp.car(), scope, tail && exp.cdr().empty(),
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

  private Inst compileAnd(List exp, Scope scope, boolean tail, Inst next) throws CompileError
  {
    if (exp.length() < 2)
      throw malformedExp(exp);

    return compileConjuction(exp.cdr(), scope, tail, next);
  }

  private Inst compileConjuction(List exp, Scope scope, boolean tail, Inst next) throws CompileError
  {
    if (exp.empty())
      return next;

    return compile(exp.car(), scope, tail && exp.cdr().empty(),
           new Branch(compileConjuction(exp.cdr(), scope, tail, next),
                      next));
  }

  /* Compiles the "do" special form.

     Syntax: (do exp...)
  */

  private Inst compileDo(List exp, Scope scope, boolean tail, Inst next) throws CompileError
  {
    if (exp.length() < 2)
      throw malformedExp(exp);

    return compileSequence(exp.cdr(), scope, tail, next);
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

  private Inst compileSequence(List exp, Scope scope, boolean tail, Inst next) throws CompileError
  {
    if (exp == List.Empty)
      return next;

    return compile(exp.car(), scope, (tail && exp.cdr().empty()),
           compileSequence(exp.cdr(), scope, tail, next));
  }

  /* Compiles the "while" special form.

     Syntax: (while pred body...)

     Compilation:

     loop: <<pred>>
           branch body next
     body: <<body>>
           jump loop
     next: ...
  */

  private Inst compileWhile(List exp, Scope scope, boolean tail, Inst next) throws CompileError
  {
    if (exp.length() < 3)
      throw malformedExp(exp);

    var pred = exp.cadr();
    var body = exp.cddr();
    var jump = new Jump();
    var loop = compile(pred, scope, false,
               new Branch(compileSequence(body, scope, tail, jump),
                          next));
    jump.setTarget(loop);
    return loop;
  }

  /* Compiles the "delay" special form.

     Syntax: (delay exp...)

     Equivalent to:

     (let ((thunk (fn () exp...))
           (value-ready? false)
           (cached-value nil))
       (fn () (if (not value-ready?)
                (do (set! cached-value (thunk))
                    (set! value-ready? true))
              cached-value)))


     Compilation:

     push-env 2
     load-const false
     store-local 0 0
     load-const nil
     store-local 0 1
     make-promise body
     pop-env

     body:   load-local 0 0
             branch done
             <<exp>>
             store-local 0 1
             load-const true
             store-local 0 0
     done:   load-local 0 1
             pop-frame

  */

  /* NOTE: Now implemented using macros
  
  private Inst compileDelay(List exp, Scope scope, boolean tail, Inst next) throws CompileError
  {
    if (exp.length() < 2)
      throw malformedExp(exp);

    Inst done = new LoadLocal(0, 1,
                new PopFrame());

    Inst body = new LoadLocal(0, 0,
                new Branch(done,
                  compileSequence(exp.cdr(), new Scope(scope), false,
                  new StoreLocal(0, 1,
                  new LoadConst(Bool.True,
                  done)))));

    return new PushEnv(2,
           new LoadConst(Bool.False,
           new StoreLocal(0, 0,
           new LoadConst(Nil.Instance,
           new StoreLocal(0, 1,
           new MakePromise(body,
           new PopEnv(next)))))));
  }
  */
  
  /* Compile the "defmacro" special form.
     
     Syntax: (defmacro f (param...) body...)
     
     Creates a macro, which is essentially just a regular procedure,
     but is intended to transform code at compile time.
  */

  private Inst compileDefMacro(List exp, Scope scope, Inst next) throws CompileError
  {
    if (exp.length() < 4 || exp.cadr().type() != Type.Symbol || exp.caddr().type() != Type.List)
      throw malformedExp(exp);

    var symb = (Symbol) exp.cadr();
    var macro = makeMacro(exp);
    vm.globals.bind(symb, macro);
    return new LoadConst(Nil.Instance, next);
  }
  
  /* Create a macro instance given its definition.
  */
  private Macro makeMacro(List exp) throws CompileError
  {
    var params = (List) exp.caddr();
    var body = exp.cdddr();

    if (!checkParamListForm(params))
      throw malformedExp(exp);

    var numParams = params.length();
    var isVariadic = numParams != 0 && isRestParam((Symbol) params.at(numParams - 1));
    var requiredArgs = isVariadic ? numParams - 1 : numParams;
    var extendedScope = new Scope(null, params);

    var code = !isVariadic ? new PushEnv(numParams,
                             bindLocals(0, numParams,
                             compileBody(body, extendedScope,
                             new PopFrame())))

                           : new PushEnv(numParams,
                             bindLocals(0, numParams - 1,
                             new PopRestArgs(
                             new StoreLocal(0, numParams - 1,
                             compileBody(body, extendedScope,
                             new PopFrame())))));

    return new Macro(code, requiredArgs, isVariadic);
  }
  
  /* Compile a macro application.
  
     Syntax: (f args...), where f was previously defined with defmacro.
     
     First lookup the macro procedure bound to f, apply this procedure to
     the list of (unevaluated) arguments, and finally compile the code produced
     by the macro procedure.
  */
  private Inst compileApplyMacro(List exp, Scope scope, boolean tail, Inst next) throws CompileError
  {
    var macro = vm.globals.lookup((Symbol)exp.car());
    var args = exp.cdr();
    Datum expansion = null;

    try {
      vm.pushFrame(null);
      vm.result = macro;
      vm.args = args;
      expansion = vm.eval(new Apply(args.length(), positionMap.get(exp)));
    }
    catch (RuntimeError err) {
      throw new CompileError(err.getMessage(), err.position);
    }

    System.out.println("macro expansion: " + expansion.repr());
    return compile(expansion, scope, tail, next);
  }
  
  // Determine if a symbol is bound to a macro in the global environment.
  
  public boolean isMacro(Symbol symb)
  {
    return vm.globals.lookup(symb).type() == Type.Macro;
  }
  
  // Compile a list, including special forms, function applications, and macro applications.
  
  private Inst compileList(List exp, Scope scope, boolean tail, Inst next) throws CompileError
  {
    if (exp.car().type() == Type.Symbol) {
      var car = (Symbol) exp.car();
      if (car == Symbol.get("if"))
        return compileIf(exp, scope, tail, next);
      if (car == Symbol.get("let"))
        return compileLet(exp, scope, tail, next);
      if (car == Symbol.get("let*"))
        return compileLetStar(exp, scope, tail, next);
      if (car == Symbol.get("letrec"))
        return compileLetRec(exp, scope, tail, next);
      if (car == Symbol.get("def"))
        return compileDef(exp, scope, next);
      if (car == Symbol.get("defun"))
        return compileDefun(exp, scope, next);
      if (car == Symbol.get("defmacro"))
        return compileDefMacro(exp, scope, next);
      if (car == Symbol.get("fun"))
        return compileFun(exp, scope, next);
      if (car == Symbol.get("quote"))
        return compileQuote(exp, next);
      if (car == Symbol.get("quasiquote"))
        return compileQuasiquote(exp, scope, next);
      if (car == Symbol.get("or"))
        return compileOr(exp, scope, tail, next);
      if (car == Symbol.get("and"))
        return compileAnd(exp, scope, tail, next);
      if (car == Symbol.get("cond"))
        return compileCond(exp, scope, tail, next);
      if (car == Symbol.get("do"))
        return compileDo(exp, scope, tail, next);
      if (car == Symbol.get("set!"))
        return compileSet(exp, scope, next);
      if (car == Symbol.get("while"))
        return compileWhile(exp, scope, tail, next);
      // NOTE: Now implemented using macros
      //if (car == Symbol.get("delay"))
        //return compileDelay(exp, scope, tail, next);
      if (isMacro(car))
        return compileApplyMacro(exp, scope, tail, next);
    }
    
    return compileApply(exp, scope, tail, next);
  }

  private Inst compile(Datum exp, Scope scope, boolean tail, Inst next) throws CompileError
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
}
