package spartan.compiling;

import spartan.runtime.*;
import spartan.data.*;
import spartan.parsing.SourceDatum;
import spartan.parsing.PositionMap;
import spartan.errors.CompileError;
import spartan.errors.MalformedExpression;
import spartan.errors.MultipleDefinition;
import spartan.errors.WrongNumberArgs;
import spartan.errors.RuntimeError;
import spartan.runtime.VirtualMachine;
import spartan.runtime.BaseEnv;

public class Compiler
{
  public Inst compile(SourceDatum sourceDatum) throws CompileError
  {
    positionMap = sourceDatum.positionMap;
    return compile(sourceDatum.datum, null, false, null);
  }
  
  private MalformedExpression malformedExp(Datum exp)
  {
    return new MalformedExpression(positionMap.get(exp));
  }
  
  private MultipleDefinition multipleDef(Symbol var)
  {
    return new MultipleDefinition(var, positionMap.get(var));
  }
  
  private static boolean isSelfEval(Datum exp)
  {
    return exp.type() == Type.Int
        || exp.type() == Type.Real
        || exp.type() == Type.Complex
        || exp.type() == Type.Text
        || exp == List.Empty
        || exp == Nil.Instance;
  }

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
  
  // (quasiquote x)
  
  private Inst compileQuasiquote(List exp, Scope scope, Inst next) throws CompileError
  {
    if (exp.length() != 2)
      throw malformedExp(exp);
    
    var transformed = transformQuasiquote(exp.cadr());
    
    System.out.println("quasiquote transformation = " + transformed.repr());
    
    return compile(transformed, scope, false, next);
  }
  
  // Reduce a quasiquote form (quasiquote x)
  
  private List transformQuasiquote(Datum exp) throws CompileError
  {
    // (quasiquote x) => (quote x) for non-list x
    
    if (exp.type() != Type.List || exp == List.Empty)
      return List.of(Symbol.get("quote"), exp);
    
    List list = (List) exp;
    
    // (quasiquote (unquote x) xs...) => (cons x (quasiquote xs...))
    
    if (list.car().type() == Type.List) {
      List list2 = (List) list.car();
      if (list2.car() == Symbol.get("unquote")) {
        if (list2.length() != 2)
          throw malformedExp(exp);
        return List.cons(Symbol.get("cons"),
               List.cons(list2.cadr(),
               List.cons(transformQuasiquote(list.cdr()),
               List.Empty)));
      }
    }
    
    // (quasiquote (unquote-splicing x) xs...) => (concat (quote x) (quasiquote xs...))
    
    if (list.car().type() == Type.List) {
      List list2 = (List) list.car();
      if (list2.car() == Symbol.get("unquote-splicing")) {
        if (list2.length() != 2)
          throw malformedExp(exp);
        return List.cons(Symbol.get("concat"),
               List.cons(List.of(Symbol.get("quote"), list2.cadr()),
               List.cons(transformQuasiquote(list.cdr()),
               List.Empty)));
      }
    }
    
    // (quasiquote x xs...) => (cons (quote x) (quasiquote xs))
    
    return List.cons(Symbol.get("cons"),
           List.cons(List.of(Symbol.get("quote"), list.car()),
           List.cons(transformQuasiquote(list.cdr()),
           List.Empty)));
  }
  
  private Inst compileVarRef(Symbol symb, Scope scope, Inst next)
  {
    var index = (scope == null) ? null : scope.lookup(symb);
    
    return (index == null) ? new LoadGlobal(symb, positionMap.get(symb), next)
                           : new LoadLocal(index.depth, index.offset, next);
  }
  
  // (def symb init)
  
  private Inst compileDef(List exp, Scope scope, Inst next) throws CompileError
  {
    if (exp.length() != 3 || exp.cadr().type() != Type.Symbol)
      throw malformedExp(exp);
    
    var symb = (Symbol) exp.cadr();
    var init = exp.caddr();
    
    return compile(init, scope, false,
           new StoreGlobal(symb, next));
  }
  
  // (defun f (param...) body...)
  
  private Inst compileDefun(List exp, Scope scope, Inst next) throws CompileError
  {
    if (exp.length() < 4)
      throw malformedExp(exp);
    
    return compile(transformDefun(exp), scope, false, next);
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
    if (exp.length() < 2 || !checkCondClauses(exp.cdr()))
      throw malformedExp(exp);
    
    return compileCondClauses(exp.cdr(), scope, tail, next);
  }
  
  private boolean checkCondClauses(List clauses)
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
    
    if (!checkLetBindings(bindings))
      throw malformedExp(bindings);
    
    int numBindings = bindings.length();

    return evalBindings(bindings, scope,
           new PushEnv(numBindings,
           performBindings(0, numBindings,
           compileSequence(body, extendLetScope(bindings, scope), tail,
           new PopEnv(next)))));
  }
  
  /* Compiles the "letrec" special form
  
     Syntax:  (letrec ((symb1 init1)
                       ...
                       (symbN initN))
                body...)
     
     Compilation:
     
     push-env N
     <<initN>>
     push-arg
     ...
     <<init1>>
     push-arg     
     pop-arg
     store-local 0 0
     ...
     pop-arg
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
    
    if (!checkLetBindings(bindings))
      throw malformedExp(exp);
    
    scope = extendLetScope(bindings, scope);
    
    int numBindings = bindings.length();
    
    return new PushEnv(numBindings,
           evalBindings(bindings, scope,
           performBindings(0, numBindings,
           compileSequence(body, scope, tail,
           new PopEnv(next)))));
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
    
    if (!checkLetBindings(bindings))
      throw malformedExp(exp);
    
    int numBindings = bindings.length();
    
    return new PushEnv(numBindings,
           bindLocalsSequential(bindings, 0, new Scope(scope),
           compileSequence(body, extendLetScope(bindings, scope), tail,
           new PopEnv(next))));
  }
  
  private Inst bindLocalsSequential(List bindings, int offset, Scope scope, Inst next) throws CompileError
  {
    if (bindings.empty())
      return next;
    
    var binding = (List) bindings.car();
    var symb = (Symbol) binding.car();
    var init = binding.cadr();
    
    return compile(init, scope, false,
                   new StoreLocal(0, offset,
                   bindLocalsSequential(bindings.cdr(), offset + 1, bindLocal(symb, scope),
                   next)));
  }
  
  private Scope bindLocal(Symbol symb, Scope scope) throws CompileError
  {
    if (!scope.bind(symb))
      throw multipleDef(symb);
    
    return scope;
  }
  
  private boolean checkLetBindings(List bindings)
  {
    if (bindings == List.Empty)
      return false;
    
    for (; bindings != List.Empty; bindings = bindings.cdr())
      if (bindings.car().type() != Type.List || !checkBinding((List) bindings.car()))
        return false;
    
    return true;
  }
  
  private boolean checkBinding(List binding)
  {
    return binding.length() == 2 && binding.car().type() == Type.Symbol;
  }
  
  private Scope extendLetScope(List bindings, Scope parent) throws MultipleDefinition
  {
    Scope scope = new Scope(parent);
    
    for (; bindings != List.Empty; bindings = bindings.cdr()) {
      var binding = (List) bindings.car();
      var symb = (Symbol) binding.car();
      if (!scope.bind(symb))
        throw multipleDef(symb);
    }
    
    return scope;
  }
  
  private Inst evalBindings(List bindings, Scope scope, Inst next) throws CompileError
  {
    if (bindings == List.Empty)
      return next;

    var binding = (List) bindings.car();
    
    return evalBindings(bindings.cdr(), scope,
           compile(binding.cadr(), scope, false,
           new PushArg(next)));
  }
  
  private Inst evalArgs(List args, Scope scope, Inst next) throws CompileError
  {
    if (args == List.Empty)
      return next;

    return evalArgs(args.cdr(), scope,
           compile(args.car(), scope, false,
           new PushArg(next)));
  }
  
  private Inst performBindings(int offset, int numBindings, Inst next)
  {
    if (offset >= numBindings)
      return next;

    return new PopArg(
           new StoreLocal(0, offset,
           performBindings(offset + 1, numBindings, next)));
  }
  
  /* Transforms the "defun" special form
  
       (defun f (params...) body...)
     
     into the equivalent
       
       (def f (fun (params...) body...))
  */
  
  private List transformDefun(List exp)
  {
    var symbol = exp.cadr();
    var params = exp.caddr();
    var body = exp.cdddr();
    
    var result = List.of(Symbol.get("def"), symbol, List.cons(Symbol.get("fun"), List.cons(params, body)));
    positionMap.put(result, positionMap.get(exp));
    return result;
  }
  
  /* Transforms the "let*" special form
  
       (let* ((symb1 init1)
              ...
              (symbN initN))
         body...)
     
     into the equivalent nested "let" form
     
       (let ((symb1 init1))
         ...
           (let ((symbN initN))
             body...))
  */
  
  private List transformLetStar(List bindings, List body)
  {
    return List.cons(Symbol.get("let"),
           List.cons(List.of(bindings.car()),
           bindings.cdr() == List.Empty ? body
                                        : List.of(transformLetStar(bindings.cdr(), body))));
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
     
       Syntax: (fun (param1 ... paramN) body...)
    
       Compilation:
       
       push-env N
       pop-arg
       store-local 0 0
       ...
       pop-arg
       store-local 0 N-1
       <<body>>
       pop-frame
     
     Case 2: Variadic
     
       Syntax: (fun (param1 ... paramN-1 &paramN) body...)
    
       Compilation:
       
       push-env N
       pop-arg
       store-local 0 0
       ...
       pop-arg
       store-local 0 N-2
       pop-rest-args
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
    
    if (!checkParams(params))
      throw malformedExp(exp);
    
    var numParams = params.length();
    var isVariadic = numParams != 0 && isRestParam((Symbol) params.at(numParams - 1));
    var requiredArgs = isVariadic ? numParams - 1 : numParams;
    
    var code = !isVariadic ? new PushEnv(numParams,
                             performBindings(0, numParams,
                             compileSequence(body, extendFunScope(params, scope), true,
                             new PopFrame())))
                           
                           : new PushEnv(numParams,
                             performBindings(0, numParams - 1,                             
                             new PopRestArgs(
                             new StoreLocal(0, numParams - 1,
                             compileSequence(body, extendFunScope(params, scope), true,
                             new PopFrame())))));
   
    return new MakeClosure(code, requiredArgs, isVariadic, next);
  }
  
  private boolean isRestParam(Symbol s)
  {
    return s.repr().charAt(0) == '&';
  }
  
  private boolean checkParams(List params)
  {
    for (; params != List.Empty; params = params.cdr()) {
      if (params.car().type() != Type.Symbol)
        return false;
      if (isRestParam((Symbol) params.car()) && params.cdr() != List.Empty)
        return false;
    }
    return true;
  }
  
  private Scope extendFunScope(List params, Scope parent) throws MultipleDefinition
  {
    var scope = new Scope(parent);
    
    for (; params != List.Empty; params = params.cdr()) {
      var symb = (Symbol) params.car();
      if (!scope.bind(symb))
        throw multipleDef(symb);
    }
    
    return scope;
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
  
  /* (do exp...) */
  
  private Inst compileDo(List exp, Scope scope, boolean tail, Inst next) throws CompileError
  {
    if (exp.length() < 2)
      throw malformedExp(exp);
    
    return compileSequence(exp.cdr(), scope, tail, next);
  }
  
  /* Evaluates a list of expressions in sequence.
     Returns the value of the last expression.
     The last expression is in tail context.
     
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

    return compile(exp.car(), scope, (tail && !exp.cdr().empty()),
           compileSequence(exp.cdr(), scope, tail, next));
  }
  
  // (set! symbol init)
  
  private Inst compileSet(List exp, Scope scope, boolean tail, Inst next) throws CompileError
  {
    if (exp.length() != 3 || exp.cdr().car().type() != Type.Symbol)
      throw malformedExp(exp);
    
    var symb = (Symbol) exp.cadr();
    var init = exp.caddr();
    var index = (scope == null) ? null : scope.lookup(symb);
    
    return (index == null)
      ? compile(init, scope, false,
        new StoreGlobal(symb, next))
      : compile(init, scope, false,
        new StoreLocal(index.depth, index.offset, next));
  }
  
  /* (while predicate body...)
  
     loop:     <<predicate>>
               branch continue done
     continue: <<body>>
               jump loop
     done:     ...
           
  */
  
  private Inst compileWhile(List exp, Scope scope, boolean tail, Inst next) throws CompileError
  {
    if (exp.length() < 3)
      throw malformedExp(exp);
    
    var jump = new Jump();
    var loop = compile(exp.cadr(), scope, false,
               new Branch(
                 compileSequence(exp.cddr(), scope, tail, jump),
                 next));
    jump.setTarget(loop);
    return loop;
  }
  
  /* (delay exp...)
          
     (let ((thunk (fn () exp...))
           (value-ready? false)
           (cached-value nil))
       (fn () (if (not value-ready?)
                (do (set! cached-value (thunk))
                    (set! value-ready? true))
              cached-value))
     
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
  
  // (defmacro f (param...) body...)
  
  private Inst compileDefMacro(List exp, Scope scope, Inst next) throws CompileError
  {
    if (exp.length() < 4 || exp.cadr().type() != Type.Symbol || exp.caddr().type() != Type.List)
      throw malformedExp(exp);
    
    var symb = (Symbol) exp.cadr();
    var macro = makeMacro(exp);
    macros.put(symb, macro);
    return new LoadConst(Nil.Instance, next);
  }
  
  private Macro makeMacro(List exp) throws CompileError
  {
    /*
    var params = (List) exp.caddr();
    var body = exp.cdddr();
    
    if (!checkParams(params))
      throw malformedExp(exp);
    
    var numParams = params.length();
    var isVariadic = numParams != 0 && isRestParam((Symbol) params.at(numParams - 1));
    var requiredArgs = isVariadic ? numParams - 1 : numParams;
    
    var code = isVariadic  ? new PushEnv(numParams,
                             compilePopArgsVariadic(0, numParams,
                             compileSequence(body, extendFunScope(params, null), true,
                             new PopFrame())))
                           
                           : new PushEnv(numParams,
                             compilePopArgs(0, numParams,
                             compileSequence(body, extendFunScope(params, null), true,
                             new PopFrame())));
   
    return new Macro(code, requiredArgs, isVariadic);
    */
    return null;
  }
  
  private Inst compileApplyMacro(List exp, Scope scope, boolean tail, Inst next) throws CompileError
  {
    var macro = macros.get(exp.car());
    var args = exp.cdr();
    Datum expansion = null;
    
    try {
      expansion = macro.apply(vm, args);
    }
    catch (WrongNumberArgs | RuntimeError err) {
      throw new CompileError(err.getMessage(), positionMap.get(exp));
    }

    System.out.println("macro expansion: " + expansion.repr());
    return compile(expansion, scope, tail, next);
  }
  
  private Inst compileList(List exp, Scope scope, boolean tail, Inst next) throws CompileError
  {
    if (exp.car() == Symbol.get("if"))
      return compileIf(exp, scope, tail, next);
    else if (exp.car() == Symbol.get("let"))
      return compileLet(exp, scope, tail, next);
    else if (exp.car() == Symbol.get("let*"))
      return compileLetStar(exp, scope, tail, next);
    else if (exp.car() == Symbol.get("letrec"))
      return compileLetRec(exp, scope, tail, next);
    else if (exp.car() == Symbol.get("def"))
      return compileDef(exp, scope, next);
    else if (exp.car() == Symbol.get("defun"))
      return compileDefun(exp, scope, next);
    else if (exp.car() == Symbol.get("defmacro"))
      return compileDefMacro(exp, scope, next);
    else if (exp.car() == Symbol.get("fun"))
      return compileFun(exp, scope, next);
    else if (exp.car() == Symbol.get("quote"))
      return compileQuote(exp, next);
    else if (exp.car() == Symbol.get("quasiquote"))
      return compileQuasiquote(exp, scope, next);
    else if (exp.car() == Symbol.get("or"))
      return compileOr(exp, scope, tail, next);
    else if (exp.car() == Symbol.get("and"))
      return compileAnd(exp, scope, tail, next);
    else if (exp.car() == Symbol.get("cond"))
      return compileCond(exp, scope, tail, next);
    else if (exp.car() == Symbol.get("do"))
      return compileDo(exp, scope, tail, next);
    else if (exp.car() == Symbol.get("set!"))
      return compileSet(exp, scope, tail, next);
    else if (exp.car() == Symbol.get("while"))
      return compileWhile(exp, scope, tail, next);
    else if (exp.car() == Symbol.get("delay"))
      return compileDelay(exp, scope, tail, next);
    else if (macros.containsKey(exp.car()))
      return compileApplyMacro(exp, scope, tail, next);
    else
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
  private final java.util.Map<Symbol, Macro> macros = new java.util.IdentityHashMap<>();
  private final VirtualMachine vm = new VirtualMachine(new BaseEnv());
}
