package spartan.compiling;

import spartan.runtime.*;
import spartan.data.*;
import spartan.parsing.SourceDatum;
import spartan.parsing.PositionMap;
import spartan.parsing.Position;
import spartan.errors.Error;
import spartan.errors.SyntaxError;
import spartan.errors.WrongNumberArgs;
import spartan.runtime.VirtualMachine;
import spartan.Config;
import java.util.logging.Logger;

/**
 * Compiles source expressions into executable bytecodes.
 */
public class Compiler
{
  /**
   * @param vm the {@code VirtualMachine} used for macro expansion.
   */
  public Compiler(VirtualMachine vm)
  {
    this.vm = vm;
  }
  
  /** Compile a source expression.
   *
   * @param exp the expression to compile
   * @return the compiled bytecode (an instruction graph)
   * @throws SyntaxError if the given exp is not syntactically valid
   */
  public Inst compile(SourceDatum exp)
  {
    positionMap = exp.positionMap();
    return compile(exp.datum(), Scope.EMPTY, false, null);
  }

  /** Convenience method for creating instances of SyntaxError for malformed expressions. */
  private SyntaxError malformedExp(Datum exp)
  {
    return new SyntaxError("malformed expression", positionMap.get(exp));
  }  
  
  /**
   * Determines if an expression is self-evaluating.
   *
   * @param exp an expression
   * @return {@code true} if the given expression is self-evaluating
   */
  private static boolean isSelfEval(Datum exp)
  {
    return exp instanceof INum
        || exp instanceof Bool
        || exp instanceof Text
        || exp == List.EMPTY
        || exp == Nil.VALUE;
  }
  
  /* Compile a self-evaluating expression. */
  
  private Inst compileSelfEval(Datum exp, Inst next)
  {
    return new LoadConst(exp, next);
  }

  /**
   * Compile a variable reference (local or global).
   * 
   * Syntax: an unquoted symbol
   *  
   * If the symbol is a bound local variable, look up its DeBruijn index (a 
   * depth, offset pair), and generate a load-local instruction.
   *
   * Otherwise, assume the symbol is a global variable, and generate a load-global instruction.
   */
  private Inst compileVarRef(Symbol s, Scope scope, Inst next)
  {
    return scope.lookup(s)
                .map(i -> compileLocalVarRef(i, next))
                .orElseGet(() -> compileGlobalVarRef(s, next));
  }
  
  private Inst compileLocalVarRef(DeBruijnIndex i, Inst next)
  {
    if (i.depth() == 0)
      return new LoadLocal0(i.offset(), next);
    else
      return new LoadLocal(i.depth(), i.offset(), next);
  }
  
  private Inst compileGlobalVarRef(Symbol s, Inst next)
  {
    return new LoadGlobal(s.intern(), positionMap.get(s), next);
  }
  
  /* Compile a variable assignment.

     Syntax: (set! symbol init)

     If the symbol is a bound local variable, look up its DeBruijn index (a depth, offset
     pair), and generate a store-local(depth, offset) instruction.

     Otherwise, assume the symbol is a global variable, and generate a store-global instruction.
  */

  private Inst compileSetVar(List exp, Scope scope, Inst next)
  {
    if (!(exp.length() == 3 && exp.cadr() instanceof Symbol s))
      throw malformedExp(exp);
    var init = exp.caddr();
    return compile(init, scope, false,
           scope.lookup(s)
                .map(i -> compileSetLocalVar(i, next))
                .orElseGet(() -> compileSetGlobalVar(s, next)));
  }
  
  private Inst compileSetLocalVar(DeBruijnIndex i, Inst next)
  {
    if (i.depth() == 0)
      return new StoreLocal0(i.offset(),
             new LoadConst(Nil.VALUE, next));
    else
      return new StoreLocal(i.depth(), i.offset(),
             new LoadConst(Nil.VALUE, next));
  }
  
  private Inst compileSetGlobalVar(Symbol s, Inst next)
  {
    return new StoreGlobal(s.intern(), new LoadConst(Nil.VALUE, next));
  }
  
  /* Compile a "def" special form.
  
     Syntax: (def symb init)

     Compilation:

     <<init>>
     store-global symb
     load-const nil
     next: ...
  */

  private Inst compileDef(List exp, Scope scope, Inst next)
  {
    if (!(exp.length() == 3 && exp.cadr() instanceof Symbol s))
      throw malformedExp(exp);
    var init = exp.caddr();
    return compile(init, scope, false,
           new StoreGlobal(s.intern(),
           new LoadConst(Nil.VALUE, next)));
  }

  /* Compile "defun" special form
  
     Syntax: (defun symbol (params...) body...)
  */
  
  private Inst compileDefun(List exp, Scope scope, Inst next)
  {
    if (exp.length() < 4)
      throw malformedExp(exp);
    
    var xform = transformDefun(exp);
    
    if (Config.LOG_DEBUG)
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
    return List.of(Symbol.DEF, symb, List.cons(Symbol.FUN, List.cons(params, body)));
  }

  /* Compile the "if" special form.

     Syntax (2-branch form): (if pred sub alt)        

     Compilation:

           <<pred>>
           branch sub alt
     sub:  <<sub>>
           jump next
     alt:  <<alt>>
     next: ...
     
     Syntax (1-branch form): (if pred sub)
     
     Translation: (if pred sub nil)
  */
  private Inst compileIf(List exp, Scope scope, boolean tail, Inst next)
  {
    int length = exp.length();

    if (length < 3 || length > 4)
      throw malformedExp(exp);

    var pred = exp.cadr();
    var sub = exp.caddr();
    var alt = length == 4 ? exp.cadddr() : Nil.VALUE;

    return compile(pred, scope, false,
           new Branch(compile(sub, scope, tail, next),
                      compile(alt, scope, tail, next)));
  }

  /* Compiles the "cond" special form.

     Syntax: (cond (pred1 body1)
                   ...
                   (predN bodyN)
                   (else  default))

     Compilation:

     pred1:  <<pred1>>
             branch body1 pred2
     body1:  <<body1>>
             jump next
             ...
     predN:  <<predN>>
             branch bodyN none
     bodyN:  <<bodyN>>
             jump next
     default:<<default>>
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
    if (clauses.empty())
      return new LoadConst(Nil.VALUE, next);

    var clause = (List) clauses.car();
    var test = clause.car();
    var body = clause.cdr();

    if (Symbol.ELSE.equals(test))
      return compileSequence(body, scope, tail, next);

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
    if (exp.length() < 3 || !(exp.cadr() instanceof List bindings) || !checkBindingListForm(bindings))
      throw malformedExp(exp);

    var body = exp.cddr();

    int numBindings = bindings.length();
    var vars = extractVars(bindings);
    var inits = extractInits(bindings);
    var extendedScope = scope.extend(vars);

    return compilePushArgs(inits, scope,
           new PushEnv(numBindings,
           compileBindLocals(0, numBindings,
           compileSequence(body, extendedScope, tail,
           new PopEnv(next)))));
  }
  
  /* (rec f bindings body)

     is essentially Scheme's "named let" and is equivalent to

     (letrec ((f (fun vars body)))
       (f inits))
  */
  private Inst compileRec(List exp, Scope scope, boolean tail, Inst next)
  {
    if (exp.length() < 4 || !(exp.cadr() instanceof Symbol f) || !(exp.caddr() instanceof List bindings))
      throw malformedExp(exp);
    
    var xform = transformRec(f, bindings, exp.cdddr());
    
    if (Config.LOG_DEBUG)
      log.info(() -> String.format("rec transform: %s => %s", exp.repr(), xform.repr()));
    
    try {
      return compile(xform, scope, tail, next);
    }
    catch (Error err) {
      err.setPosition(positionMap.get(exp));
      throw err;
    }
  }
  
  /* (letrec ((f (fun vars body))) (f inits))
  */
  private List transformRec(Symbol f, List bindings, List body)
  {
    var vars = extractVars(bindings);
    var inits = extractInits(bindings);
    return List.of(Symbol.LETREC, List.cons(List.of(f, List.cons(Symbol.FUN, List.cons(vars, body))), List.EMPTY), List.cons(f, inits));
  }
  
  /* Compiles the "letrec" special form

     Syntax:  (letrec ((symb1 init1)
                       ...
                       (symbN initN))
                body...)

     Compilation:

     push-env N                ; Extend local environment
     load-const nil            ; Initialize bindings to NIL value
     store-local 0 0
     ...
     store-local 0 N-1
     <<init1>>                 ; Evaluate initializer expression
     store-local 0 0           
     ...
     <<initN>>
     store-local 0 N-1
     <<body>>                  ; Evaluate body in extended environment
     pop-env                   ; Discard local environment
  */
  private Inst compileLetRec(List exp, Scope scope, boolean tail, Inst next)
  {
    if (exp.length() < 3 || !(exp.cadr() instanceof List bindings) || !checkBindingListForm(bindings))
      throw malformedExp(exp);

    var body = exp.cddr();

    int numBindings = bindings.length();
    var vars = extractVars(bindings);
    var inits = extractInits(bindings);
    var extendedScope = scope.extend(vars);

    return new PushEnv(numBindings,
           new LoadConst(Nil.VALUE,
           compileInitRecEnv(0, numBindings,
           compileRecursiveBindings(inits, 0, extendedScope,
           compileSequence(body, extendedScope, tail,
           new PopEnv(next))))));
  }
  
  /* Compile the binding sequence of a letrec expression */
  
  private Inst compileRecursiveBindings(List inits, int offset, Scope scope, Inst next)
  {
    if (inits.empty())
      return next;

    return compile(inits.car(), scope, false,
           new StoreLocal0(offset,
           compileRecursiveBindings(inits.cdr(), offset + 1, scope, next)));
  }
  
  /* Compile the initiation sequence of a letrec expression. */
  
  private static Inst compileInitRecEnv(int offset, int numBindings, Inst next)
  {
    if (offset >= numBindings)
      return next;
    else
      return new StoreLocal0(offset,
             compileInitRecEnv(offset + 1, numBindings, next));
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
    if (exp.length() < 3 || !(exp.cadr() instanceof List bindings) || !checkBindingListForm(bindings))
      throw malformedExp(exp);

    var body = exp.cddr();

    int numBindings = bindings.length();
    var vars = extractVars(bindings);
    var extendedScope = scope.extend(vars);

    return new PushEnv(numBindings,
           compileSequentialBindings(bindings, 0, scope.extend(),
           compileSequence(body, extendedScope, tail,
           new PopEnv(next))));
  }

  private Inst compileSequentialBindings(List bindings, int offset, Scope scope, Inst next)
  {
    if (bindings.empty())
      return next;

    var binding = (List) bindings.car();
    var symb = (Symbol) binding.car();
    var init = binding.cadr();

    return compile(init, scope, false,
                   new StoreLocal0(offset,
                   compileSequentialBindings(bindings.cdr(), offset + 1, scope.bind(symb),
                   next)));
  }
  
  private boolean checkBindingListForm(List bindings)
  {
    for (var binding : bindings)
      if (!(binding instanceof List pair && pair.length() == 2 && pair.car() instanceof Symbol))
        return false;
    return true;
  }
    
  private boolean checkParamListForm(List params)
  {
    for (; !params.empty(); params = params.cdr())
      if (!(params.car() instanceof Symbol param) || (param.equals(Symbol.AMPERSAND) && (params.cdr().empty() || !params.cddr().empty())))
        return false;
    return true;
  }

  private boolean checkClauseListForm(List clauses)
  {
    for (; !clauses.empty(); clauses = clauses.cdr())
      if (!(clauses.car() instanceof List clause) || clause.length() < 2 || (Symbol.ELSE.equals(clause.car()) && !clauses.cdr().empty()))
        return false;
    return true;
  }

  // Extract the variables from a binding list
  private List extractVars(List bindings)
  {
    return bindings.map(pair -> ((List)pair).car());
  }

  // Extract the initializers from a binding list
  private List extractInits(List bindings)
  {
    return bindings.map(pair -> ((List)pair).cadr());
  }
  
  private Inst compilePushArgs(List args, Scope scope, Inst next)
  {
    if (args.empty())
      return next;

    return compilePushArgs(args.cdr(), scope,
           compile(args.car(), scope, false,
           new PushArg(next)));
  }

  private Inst compileBindLocals(int offset, int numBindings, Inst next)
  {
    if (offset >= numBindings)
      return next;

    return new PopArg(
           new StoreLocal0(offset,
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
    
    return tail ? compilePushArgs(exp.cdr(), scope,
                  compile(exp.car(), scope, false,
                  new Apply(numArgs, positionMap.get(exp))))
    
                : new PushFrame(next, positionMap.get(exp),
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
    
    return tail ? new MakeCont(
                  new PushArg(
                  compile(exp.cadr(), scope, tail,           
                  new Apply(1, positionMap.get(exp.cadr())))))
    
                : new PushFrame(next, positionMap.get(exp),
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
    if (exp.empty())
      return next;

    return compile(exp.car(), scope, (tail && exp.cdr().empty()),
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
      if (Config.LOG_DEBUG)
        log.info(() -> String.format("inner defs transform: %s => %s", body.repr(), xform.repr()));
      return compile(xform, scope, true, next);
    }

    return compileSequence(body, scope, true, next);
  }
    
  /* Compiles a lambda expression (anonymous procedure)
   *
   * Syntax: (fun (args...) body...)
   *         (fun (args... & rest) body...)
   *
   */
  private Inst compileFun(List exp, Scope scope, Inst next)
  {
    if (exp.length() < 3 || !(exp.cadr() instanceof List params) || !checkParamListForm(params))
      throw malformedExp(exp);
    var body = exp.cddr();
    return new MakeClosure(makeProcedure(params, body, scope), next);
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

    while (!body.empty() && isInnerDef(body.car())) {
      var exp = (List) body.car();
      if (Symbol.DEFUN.equals(exp.car()))
        exp = transformDefun(exp);
      bindings.add(exp.cdr());
      body = body.cdr();
    }

    return List.cons(Symbol.LETREC, List.cons(bindings.build(), body));
  }

  /* Determine if an expression is an inner-define form, i.e. a list beginning
     with the symbol "def" or "defun".
  */
  private boolean isInnerDef(Datum exp)
  {
    return exp instanceof List form && !form.empty() && (Symbol.DEF.equals(form.car()) || Symbol.DEFUN.equals(form.car()));
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
    if (exp.empty())
      return next;

    return compile(exp.car(), scope, (tail && exp.cdr().empty()),
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
    if (exp.empty())
      return next;

    return compile(exp.car(), scope, (tail && exp.cdr().empty()),
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
               new Branch(compileSequence(body, scope, false, jump),
                          new LoadConst(Nil.VALUE, next)));
    jump.setTarget(loop);
    return loop;
  }
  
  // (quote x)

  private Inst compileQuote(List exp, Inst next)
  {
    if (exp.length() != 2)
      throw malformedExp(exp);
    
    var x = exp.cadr();
    
    if (x instanceof Symbol s)
      x = s.intern();
    
    return new LoadConst(x, next);
  }

  // Compile a (quasiquote x) form by reducing it to an equivalent
  // list form and compiling the result.

  private Inst compileQuasiquote(List exp, Scope scope, Inst next)
  {
    if (exp.length() != 2)
      throw malformedExp(exp);

    var xform = transformQuasiquote(exp.cadr());
    
    if (Config.LOG_DEBUG)
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
    // (quasiquote x) = x, for self-evaluating x
    
    if (exp == List.EMPTY)
      return List.EMPTY;
    
    if (!(exp instanceof List list))
      return List.of(Symbol.QUOTE, exp);
    
    // Check for the unquote and unquote-splicing forms
    
    if (list.car() instanceof List form && !form.empty())
    {      
      // (quasiquote (unquote x) xs...) => (cons x (quasiquote xs...))
      
      if (form.car() instanceof Symbol s && s.equals(Symbol.UNQUOTE)) {
        if (form.length() != 2)
          throw malformedExp(exp);
        return List.cons(new Symbol("cons"),
               List.cons(form.cadr(),
               List.cons(transformQuasiquote(list.cdr()),
               List.EMPTY)));
      }

      // (quasiquote (unquote-splicing x) xs...) => (concat x (quasiquote xs...))    
      
      if (form.car() instanceof Symbol s && s.equals(Symbol.UNQUOTE_SPLICING)) {
        if (form.length() != 2)
          throw malformedExp(exp);
        return List.cons(new Symbol("concat"),
               List.cons(form.cadr(),
               List.cons(transformQuasiquote(list.cdr()),
               List.EMPTY)));
      }
    }
    
    // General case: recursively transform the list
    
    // (quasiquote x xs...) => (cons (quasiquote x) (quasiquote xs...))

    return List.cons(new Symbol("cons"),
           List.cons(transformQuasiquote(list.car()),
           List.cons(transformQuasiquote(list.cdr()),
           List.EMPTY)));
  }
  
  /** Create a Procedure
   *
   * @param params the procedure parameters
   * @param body the procedure body
   * @param scope scope of procedure definition
   * @return 
   */  
  private Procedure makeProcedure(List params, List body, Scope scope)
  {
    var isVariadic = params.index(Symbol.AMPERSAND::equals) >= 0;
    var requiredArgs = isVariadic ? params.length() - 2 : params.length();
    if (isVariadic)
      params = params.remove(Symbol.AMPERSAND::equals);
    var extendedScope = scope.extend(params);
    return new Procedure(
      isVariadic ? compileVariadicProc(body, extendedScope, requiredArgs)
                 : compileFixedProc(body, extendedScope, requiredArgs),
      new Signature(requiredArgs, isVariadic));
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
               new StoreLocal0(requiredArgs,
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
    if (!(exp.length() >= 4 && exp.cadr() instanceof Symbol symb && exp.caddr() instanceof List params && checkParamListForm(params)))
      throw malformedExp(exp);
    var body = exp.cdddr();    
    MacroEnv.bind(symb, new Macro(makeProcedure(params, body, Scope.EMPTY)));
    return new LoadConst(Nil.VALUE, next);
  }
  
  /* Compile a macro application
  
     Syntax: (f args...)
     
       where f is a macro
     
     Applies the macro procedure to the list of (unevaluated) arguments,
     and compiles the code returned by the macro.
  */
  private Inst compileApplyMacro(List exp, Scope scope, boolean tail, Inst next)
  {
    var symb = (Symbol) exp.car();
    var args = exp.cdr();
    var macro = MacroEnv.lookup(symb).get();
    
    try {
      var xform = macro.apply(vm, args);
      if (Config.LOG_DEBUG)
        log.info(() -> String.format("macro transform: %s => %s", exp.repr(), xform.repr()));
      return compile(xform, scope, tail, next);
    }
    catch (Error err) {
      err.setPosition(positionMap.get(exp));
      throw err;
    }
  }
  
  /* Compile a combination (i.e., special forms, procedure application, and macro expansion. */
  
  private Inst compileCombo(List exp, Scope scope, boolean tail, Inst next)
  {
    // Handle special forms
    if (exp.car() instanceof Symbol s) {
      if (s.equals(Symbol.IF))
        return compileIf(exp, scope, tail, next);
      if (s.equals(Symbol.LET))
        return compileLet(exp, scope, tail, next);
      if (s.equals(Symbol.LETSTAR))
        return compileLetStar(exp, scope, tail, next);
      if (s.equals(Symbol.LETREC))
        return compileLetRec(exp, scope, tail, next);
      if (s.equals(Symbol.DO))
        return compileDo(exp, scope, tail, next);
      if (s.equals(Symbol.DEF))
        return compileDef(exp, scope, next);
      if (s.equals(Symbol.DEFUN))
        return compileDefun(exp, scope, next);
      if (s.equals(Symbol.DEFMACRO))
        return compileDefMacro(exp, scope, next);
      if (s.equals(Symbol.FUN))
        return compileFun(exp, scope, next);
      if (s.equals(Symbol.QUOTE))
        return compileQuote(exp, next);
      if (s.equals(Symbol.QUASIQUOTE))
        return compileQuasiquote(exp, scope, next);
      if (s.equals(Symbol.OR))
        return compileOr(exp, scope, tail, next);
      if (s.equals(Symbol.AND))
        return compileAnd(exp, scope, tail, next);
      if (s.equals(Symbol.COND))
        return compileCond(exp, scope, tail, next);      
      if (s.equals(Symbol.SET))
        return compileSetVar(exp, scope, next);
      if (s.equals(Symbol.WHILE))
        return compileWhile(exp, scope, tail, next);
      if (s.equals(Symbol.CALL_CC))
        return compileCallCC(exp, scope, tail, next);
      if (s.equals(Symbol.REC))
        return compileRec(exp, scope, tail, next);
      // Handle macro expansion
      if (MacroEnv.contains(s))
        return compileApplyMacro(exp, scope, tail, next);
    }
    
    // Handle procedure application
    return compileApply(exp, scope, tail, next);
  }

  /* This is the top-level compilation function that dispatches on the type of the expression. */
  
  private Inst compile(Datum exp, Scope scope, boolean tail, Inst next)
  {
    if (isSelfEval(exp))
      return compileSelfEval(exp, next);
    else if (exp instanceof Symbol s)
      return compileVarRef(s, scope, next);
    else 
      return compileCombo((List)exp, scope, tail, next);
  }

  private PositionMap positionMap;
  private final VirtualMachine vm;
  private static final Logger log = Logger.getLogger(Compiler.class.getName());
}
