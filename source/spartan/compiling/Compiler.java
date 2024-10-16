package spartan.compiling;

import spartan.runtime.*;
import spartan.data.*;
import spartan.data.Void; // shadows java.lang.Void
import spartan.parsing.SourceDatum;
import spartan.parsing.PositionMap;
import spartan.parsing.Position;
import spartan.errors.SourceInfo;
import spartan.errors.Error;
import spartan.errors.SyntaxError;
import spartan.errors.MultipleDefinition;
import spartan.errors.MatchFailure;
import spartan.errors.InvalidPattern;
import spartan.runtime.VirtualMachine;
import spartan.Config;
import java.util.logging.Logger;
import java.util.Optional;
import java.util.Map;
import static java.util.Map.entry;
import static spartan.Runtime.lookupMacro;

interface ISpecialForm
{
  Inst compile(List exp, Scope scope, boolean tail, Inst next);
}

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
    var code = compile(exp.datum(), Scope.EMPTY, false, new Halt());
    if (Config.LOG_DEBUG && Config.EMIT_BYTECODE)
      log.info(() -> String.format("Listing for expression at %s\n%s", positionOf(exp.datum()), CodeListing.generate(code)));
    return code;
  }

  /** Convenience method for creating instances of SyntaxError for malformed expressions. */
  private SyntaxError malformedExp(Datum exp)
  {
    return new SyntaxError("malformed expression", new SourceInfo(exp, positionOf(exp)));
  }
  
  private Position positionOf(Datum exp)
  {
    var pos = positionMap.get(exp);
    if (pos == null)
      log.warning(() -> String.format("The position is null for the expression %s", exp.repr()));
    return pos;
  }
  
  /**
   * Determines if an expression is self-evaluating.
   *
   * @param exp an expression
   * @return {@code true} if the given expression is self-evaluating
   */
  private static boolean isSelfEval(Datum exp)
  {
    return exp == List.EMPTY
        || exp == Void.VALUE
        || exp instanceof INum
        || exp instanceof Bool
        || exp instanceof Text
        || (exp instanceof Symbol s && s.isKeyword());
  }
  
  /* Compile a self-evaluating expression. */
  
  private Inst compileSelfEval(Datum exp, Inst next)
  {
    return new LoadConst(exp, next);
  }

  /**
   * Compile a variable reference (local or global).
   * 
   * Syntax: an unquoted <symbol>
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
    if (s instanceof QualifiedSymbol qs)
      return new LoadGlobal(Symbol.of(qs.packageName()), Symbol.of(qs.baseName()), new SourceInfo(qs, positionOf(qs)), next);
    else
      return new LoadGlobal(spartan.Runtime.currentPackage().name(), s.intern(), new SourceInfo(s, positionOf(s)), next);
  }
  
  
  /* Compile a variable assignment.

     Syntax: (set! symbol init)

     If the symbol is a bound local variable, look up its DeBruijn index (a depth, offset
     pair), and generate a store-local(depth, offset) instruction.

     Otherwise, assume the symbol is a global variable, and generate a store-global instruction.
  */
  private Inst compileSet(List exp, Scope scope, boolean tail, Inst next)
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
             new LoadConst(Void.VALUE, next));
    else
      return new StoreLocal(i.depth(), i.offset(),
             new LoadConst(Void.VALUE, next));
  }
  
  private Inst compileSetGlobalVar(Symbol s, Inst next)
  {
    if (s instanceof QualifiedSymbol qs)
      return new StoreGlobal(Symbol.of(qs.packageName()), Symbol.of(qs.baseName()), new SourceInfo(qs, positionOf(qs)),
             new LoadConst(Void.VALUE, next));
    else
      return new StoreGlobal(spartan.Runtime.currentPackage().name(), s.intern(), new SourceInfo(s, positionOf(s)),
             new LoadConst(Void.VALUE, next));
  }
  
  private Inst compileDef(List exp, Scope scope, boolean tail, Inst next)
  {
    if (!(exp.length() == 3 && exp.cadr() instanceof Symbol s && s.isSimple()))
      throw malformedExp(exp);
    var init = exp.caddr();  
    return compile(init, scope, false,
           new BindGlobal(spartan.Runtime.currentPackage().name(), s.intern(), new SourceInfo(exp, positionOf(s)),
           new LoadConst(Void.VALUE, next)));
  }
  
  /* Compile "defun" special form
  
     Syntax: (defun symbol (params...) body...)
  */
  
  private Inst compileDefun(List exp, Scope scope, boolean tail, Inst next)
  {
    if (exp.length() < 4)
      throw malformedExp(exp);
    
    var xform = transformDefun(exp);
    
    if (Config.LOG_DEBUG && Config.SHOW_MACRO_EXPANSION)
      log.info(() -> String.format("defun transform: %s => %s", exp.repr(), xform.repr()));
    
    try {
      return compile(xform, scope, false, next);
    }
    catch (Error err) {
      err.setSource(new SourceInfo(xform, positionOf(exp)));
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
           jf L
           <<sub>>
           j next
     L:    <<alt>>
     next: ...
     
     Syntax (1-branch form): (if pred sub)
     
     Translation: (if pred sub void)
  */
  private Inst compileIf(List exp, Scope scope, boolean tail, Inst next)
  {
    int length = exp.length();

    if (length < 3 || length > 4)
      throw malformedExp(exp);

    var pred = exp.cadr();
    var sub = exp.caddr();
    var alt = length == 4 ? exp.cadddr() : Void.VALUE;
        
    return compile(pred, scope, false,
           new BranchFalse(compile(sub, scope, tail, new Jump(next)),
                           compile(alt, scope, tail, next)));
  }

  /* Compiles the "cond" special form.
     
     Syntax: (cond (pred1 body1)
                   ...
                   (predN bodyN))
     
     Compilation:

     P1:  <<pred1>>
          jf P2
          <<body1>>
          j next
          ...
     PN:  <<predN>>
          jf none
             <<bodyN>>
             jump next
     none:   load-const nil
     next:   ...
     
     Syntax: (cond (pred1 body1)
                   ...
                   (predN bodyN)
                   (else  default))

     Compilation:

     pred1:    <<pred1>>
               branchf pred2
               <<body1>>
               jump next
               ...
     predN:    <<predN>>
               branchf default
               <<bodyN>>
               jump next
     default:  <<default>>
     next:     ...
  */
  private Inst compileCond(List exp, Scope scope, boolean tail, Inst next)
  {
    if (exp.length() < 2 || !checkClauseListForm(exp.cdr()))
      throw malformedExp(exp);

    return compileCondClauses(exp.cdr(), scope, tail, next);
  }

  private Inst compileCondClauses(List clauses, Scope scope, boolean tail, Inst next)
  {
    if (clauses.isEmpty())
      return new LoadConst(Void.VALUE, next);

    var clause = (List) clauses.car();
    var pred = clause.car();
    var body = clause.cdr();

    if (Symbol.ELSE.equals(pred))
      return compileSequence(body, scope, tail, next);

    return compile(pred, scope, false,
           new BranchFalse(compileSequence(body, scope, tail, new Jump(next)),
                           compileCondClauses(clauses.cdr(), scope, tail, next)));
  }

  /* Compiles the "let" special form

     Syntax:  (let ((symb1 init1)
                    ...
                    (symbN initN))
                body...)
     
     Equivalent to: ((fun (symb1 ... symbN) body...) init1 ... initN)
     
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
     next: ...
  */

  private Inst compileLet(List exp, Scope scope, boolean tail, Inst next)
  {
    if (!(exp.length() >= 3 && exp.cadr() instanceof List bindings && checkBindingListForm(bindings)))
      throw malformedExp(exp);

    var body = exp.cddr();
    int numBindings = bindings.length();
    var vars = extractFirst(bindings);
    var inits = extractSecond(bindings);
    var extendedScope = scope.extend(vars);

    return compilePushArgs(inits, scope,
           new PushEnv(numBindings,
           compileBindLocals(0, numBindings,
           compileSequence(body, extendedScope, tail,
           new PopEnv(next)))));
  }
  
  /* Compiles the "let*" special form, for sequential binding

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
     next: ...
  */
  private Inst compileLetStar(List exp, Scope scope, boolean tail, Inst next)
  {
    if (!(exp.length() >= 3 && exp.cadr() instanceof List bindings && checkBindingListForm(bindings)))
      throw malformedExp(exp);

    var body = exp.cddr();
    int numBindings = bindings.length();
    var vars = extractFirst(bindings);
    var extendedScope = scope.extend(vars);

    return new PushEnv(numBindings,
           compileSequentialBindings(bindings, 0, scope.extend(),
           compileSequence(body, extendedScope, tail,
           new PopEnv(next))));
  }
  
  /* Generate the binding sequence of a let* expression */
  
  private Inst compileSequentialBindings(List bindings, int offset, Scope scope, Inst next)
  {
    if (bindings.isEmpty())
      return next;

    var binding = (List) bindings.car();
    var symb = (Symbol) binding.car();
    var init = binding.cadr();

    return compile(init, scope, false,
                   new StoreLocal0(offset,
                   compileSequentialBindings(bindings.cdr(), offset + 1, scope.bind(symb),
                   next)));
  }
  
  /* Compiles the "letrec" special form, for recursive and mutally-recursive functions

     Syntax:  (letrec ((symb1 init1)
                       ...
                       (symbN initN))
                body...)

     Compilation:

           push-env N
           load-const nil
           store-local 0 0
           ...
           store-local 0 N-1
           <<init1>>
           store-local 0 0           
           ...
           <<initN>>
           store-local 0 N-1
           <<body>>
           pop-env
     next: ...
  */
  private Inst compileLetRec(List exp, Scope scope, boolean tail, Inst next)
  {
    if (!(exp.length() >= 3 && exp.cadr() instanceof List bindings && checkBindingListForm(bindings)))
      throw malformedExp(exp);

    var body = exp.cddr();
    int numBindings = bindings.length();
    var vars = extractFirst(bindings);
    var inits = extractSecond(bindings);
    var extendedScope = scope.extend(vars);

    return new PushEnv(numBindings,
           new LoadConst(Void.VALUE,
           compileInitRecEnv(0, numBindings,
           compileRecursiveBindings(inits, 0, extendedScope,
           compileSequence(body, extendedScope, tail,
           new PopEnv(next))))));
  }
  
  /* Generate the binding sequence of a letrec expression */
  
  private Inst compileRecursiveBindings(List inits, int offset, Scope scope, Inst next)
  {
    if (inits.isEmpty())
      return next;

    return compile(inits.car(), scope, false,
           new StoreLocal0(offset,
           compileRecursiveBindings(inits.cdr(), offset + 1, scope, next)));
  }
  
  /* Generate the initialization sequence of a letrec expression. */
  
  private static Inst compileInitRecEnv(int offset, int numBindings, Inst next)
  {
    if (offset >= numBindings)
      return next;
    else
      return new StoreLocal0(offset,
             compileInitRecEnv(offset + 1, numBindings, next));
  }
  
  /* Compile the "rec" special form, a shorthand for defining and invoking recursive procedures
  
     Syntax:     (rec f ((var1 init1) ... (varN initN)) body...)
      
     Transform:  (letrec ((f (fun (var1 ... varN) body..)))
                   (f init1 ... initN))
  */
  /* Compiles the "rec" special form

     Syntax:  (rec label ((var1 init1) ... (varN initN)) body...)
     
     Equivalent to: (letrec ((label (fun (var1 ... varN) body..)))
                      (label init1 ... initN))
     
     Compilation:

           <<initN>>
           push-arg
           ...
           <<init1>>
           push-arg
           push-env N
     tailEntry:
           pop-arg
           store-local 0 0
           ...
           pop-arg
           store-local 0 N-1
           <<body>>
           pop-env
     next: ...
     
     Within body, a tail call of the form (label arg1 ... argN) shall be compiled as:
     
           
  */
  private Inst compileRec(List exp, Scope scope, boolean tail, Inst next)
  {
    if (!(exp.length() >= 4 && exp.cadr() instanceof Symbol s && s.isSimple() && exp.caddr() instanceof List bindings && checkBindingListForm(bindings)))
      throw malformedExp(exp);
    
    var xform = transformRec(s, bindings, exp.cdddr());
    
    if (Config.LOG_DEBUG && Config.SHOW_MACRO_EXPANSION)
      log.info(() -> String.format("rec transform: %s => %s", exp.repr(), xform.repr()));
    
    try {
      return compile(xform, scope, tail, next);
    }
    catch (Error err) {
      err.setSource(new SourceInfo(xform, positionOf(exp)));
      throw err;
    }
  }
  
  private List transformRec(Symbol f, List bindings, List body)
  {
    var vars = extractFirst(bindings);
    var inits = extractSecond(bindings);
    return List.of(Symbol.LETREC, List.cons(List.of(f, List.cons(Symbol.FUN, List.cons(vars, body))), List.EMPTY), List.cons(f, inits));
  }
  
  /* Check that a binding list for a "let" expression is well-formed */
  
  private boolean checkBindingListForm(List bindings)
  {
    for (; !bindings.isEmpty(); bindings = bindings.cdr())
      if (!(bindings.car() instanceof List list && list.length() == 2 && list.car() instanceof Symbol s && s.isSimple()))
        return false;
    return true;
  }
  
  /* Check that a binding list for a "do" expression is well-formed */
  
  private boolean checkForBindingsForm(List bindings)
  {
    for (; !bindings.isEmpty(); bindings = bindings.cdr())
      if (!(bindings.car() instanceof List list && list.length() == 3 && list.car() instanceof Symbol s && s.isSimple()))
        return false;
    return true;
  }
  
  /* Check that a parameter list is well-formed
   * 
   * <parameter-list> => "(" <symbol>* ("&" <symbol>)? ")"
   */
  private boolean checkParamListForm(List params)
  {
    for (; !params.isEmpty(); params = params.cdr())
      if (!(params.car() instanceof Symbol param && param.isSimple()) || (param.equals(Symbol.AMPERSAND) && (params.cdr().isEmpty() || !params.cddr().isEmpty())))
        return false;
    return true;
  }

  /* Check that a typed parameter list is well-formed
   * 
   * <typed-parameter-list> => "(" <typed-parameter>* <rest-parameter>? ")"
   * <typed-parameter> => "(" <symbol> <symbol> ")"
   * <rest-parameter> => "&" <symbol>
   */
  private boolean checkTypedParamListForm(List params)
  {
    for (; !params.isEmpty(); params = params.cdr())
      if (!(params.car() instanceof List param && checkTypedParamForm(param)) || (Symbol.AMPERSAND.equals(param) && (params.cdr().isEmpty() || !params.cddr().isEmpty())))
        return false;
    return true;
  }
  
  private boolean checkTypedParamForm(List param)
  {
    return param.length() == 2 && param.car() instanceof Symbol name && name.isSimple() && param.cadr() instanceof Symbol type && type.isSimple();
  }
  
  /* Check that a clause list is well-formed
   * 
   * <clause-list> => "(" <clause>+ <else-clause>? ")"
   * <clause> => "(" <expr> <expr>+ ")"
   * <else-clause> => "(" "else" <expr>+ ")"
   */
  private boolean checkClauseListForm(List clauses)
  {
    for (; !clauses.isEmpty(); clauses = clauses.cdr())
      if (!(clauses.car() instanceof List clause) || clause.length() < 2 || (Symbol.ELSE.equals(clause.car()) && !clauses.cdr().isEmpty()))
        return false;
    return true;
  }

  // Extract the first sub-element from each element in a list of lists
  private List extractFirst(List bindings)
  {
    return bindings.map(list -> ((List)list).car());
  }

  // Extract the second sub-element from each element in a list of lists
  private List extractSecond(List bindings)
  {
    return bindings.map(list -> ((List)list).cadr());
  }
  
  // Extract the third sub-element from each element in a list of lists
  private List extractThird(List bindings)
  {
    return bindings.map(list -> ((List)list).caddr());
  }
  
  /* Generate the argument-evaluation sequence of a procedure application */
  
  private Inst compilePushArgs(List args, Scope scope, Inst next)
  {
    if (args.isEmpty())
      return next;

    return compilePushArgs(args.cdr(), scope,
           compile(args.car(), scope, false,
           new PushArg(next)));
  }
  
  /* Generate the parameter binding sequence of a procedure application */
  
  private Inst compileBindLocals(int offset, int numBindings, Inst next)
  {
    if (offset >= numBindings)
      return next;

    return new PopArg(
           new StoreLocal0(offset,
           compileBindLocals(offset + 1, numBindings, next)));
  }
  
  /* Compiles a procedure application.

     Syntax: (proc arg1 arg2 ... argN)

     Compilation:

           push-frame  // Omitted if this expression occurs in tail context
           <<argN>>
           push-arg
           ...
           <<arg1>>
           push-arg
           <<proc>>
           apply
     next: ...
  */

  private Inst compileApply(List exp, Scope scope, boolean tail, Inst next)
  {
    var proc = exp.car();
    var args = exp.cdr();
    int numArgs = args.length();
    var position = positionOf(exp);
    var source = new SourceInfo(exp, position);
    
    if (tail && Config.TAIL_CALLS) // Tail call optimization: omit call frame, return directly to the caller
      return compilePushArgs(args, scope,
             compile(proc, scope, false,
             new Apply(numArgs, source,
             next)));
    else
      return new PushFrame(next, position,
             compilePushArgs(args, scope,
             compile(proc, scope, false,
             new Apply(numArgs, source,
             next))));
  }

  /* Compile the "call/cc" form, or "call-with-current-continuation"
  
     Syntax: (call/cc proc)
     
     Compilation:
     
           push-frame  // Omitted if this expression occurs in tail context
           make-cont
           push-arg
           <<proc>>    
           apply
     next: ...
    
  private Inst compileCallCC(List exp, Scope scope, boolean tail, Inst next)
  {
    if (exp.length() != 2)
      throw malformedExp(exp);
    
    var proc = exp.cadr();
    var position = positionOf(exp);
    
    // Optimization: omit frame for call in tail position
    
    if (tail) // Tail call optimization: omit call frame, return directly to the caller
      return new MakeCont(
             new PushArg(
             compile(proc, scope, tail,           
             new Apply(1, position))));
    else
      return new PushFrame(next, position,
             new MakeCont(
             new PushArg(
             compile(proc, scope, tail,           
             new Apply(1, position)))));
  }
  */
  
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
    if (exp.isEmpty())
      return next;

    return compile(exp.car(), scope, (tail && exp.cdr().isEmpty()),
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
      if (Config.LOG_DEBUG && Config.SHOW_MACRO_EXPANSION)
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
  private Inst compileFun(List exp, Scope scope, boolean tail, Inst next)
  {
    if (!(exp.length() >= 3 && exp.cadr() instanceof List params && checkParamListForm(params)))
      throw malformedExp(exp);
    var body = exp.cddr();
    var proc = makeProcedure(params, body, scope);    
    //if (Config.LOG_DEBUG)
      //log.info(() -> String.format("Listing for procedure defined at %s\n%s", positionOf(exp), CodeListing.generate(proc.body())));
    return new MakeClosure(proc, next);
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

    while (!body.isEmpty() && isInnerDef(body.car())) {
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
    return exp instanceof List form && !form.isEmpty() && (Symbol.DEF.equals(form.car()) || Symbol.DEFUN.equals(form.car()));
  }

  /* Compiles the "or" special form, a logical disjunction.

     Syntax: (or exp1 ... expN)

     Compilation:

             <<exp1>>
             branchf L1
             jump next
     L1:     <<exp2>>           
             branchf LN
             jump next
     ...
     LN:     <<expN>>
             branchf next
             jump next
     next:   ...
  */

  private Inst compileOr(List exp, Scope scope, boolean tail, Inst next)
  {
    if (exp.length() < 2)
      throw malformedExp(exp);

    return compileDisjunction(exp.cdr(), scope, tail, next);
  }

  private Inst compileDisjunction(List exp, Scope scope, boolean tail, Inst next)
  {
    if (exp.isEmpty())
      return next;

    return compile(exp.car(), scope, (tail && exp.cdr().isEmpty()),
           new BranchFalse(new Jump(next),
                           compileDisjunction(exp.cdr(), scope, tail, next)));
  }

  /* Compiles the "and" special form, a logical conjunction.

     Syntax: (and exp1 ... expN)

     Compilation:

             <<exp1>>
             brancht L1
             jump next
     L1:     <<exp2>>           
             brancht LN
             jump next
     ...
     LN:     <<expN>>
             brancht next
             jump next
     next:   ...
  */

  private Inst compileAnd(List exp, Scope scope, boolean tail, Inst next)
  {
    if (exp.length() < 2)
      throw malformedExp(exp);

    return compileConjuction(exp.cdr(), scope, tail, next);
  }

  private Inst compileConjuction(List exp, Scope scope, boolean tail, Inst next)
  {
    if (exp.isEmpty())
      return next;

    return compile(exp.car(), scope, (tail && exp.cdr().isEmpty()),
           new BranchTrue(new Jump(next),
                          compileConjuction(exp.cdr(), scope, tail, next)));
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

     L0: <<pred>>
         jf L1
         <<body>>
         j L0
     L1: ldc nil
     next: ...
  */

  private Inst compileWhile(List exp, Scope scope, boolean tail, Inst next)
  {
    if (exp.length() < 3)
      throw malformedExp(exp);

    var pred = exp.cadr();
    var body = exp.cddr();
    var jump = new Jump();
    var loop = compile(pred, scope, false,
               new BranchFalse(compileSequence(body, scope, false, jump),
                               new LoadConst(Void.VALUE, next)));
    jump.setTarget(loop);
    return loop;
  }
  
  /* Compile the "for" special form.
   
     Syntax: (for ((var1 init1 step1) ...)
               (test result)
               body ...)
     
     Compilation:
     
           ; pre-iteration initialization sequence

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
            
           ; iteration test and body

     loop: <<test>>
           brancht done

           ; evaluate loop body for side-effect

     body: <<body>>
           
           ; update step

           <<stepN>>
           push-arg
           ...
           <<step1>>
           push-arg
           pop-arg
           store-local 0 0
           ...
           pop-arg
           store-local 0 N-1
           
           jump loop
           
     done: <<result>>
           pop-env
  */
  private Inst compileFor(List exp, Scope scope, boolean tail, Inst next)
  {
    if (!(exp.length() >= 3 && exp.cadr() instanceof List bindings && checkForBindingsForm(bindings)
          && exp.caddr() instanceof List caddr && (caddr.length() == 1 || caddr.length() == 2)))
      throw malformedExp(exp);
    
    int numBindings = bindings.length();
    var vars = extractFirst(bindings);
    var inits = extractSecond(bindings);
    var steps = extractThird(bindings);
    var extendedScope = scope.extend(vars);
    var test = caddr.car();
    var result = caddr.length() == 2 ? caddr.cadr() : Void.VALUE;
    var body = exp.cdddr();
    
    //System.out.println("for vars = " + vars.repr());
    //System.out.println("for inits = " + inits.repr());
    //System.out.println("for steps = " + steps.repr());
    //System.out.println("for caddr = " + caddr.repr());
    
    var jump = new Jump();
    var loop = compile(test, extendedScope, false,
               new BranchTrue(
                          compileSequence(body, extendedScope, false,
                          compilePushArgs(steps, extendedScope,
                          compileBindLocals(0, numBindings,                          
                          jump))),
                          compile(result, extendedScope, false,
                          new PopEnv(next))));
    jump.setTarget(loop);                          
    return compilePushArgs(inits, scope,
           new PushEnv(numBindings,
           compileBindLocals(0, numBindings,
           loop)));
  }
  
  // (quote x)

  private Inst compileQuote(List exp, Scope scope, boolean tail, Inst next)
  {
    if (exp.length() != 2)
      throw malformedExp(exp);
    
    if (exp.cadr() instanceof Symbol s)
      return new LoadConst(s.intern(), next);
    else if (exp.cadr() instanceof List list)
      return new LoadConst(quoteList(list), next);
    else
      return new LoadConst(exp.cadr(), next);
  }

  private List quoteList(List list)
  {
    return list.map(
      x -> switch (x) {
        case Symbol s -> s.intern();
        case List l -> quoteList(l);
        default -> x;
      });
  }
  
  // Compile a (quasiquote x) form by reducing it to an equivalent
  // list form and compiling the result.

  private Inst compileQuasiquote(List exp, Scope scope, boolean tail, Inst next)
  {
    if (exp.length() != 2)
      throw malformedExp(exp);

    var xform = transformQuasiquote(exp.cadr(), 0);
    
    if (Config.LOG_DEBUG && Config.SHOW_MACRO_EXPANSION)
      log.info(() -> String.format("quasiquote transform: %s => %s", exp.repr(), xform.repr()));
    
    try {
      return compile(xform, scope, false, next);
    }
    catch (Error err) {
      err.setSource(new SourceInfo(exp, positionOf(exp)));
      throw err;
    }
  }

  // Reduce a quasiquote form (quasiquote x) to equivalent list form.

/*
  private List transformQuasiquote(Datum exp, int level)
  {
    if (!(exp instanceof List list)) {
      return List.of(Symbol.QUOTE, exp);
    }
    else if (list.car() instanceof Symbol s && s.equals(Symbol.QUASIQUOTE)) {
      return transformQuasiquote(list.cadr(), level + 1);
    }
    else if (list.car() instanceof Symbol s && s.equals(Symbol.UNQUOTE)) {
      if (level == 0)
        return list.cdr();
      return transformQuasiquote(list.cadr(), level - 1);
    }
    else if (list.car() instanceof List inner && inner.car() instanceof Symbol s && s.equals(Symbol.UNQUOTE_SPLICING)) {
      if (level == 0)
        return List.cons(Symbol.Q_CONCAT,
               List.cons(inner.cadr(),
               List.cons(transformQuasiquote(list.cdr(), level),
               List.EMPTY)));
      return List.cons(Symbol.Q_CONCAT,
             List.cons(transformQuasiquote(inner.cadr(), level - 1),
             List.cons(transformQuasiquote(list.cdr(), level),
             List.EMPTY)));
    }
    else {
      return List.cons(Symbol.Q_CONS,
             List.cons(transformQuasiquote(list.car(), level),
             List.cons(transformQuasiquote(list.cdr(), level),
             List.EMPTY)));
    }
  }
*/

  /**
   * Transform a quasiquoted expression
   * 
   * Let QQ(exp, N) denote the quasiquote transformation of exp with nesting
   * level N. Then QQ is defined as follows:
   *
   * QQ(x, N) => (quote x), for non-list value x
   * QQ(((unquote x) xs...), 0) => (cons x QQ(xs..., 0))
   * QQ(((unquote x) xs...), N) => (cons QQ((unquote x), N-1) QQ(xs..., N)), N > 0
   * QQ(((unquote-splicing x) xs...), 0) => (concat x QQ(xs..., 0))
   * QQ(((unquote-splicing x) xs...), N) => (concat QQ((unquote-splicing x), N-1) QQ(xs..., N)), N > 0
   * QQ(((quasiquote x) xs...), N) => (cons QQ((quasiquote x), N+1) QQ(xs..., N))
   * QQ((x xs...), N) => (cons QQ(x, N) QQ(xs..., N))
   */
  
  private List transformQuasiquote(Datum exp, int level)
  {
    if (exp == List.EMPTY)
      return List.EMPTY;
    
    // (quasiquote<N> x) => (quote x), for non-list x
    
    if (!(exp instanceof List list))
      return List.of(Symbol.QUOTE, exp);
    
    // Handle unquote, unquote-splicing, and nested quasiquote forms
    
    if (list.car() instanceof List form && !form.isEmpty())
    {
      if (form.car() instanceof Symbol s && s.equals(Symbol.UNQUOTE)) {
        if (form.length() != 2)
          throw malformedExp(form);
        
        // (quasiquote<0> ((unquote x) xs...)) => (cons x (quasiquote<0> xs...))
        
        if (level == 0) {
          return List.cons(Symbol.Q_CONS,
                 List.cons(form.cadr(),
                 List.cons(transformQuasiquote(list.cdr(), 0),
                 List.EMPTY)));
        }
        
        // (quasiquote<N> ((unquote x) xs...)) => (cons (quasiquote<N-1> (unquote x)) (quasiquote<N> xs...))
        
        return List.cons(Symbol.Q_CONS,
               List.cons(transformQuasiquote(form, level - 1),
               List.cons(transformQuasiquote(list.cdr(), level),
               List.EMPTY)));
      }

      if (form.car() instanceof Symbol s && s.equals(Symbol.UNQUOTE_SPLICING)) {
        if (form.length() != 2)
          throw malformedExp(exp);
        
        // (quasiquote<0> ((unquote-splicing x) xs...)) => (concat x (quasiquote<0> xs...))
        
        if (level == 0) {
          return List.cons(Symbol.Q_CONCAT,
                 List.cons(form.cadr(),
                 List.cons(transformQuasiquote(list.cdr(), level),
                 List.EMPTY)));
        }
        
        // (quasiquote<N> ((unquote-splicing x) xs...)) => (concat (quasiquote<N-1> (unquote-splicing x)) (quasiquote<N> xs...))
        
        return List.cons(Symbol.Q_CONS,
               List.cons(transformQuasiquote(form, level - 1),
               List.cons(transformQuasiquote(list.cdr(), level),
               List.EMPTY)));
      }
      
      // (quasiquote<N> ((quasiquote x) xs...)) => (cons (quasiquote<N+1> (quasiquote x)) (quasiquote<N> xs...))

      if (form.car() instanceof Symbol s && s.equals(Symbol.QUASIQUOTE)) {
        if (form.length() != 2)
          throw malformedExp(exp);

        return List.cons(Symbol.Q_CONS,
               List.cons(transformQuasiquote(form, level + 1),
               List.cons(transformQuasiquote(list.cdr(), level),
               List.EMPTY)));
      }
    }
    
    // General case: recursively transform the car and cdr of the list
    
    // (quasiquote<N> (x xs...)) => (cons (quasiquote<N> x) (quasiquote<N> xs...))

    return List.cons(Symbol.Q_CONS,
           List.cons(transformQuasiquote(list.car(), level),
           List.cons(transformQuasiquote(list.cdr(), level),
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
    return isVariadic
      ? new Procedure(compileVariadicProc(body, extendedScope, requiredArgs),
                      Signature.variadic(requiredArgs))
      : new Procedure(compileFixedProc(body, extendedScope, requiredArgs),
                      Signature.fixed(requiredArgs));
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

  private Inst compileDefmacro(List exp, Scope scope, boolean tail, Inst next)
  {
    if (!(exp.length() >= 4 && exp.cadr() instanceof Symbol symbol && symbol.isSimple() && exp.caddr() instanceof List params && checkParamListForm(params)))
      throw malformedExp(exp);
    var body = exp.cdddr();
    var macro = new Macro(makeProcedure(params, body, Scope.EMPTY));
    try {
      spartan.Runtime.currentPackage().bind(symbol.intern(), macro);
    }
    catch (MultipleDefinition err) {
      err.setSource(new SourceInfo(exp, positionOf(symbol)));
      throw err;
    }
    return new LoadConst(Void.VALUE, next);
  }
  
  /* Expand a macro
  
     Syntax: (f args...)
     
       where f is a macro
     
     Applies the macro procedure to the list of (unevaluated) arguments,
     and compiles the returned code.
  */
  private Datum expand(Macro macro, List exp)
  {
    var position = positionOf(exp);
    var args = quoteList(exp.cdr());
    var xform = macro.expand(vm, args, new SourceInfo(exp, position));
    
    if (Config.LOG_DEBUG && Config.SHOW_MACRO_EXPANSION)
      log.info(() -> String.format("macro transform: %s => %s", exp.repr(), xform.repr()));
    
    // augment source position map for the resulting generated code
    augmentSourceMap(xform, position);
    return xform;
  }
  
  private void augmentSourceMap(Datum exp, Position position)
  {
    if (exp instanceof List list) {
      positionMap.put(exp, position);
      for (var elem : list) {
        augmentSourceMap(elem, position);
      }
    }
    else if (exp instanceof Symbol) {
      positionMap.put(exp, position);
    }
  }
  
  /**
       <pattern> =>   _
                    | <symbol>
                    | <number>
                    | <string>
                    | <boolean>
                    | (quote <datum>)
                    | (list <pattern> ...)
                    | (list* <pattern> ...)
                    | (vector <pattern> ...)
                    | (record <symbol> <pattern> ...)
                    | (and <pattern> ...)
                    | (or <pattern> ...)
                    
                    
     Syntax: (match exp
               (pattern1 body1)
               ...
               (patternN bodyN))
   
     Compilation:

          <<exp>>
          push-env N          // extend environment for variables in pattern1
          match pattern1 L1   // continue if pattern matches value in RES; otherwise try next pattern
          <<body1>>           // evaluate body
          pop-env
          j LN
     L1:  push-env N
          match pattern2 L2
          <<body2>>
          pop-env
          j LN
          ...
          raise               // all patterns failed to match; raise error
     LN:  next
  */
  private Inst compileMatch(List exp, Scope scope, boolean tail, Inst next)
  {
    if (exp.length() < 3 || !checkMatchClauses(exp.cddr()))
      throw malformedExp(exp);

    return compile(exp.cadr(), scope, false,
           compileMatchClauses(exp, exp.cddr(), scope, tail, next));
  }
  
  private Inst compileMatchClauses(List matchExp, List clauses, Scope scope, boolean tail, Inst next)
  {
    if (clauses.isEmpty())
      return new Raise(new MatchFailure(new SourceInfo(matchExp, positionOf(matchExp))), next);

    var clause = (List) clauses.car();
    var patt = clause.car();
    var body = clause.cdr();
    var vars = patternVars(patt);
    var extendedScope = scope.extend(vars);
    
    //System.out.println("pattern = " + patt.repr());
    //System.out.println("pattern vars = " + vars.repr());    
    
    return new PushEnv(vars.length(),
           new Match(compilePattern(patt, vars),
                     compileSequence(body, extendedScope, tail, new PopEnv(new Jump(next))),
                     compileMatchClauses(matchExp, clauses.cdr(), scope, tail, next)));
  }
  
  private boolean checkMatchClauses(List clauses)
  {
    for (; !clauses.isEmpty(); clauses = clauses.cdr())
      if (!(clauses.car() instanceof List clause && clause.length() >= 2 && checkPattern(clause.car())))
        return false;
    return true;
  }
  
  private boolean checkPattern(Datum pattern)
  {
    return true;
  }
  
  private List patternVars(Datum pattern)
  {
    if (pattern instanceof Symbol symb && !symb.isKeyword() && !Symbol.UNDERSCORE.equals(symb))
      return List.of(symb);
    if (pattern instanceof List list && !list.isEmpty() && list.car() instanceof Symbol car) {
      if (car.equals(Symbol.LIST))
        return patternVarsInner(list.cdr());
      if (car.equals(Symbol.LIST_STAR))
        return patternVarsInner(list.cdr());
      if (car.equals(Symbol.VECTOR))
        return patternVarsInner(list.cdr());
      if (car.equals(Symbol.RECORD))
        return patternVarsInner(list.cddr());
      if (car.equals(Symbol.AND))
        return patternVarsInner(list.cdr());
      if (car.equals(Symbol.OR))
        return patternVarsInner(list.cdr());
    }
    return List.EMPTY;
  }
  
  private List patternVarsInner(List patterns)
  {
    if (patterns.isEmpty())
      return List.EMPTY;
    else
      return List.concat2(patternVars(patterns.car()), patternVarsInner(patterns.cdr()));
  }
  
  private IPattern compilePattern(Datum pattern, List vars)
  {
    if (pattern instanceof Symbol symb && Symbol.UNDERSCORE.equals(symb))
      return new MatchAny();
    if (pattern instanceof Symbol symb && symb.isKeyword())
      return new MatchEqual(symb);
    if (pattern instanceof Symbol symb)
      return new MatchVar(vars.index(symb::equals));
    if (pattern instanceof Bool || pattern instanceof IInt || pattern instanceof Text)
      return new MatchEqual((IEq)pattern);
    if (pattern instanceof List list && !list.isEmpty() && list.car() instanceof Symbol car) {
      if (car.equals(Symbol.QUOTE) && checkQuotePattern(list))
        return compileQuotePattern(list);
      if (car.equals(Symbol.LIST))
        return compileListPattern(list.cdr(), vars);
      if (car.equals(Symbol.LIST_STAR))
        return compileListStarPattern(list.cdr(), vars);
      if (car.equals(Symbol.VECTOR))
        return compileVectorPattern(list.cdr(), vars);
      if (car.equals(Symbol.RECORD) && checkRecordPattern(list))
        return compileRecordPattern(list, vars);
      if (car.equals(Symbol.AND))
        return compileAndPattern(list.cdr(), vars);
      if (car.equals(Symbol.OR) && checkOrPattern(list.cdr()))
        return compileOrPattern(list.cdr(), vars);
    }
    throw malformedExp(pattern);
  }
  
  private boolean checkQuotePattern(List pattern)
  {
    return pattern.length() == 2 && pattern.cadr() instanceof IEq;
  }
  
  // (quote <datum>)
  private IPattern compileQuotePattern(List pattern)
  {
    var datum = (IEq)pattern.cadr();
    return new MatchEqual(datum instanceof Symbol s ? s.intern() : datum);
  }
  
  private IPattern compileListPattern(List patterns, List vars)
  {
    if (patterns.isEmpty())
      return new MatchNull();
    else
      return new MatchCons(compilePattern(patterns.car(), vars),
                           compileListPattern(patterns.cdr(), vars));
  }
  
  private IPattern compileListStarPattern(List patterns, List vars)
  {
    if (!patterns.cdr().isEmpty() && patterns.cddr().isEmpty())
      return new MatchCons(compilePattern(patterns.car(), vars),
                           compilePattern(patterns.cadr(), vars));
    else
      return new MatchCons(compilePattern(patterns.car(), vars),
                           compileListStarPattern(patterns.cdr(), vars));
  }
  
  private IPattern compileVectorPattern(List patterns, List vars)
  {
    return new MatchVector(patterns.stream().map(pat -> compilePattern(pat, vars)).toArray(IPattern[]::new));
  }
  
  private IPattern compileRecordPattern(List pattern, List vars)
  {
    var typeName = (Symbol) pattern.cadr();
    var fieldPatterns = pattern.cddr();
    var rtd = spartan.Runtime.lookupRTD(typeName).get();
    return new MatchRecord(rtd, fieldPatterns.stream().map(pat -> compilePattern(pat, vars)).toArray(IPattern[]::new));
  }
  
  private IPattern compileAndPattern(List patterns, List vars)
  {
    if (patterns.isEmpty())
      return new MatchAny();
    else
      return new MatchAnd(compilePattern(patterns.car(), vars),
                          compileAndPattern(patterns.cdr(), vars));
  }
  
  private IPattern compileOrPattern(List patterns, List vars)
  {
    if (patterns.isEmpty())
      return new MatchFail();
    else
      return new MatchOr(compilePattern(patterns.car(), vars),
                         compileOrPattern(patterns.cdr(), vars));
  }
  
  private boolean checkOrPattern(List patterns)
  {
    var vars = patternVars(patterns.car());
    for (patterns = patterns.cdr(); !patterns.isEmpty(); patterns = patterns.cdr())
      if (!vars.equals(patternVars(patterns.car())))
        return false;
    return true;
  }
  
  // (record <type-name> <pattern>...)
  private boolean checkRecordPattern(List pattern)
  {
    return pattern.length() >= 2
        && pattern.cadr() instanceof Symbol typeName
        && spartan.Runtime.lookupRTD(typeName)
           .map(rtd -> rtd.fields().length == pattern.cddr().length())
           .orElse(false);
  }
  
  /* Compile a combination (i.e., special forms, procedure application, and macro expansion.
     
     Evaluation rules:
     
     A form (<exp0> <exp1> ... <expN>) is evaluated using the following rules:
     
     1. If <exp0> is a symbol that names a special form, then the rules of that
        special form apply.
        
     2. If <exp0> is a symbol bound to a macro, then macroexpansion occurs.
        
     3. Otherwise, <exp0> is assumed to evaluate to a procedure and the rules
        of procedure application apply.
  */
  
  private Inst compileCombo(List exp, Scope scope, boolean tail, Inst next)
  {
    if (exp.car() instanceof Symbol s) {
      return Optional.ofNullable(specialForms.get(s))
             .map(form -> form.compile(exp, scope, tail, next))
             .or(() -> lookupMacro(s).map(macro -> compile(expand(macro, exp), scope, tail, next)))
             .orElseGet(() -> compileApply(exp, scope, tail, next));
    }
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
  
  private final Map<Symbol, ISpecialForm> specialForms = Map.ofEntries(
    Map.entry(Symbol.DEF, this::compileDef),
    Map.entry(Symbol.DEFUN, this::compileDefun),
    Map.entry(Symbol.DEFMACRO, this::compileDefmacro),
    Map.entry(Symbol.IF, this::compileIf),
    Map.entry(Symbol.COND, this::compileCond),
    Map.entry(Symbol.LET, this::compileLet),
    Map.entry(Symbol.LETSTAR, this::compileLetStar),
    Map.entry(Symbol.LETREC, this::compileLetRec),
    Map.entry(Symbol.FUN, this::compileFun),
    Map.entry(Symbol.DO, this::compileDo),
    Map.entry(Symbol.FOR, this::compileFor),
    //Map.entry(Symbol.REC, this::compileRec),
    Map.entry(Symbol.SET, this::compileSet),
    Map.entry(Symbol.WHILE, this::compileWhile),
    Map.entry(Symbol.QUOTE, this::compileQuote),
    //Map.entry(Symbol.QUASIQUOTE, this::compileQuasiquote),
    Map.entry(Symbol.OR, this::compileOr),
    Map.entry(Symbol.AND, this::compileAnd),
    Map.entry(Symbol.MATCH, this::compileMatch)
  );
  
  private PositionMap positionMap;
  private final VirtualMachine vm;
  private static final Logger log = Logger.getLogger(Compiler.class.getName());
}
