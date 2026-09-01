package spartan.compiling;

import spartan.runtime.*;
import spartan.data.*;
import spartan.parsing.SourceDatum;
import spartan.parsing.PositionMap;
import spartan.parsing.Position;
import spartan.errors.SourceInfo;
import spartan.errors.Error;
import spartan.errors.SyntaxError;
import spartan.errors.UnboundSymbol;
import spartan.errors.ModuleDoesNotExist;
import spartan.errors.MultipleDefinition;
import spartan.errors.MatchFailure;
import spartan.errors.InvalidPattern;
import spartan.runtime.VirtualMachine;
import spartan.Config;
import java.util.logging.Logger;
import java.util.Optional;
import java.util.Map;
import static spartan.compiling.CompilerUtils.*;
import static java.util.Map.entry;
import static spartan.Runtime.lookupMacro;
import static spartan.Runtime.currentModule;
import static spartan.Runtime.canonicalName;
import spartan.util.Box;

/**
 * Compiles source expressions into executable bytecodes.
 */
public class Compiler
{
  /** Constructor. Requires a VM instance for macro expansion.
   * @param vm the {@code VirtualMachine} used for macro expansion.
   */
  public Compiler(VirtualMachine vm)
  {
    this.vm = vm;
  }
  
  /** Compile a single top-level source expression.
   *
   * @param exp the expression to compile
   * @return the compiled bytecode (an instruction graph)
   * @throws SyntaxError if the given exp is not syntactically valid
   */
  public Inst compile(SourceDatum exp)
  {
    positionMap = exp.positionMap();
    var code = compile(exp.datum(), Scope.EMPTY, false, true, new Halt());    
    if (Config.LOG_DEBUG && Config.EMIT_BYTECODE) {
      log.info(() -> String.format("Generating bytecode listing for expression at %s\n", positionOf(exp.datum())));
      try {
        CodeListing.generate(code);
      }
      catch (java.io.IOException _) {
        // ignore
      }
    }
    return code;
  }
  
  /** Convenience method for creating instances of SyntaxError for malformed expressions. */
  private SyntaxError malformedExp(Datum exp)
  {
    return new SyntaxError("malformed expression", new SourceInfo(exp, positionOf(exp)));
  }
  
  /** Convenience method for creating instances of SyntaxError for misplaced definitions. */
  private SyntaxError defNotAllowed(Datum exp)
  {
    return new SyntaxError("definition not allowed in this context", new SourceInfo(exp, positionOf(exp)));
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
        || exp == Nil.VALUE
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
    if (s.isQualified())
      return compileGlobalVarRef(s, next);
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
  
  /* Compile a global variable reference.
     
     All global variable references are normalized such that they are fully-qualified:
     
     1. If the symbol is qualified and the namespace portion is a namespace alias,
        replace the alias with the namespace's canonical name.
     
     2. If the symbol is not qualified, it is qualified as being in the current namespace.
  */
  private Inst compileGlobalVarRef(Symbol s, Inst next)
  {
    if (s instanceof QualifiedSymbol qs) {
      var moduleName = Symbol.of(qs.moduleName());
      var baseName = Symbol.of(qs.baseName());
      var loc = spartan.Runtime.getModule(canonicalName(moduleName))
                .orElseThrow(() -> new ModuleDoesNotExist(moduleName, new SourceInfo(s, positionOf(s))))
                .lookupPublic(baseName)
                .orElseThrow(() -> new UnboundSymbol(s, new SourceInfo(s, positionOf(s))));
      return new LoadGlobal(s, loc, next);
    }
    else {
      var loc = currentModule().lookup(s.intern())
                .orElseThrow(() -> new UnboundSymbol(s, new SourceInfo(s, positionOf(s))));
      return new LoadGlobal(s, loc, next);
    }
  }
  
  /* Compile a variable assignment.

     Syntax: (set! symbol init)

     If the symbol is a bound local variable, look up its DeBruijn index (a depth, offset
     pair), and generate a store-local(depth, offset) instruction.

     Otherwise, assume the symbol is a global variable, and generate a store-global instruction.
  */
  private Inst compileSet(List exp, Scope scope, boolean tail, boolean allowDefs, Inst next)
  {
    if (!(exp.length() == 3 && exp.second() instanceof Symbol s))
      throw malformedExp(exp);
    
    var init = exp.third();
    
    if (s.isQualified())
      return compile(init, scope, false, false,
             compileSetGlobalVar(s, next));
    return compile(init, scope, false, false,
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
    if (s instanceof QualifiedSymbol qs) {
      var moduleName = Symbol.of(qs.moduleName());
      var baseName = Symbol.of(qs.baseName());
      var loc = spartan.Runtime.getModule(canonicalName(moduleName))
                .orElseThrow(() -> new ModuleDoesNotExist(moduleName, new SourceInfo(s, positionOf(s))))
                .lookupPublic(baseName)
                .orElseThrow(() -> new UnboundSymbol(s, new SourceInfo(s, positionOf(s))));
      return new StoreGlobal(s, loc, new LoadConst(Nil.VALUE, next));
    }
    else {
      var loc = currentModule().lookup(s.intern())
                .orElseThrow(() -> new UnboundSymbol(s, new SourceInfo(s, positionOf(s))));
      return new StoreGlobal(s, loc, new LoadConst(Nil.VALUE, next));
    }
  }
  
  /* Compile "def" special form
  
     Syntax: (def name init)
     
     Compilation:
     
         <<init>>
         store-global name
         load-const #nil
  */
  private Inst compileDef(List exp, Scope scope, boolean tail, boolean allowDefs, Inst next)
  {
    if (!allowDefs)
      throw defNotAllowed(exp);
    if (!(exp.length() == 3 && exp.second() instanceof Symbol name && name.isSimple()))
      throw malformedExp(exp);
    var init = exp.third();
    Box<Datum> loc = null;
    try {
      loc = currentModule().bind(name.intern());
    }
    catch (MultipleDefinition err) {
      err.setSource(new SourceInfo(exp, positionOf(name)));
      throw err;
    }
    return compile(init, scope, false, false,
       new StoreGlobal(name, loc,
       new LoadConst(Nil.VALUE, next)));
  }
  
  /* Compile "defun" special form
  
     Syntax: (defun name (param...) body...)
  */
  private Inst compileDefun(List exp, Scope scope, boolean tail, boolean allowDefs, Inst next)
  {
    if (!allowDefs)
      throw defNotAllowed(exp);
    if (!(exp.length() > 3 && exp.second() instanceof Symbol name && name.isSimple()
       && exp.third() instanceof List params && checkParamList(params)))
     throw malformedExp(exp);
    
    var xform = transformDefun(exp);
    
    if (Config.LOG_DEBUG && Config.SHOW_MACRO_EXPANSION)
      log.info(() -> String.format("defun transform: %s => %s", exp.repr(), xform.repr()));
    
    return compile(xform, scope, false, allowDefs, next);
  }
  
  /* Transform "defun" special form into equivalent equivalent "def" form:
     
     (defun name (param...) body...) => (def name (fun (param...) body...))
  */
  private List transformDefun(List exp)
  {
    var name = exp.second();
    var params = exp.third();
    var body = exp.drop3();
    return List.of(Symbol.DEF, name, List.adjoin(Symbol.FUN, List.adjoin(params, body)));
  }

  /* Compile the "if" special form.

     Syntax: (if pred sub alt)
     
     Syntax: (if pred sub)

     The second form (without a 2nd branch) is converted to the first form,
     as (if pred sub #nil).
     
     Compilation:

           <<pred>>
           jump-false L
           <<sub>>
           jump next
     L:    <<alt>>
     next: ...
  */
  private Inst compileIf(List exp, Scope scope, boolean tail, boolean allowDefs, Inst next)
  {
    int length = exp.length();

    if (length < 3 || length > 4)
      throw malformedExp(exp);

    var pred = exp.second();
    var sub = exp.third();
    var alt = length == 4 ? exp.fourth() : Nil.VALUE;
    
    var elseBranch = compile(alt, scope, tail, false, next);
    return compile(pred, scope, false, false,
           new JumpFalse(elseBranch,
           compile(sub, scope, tail, false,
           new Jump(next,
           elseBranch))));
  }

  /* Compiles the "cond" special form.
     
     Syntax: (cond (pred1 body1)
                   ...
                   (predN bodyN))
     
     Syntax: (cond (pred1 body1)
                   ...
                   (predN bodyN)
                   (else  default))
          
     The first form (without an explicit else clause) is equivalent to the
     second form with an (else #nil) clause.
     
     Compilation:

     L1:   <<pred1>>
           jump-false P2
           <<body1>>
           jump next
     L2:   ...
     LN:   <<predN>>
           jump-false none
           <<bodyN>>
           jump next
     none: <<default>>
     next: ...
  */
  private Inst compileCond(List exp, Scope scope, boolean tail, boolean allowDefs, Inst next)
  {
    if (exp.length() < 2 || !checkCondClauses(exp.rest()))
      throw malformedExp(exp);

    return compileCondClauses(exp.rest(), scope, tail, next);
  }

  private Inst compileCondClauses(List clauses, Scope scope, boolean tail, Inst next)
  {
    if (clauses.isEmpty())
      return new LoadConst(Nil.VALUE, next);

    var clause = (List) clauses.first();
    var pred = clause.first();
    var body = clause.rest();

    if (Symbol.ELSE.equals(pred))
      return compileSequence(body, scope, tail, next);

    var nextClause = compileCondClauses(clauses.rest(), scope, tail, next);
    return compile(pred, scope, false, false,
           new JumpFalse(nextClause,
           compileSequence(body, scope, tail,
           new Jump(next,
           nextClause))));
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

  private Inst compileLet(List exp, Scope scope, boolean tail, boolean allowDefs, Inst next)
  {
    if (!(exp.length() >= 3 && exp.second() instanceof List bindings && checkBindingList(bindings)))
      throw malformedExp(exp);

    var body = exp.drop2();
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
  private Inst compileLetStar(List exp, Scope scope, boolean tail, boolean allowDefs, Inst next)
  {
    if (!(exp.length() >= 3 && exp.second() instanceof List bindings && checkBindingList(bindings)))
      throw malformedExp(exp);

    var body = exp.drop2();
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

    var binding = (List) bindings.first();
    var symb = (Symbol) binding.first();
    var init = binding.second();

    return compile(init, scope, false, false,
                   new StoreLocal0(offset,
                   compileSequentialBindings(bindings.rest(), offset + 1, scope.bind(symb),
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
  private Inst compileLetRec(List exp, Scope scope, boolean tail, boolean allowDefs, Inst next)
  {
    if (!(exp.length() >= 3 && exp.second() instanceof List bindings && checkBindingList(bindings)))
      throw malformedExp(exp);

    var body = exp.drop2();
    int numBindings = bindings.length();
    var vars = extractFirst(bindings);
    var inits = extractSecond(bindings);
    var extendedScope = scope.extend(vars);

    return new PushEnv(numBindings,
           new LoadConst(Nil.VALUE,
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

    return compile(inits.first(), scope, false, false,
           new StoreLocal0(offset,
           compileRecursiveBindings(inits.rest(), offset + 1, scope, next)));
  }
  
  /* Generate the initialization sequence of a letrec expression. */
  
  private Inst compileInitRecEnv(int offset, int numBindings, Inst next)
  {
    if (offset >= numBindings)
      return next;
    else
      return new StoreLocal0(offset,
             compileInitRecEnv(offset + 1, numBindings, next));
  }

  /* Generate the argument-evaluation sequence of a procedure application */
  
  private Inst compilePushArgs(List args, Scope scope, Inst next)
  {
    if (args.isEmpty())
      return next;

    return compilePushArgs(args.rest(), scope,
           compile(args.first(), scope, false, false,
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

     Syntax: (f arg1 arg2 ... argN)

     Compilation:

           push-frame  // Omit when this expression occurs in tail context
           <<argN>>
           push-arg
           ...
           <<arg1>>
           push-arg
           <<f>>
           apply
     next: ...
  */

  private Inst compileApply(List exp, Scope scope, boolean tail, Inst next)
  {
    var proc = exp.first();
    var args = exp.rest();
    int numArgs = args.length();
    var position = positionOf(exp);
    var source = new SourceInfo(exp, position);
    
    if (tail && Config.TAIL_CALLS) // Tail call optimization: omit call frame, return directly to the caller
      return compilePushArgs(args, scope,
             compile(proc, scope, false, false,
             new Apply(numArgs, source,
             next)));
    else
      return new PushFrame(next, position,
             compilePushArgs(args, scope,
             compile(proc, scope, false, false,
             new Apply(numArgs, source,
             next))));
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
    if (exp.isEmpty())
      return next;

    return compile(exp.first(), scope, (tail && exp.rest().isEmpty()), false,
           compileSequence(exp.rest(), scope, tail, next));
  }

  /* Compiles a procedure body. A procedure body consists of a sequence
     of expressions, optionally preceeded by a list of inner definitions.

     Inner definitions are transformed into an equivalent form using a
     nested letrec form, shown below.
  */
  private Inst compileBody(List body, Scope scope, Inst next)
  {
    if (isInnerDefinition(body.first())) {
      var xform = transformInnerDefs(body);
      if (Config.LOG_DEBUG && Config.SHOW_MACRO_EXPANSION)
        log.info(() -> String.format("inner defs transform: %s => %s", body.repr(), xform.repr()));
      return compile(xform, scope, true, false, next);
    }

    return compileSequence(body, scope, true, next);
  }
  
  /* Compiles a lambda expression (anonymous procedure)
    
     Syntax: (fun <params> body...)
   */
  private Inst compileFun(List exp, Scope scope, boolean tail, boolean allowDefs, Inst next)
  {
    if (!(exp.length() >= 3 && exp.second() instanceof List params && checkParamList(params)))
      throw malformedExp(exp);
    var body = exp.drop2();
    var proc = makeProcedure(params, body, scope);    
    return new MakeClosure(proc, next);
  }

  /* Transforms a sequence of inner definitions at the beginning
     of a procedure body:

     (fun (param...)
       (def var1 init1)
       ...
       (def varN initN)
       body...)

     into an equivalent letrec form:

     (fun (param...)
       (letrec ((var1 init1)
                ...
                (varN initN))
         body...))

     Inner procedure definitions are first transformed into simple definitions:

     (defun var params body) => (def var (fun params body))
  */
  private List transformInnerDefs(List body)
  {
    var bindings = new List.Builder();

    while (!body.isEmpty() && isInnerDefinition(body.first())) {
      var exp = (List) body.first();
      if (Symbol.DEFUN.equals(exp.first())) {
        if (!(exp.length() > 3 && exp.second() instanceof Symbol s && s.isSimple()
           && exp.third() instanceof List params && checkParamList(params)))
          throw malformedExp(exp);
        exp = transformDefun(exp);
      }
      if (!(exp.length() == 3 && exp.second() instanceof Symbol s && s.isSimple()))
        throw malformedExp(exp);
      bindings.add(exp.rest());
      body = body.rest();
    }

    // TODO: body possibly empty after inner definitions; causes error since letrec
    // doesn't allow empty body
    return List.adjoin(Symbol.LETREC, List.adjoin(bindings.build(), body));
  }

  /* Compiles the "or" special form, a logical disjunction.

     Syntax: (or exp1 ... expN)

     Compilation:

            <<exp1>>
            jump-true next
            ...
            <<expN>>
      next: ...
  */

  private Inst compileOr(List exp, Scope scope, boolean tail, boolean allowDefs, Inst next)
  {
    if (exp.length() < 2)
      throw malformedExp(exp);

    return compileDisjunction(exp.rest(), scope, tail, next);
  }

  private Inst compileDisjunction(List exp, Scope scope, boolean tail, Inst next)
  {
    if (exp.rest().isEmpty())
      return compile(exp.first(), scope, tail, false, next);

    return compile(exp.first(), scope, false, false,
           new JumpTrue(next,
           compileDisjunction(exp.rest(), scope, tail, next)));
  }

  /* Compiles the "and" special form, a logical conjunction.

     Syntax: (and exp1 ... expN)

     Compilation:

            <<exp1>>
            jump-false next
            ...
            <<expN>>
      next: ...
  */

  private Inst compileAnd(List exp, Scope scope, boolean tail, boolean allowDefs, Inst next)
  {
    if (exp.length() < 2)
      throw malformedExp(exp);

    return compileConjuction(exp.rest(), scope, tail, next);
  }

  private Inst compileConjuction(List exp, Scope scope, boolean tail, Inst next)
  {
    if (exp.rest().isEmpty())
      return compile(exp.first(), scope, tail, false, next);

    return compile(exp.first(), scope, false, false,
           new JumpFalse(next,
           compileConjuction(exp.rest(), scope, tail, next)));
  }

  /* Compiles the "do" special form. */
  
  private Inst compileDo(List exp, Scope scope, boolean tail, boolean allowDefs, Inst next)
  {
    if (exp.length() < 2)
      throw malformedExp(exp);
    
    return compileDoInner(exp.rest(), scope, tail, allowDefs, next);
  }
  
  private Inst compileDoInner(List exp, Scope scope, boolean tail, boolean allowDefs, Inst next)
  {
    if (exp.isEmpty())
      return next;
    
    // Handle definitions within the "do" form.
    // Some weirdness here to ensure definitions appearing in the sequence are compiled
    // top-to-bottom (such that later expressions may refer to them) instead of the usual
    // bottom-to-top compilation strategy. Compile the definition with a mutable "nop" as
    // the next instruction; only once it is compiled do we set the next of the nop.
    if (isTopLevelDefinition(exp.first())) {
      var end = new Nop();
      var result = compile(exp.first(), scope, tail && exp.rest().isEmpty(), allowDefs, end);
      end.setNext(compileDoInner(exp.rest(), scope, tail, allowDefs, next));
      return result;
    }

    return compile(exp.first(), scope, tail && exp.rest().isEmpty(), allowDefs,
           compileDoInner(exp.rest(), scope, tail, allowDefs, next));
  }
  
  /* Compiles the "while" special form.

     Syntax: (while pred body...)

     Compilation:

     L0:   <<pred>>
           jump-false L1
           <<body>>
           jump L0
     L1:   load-const #nil
     next: ...
  */

  private Inst compileWhile(List exp, Scope scope, boolean tail, boolean allowDefs, Inst next)
  {
    if (exp.length() < 3)
      throw malformedExp(exp);

    var pred = exp.second();
    var body = exp.drop2();
    
    var jump = new Jump();    
    var end = new LoadConst(Nil.VALUE, next);
    var top = compile(pred, scope, false, false,
              new JumpFalse(end,
              compileSequence(body, scope, false, jump)));
    jump.setTarget(top);
    jump.setNext(end);
    return top;
  }
  
  /* Compile the "rep" special form for expressing (functional) iteration.
   
     Syntax: (rep ((var init step) ...)
               (when test result)
               body...)
     
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
           jump-true done

           ; evaluate body expressions for side-effect

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
  private Inst compileRep(List exp, Scope scope, boolean tail, boolean allowDefs, Inst next)
  {
    if (!(exp.length() >= 3 && exp.second() instanceof List bindings && checkRepBindings(bindings)
        && exp.third() instanceof List subform && subform.length() == 3 && Symbol.WHEN.equals(subform.first())))
      throw malformedExp(exp);
    
    int numBindings = bindings.length();
    var vars = extractFirst(bindings);
    var inits = extractSecond(bindings);
    var steps = extractThird(bindings);
    var extendedScope = scope.extend(vars);
    var test = subform.second();
    var result = subform.third();
    var body = exp.drop3();
    
    var jump = new Jump();
    var end = compile(result, extendedScope, false, false,
              new PopEnv(next));
    var top = compile(test, extendedScope, false, false,
              new JumpTrue(end,
              compileSequence(body, extendedScope, false,
              compilePushArgs(steps, extendedScope,
              compileBindLocals(0, numBindings,                          
              jump)))));
    jump.setTarget(top);
    jump.setNext(end);
    return compilePushArgs(inits, scope,
           new PushEnv(numBindings,
           compileBindLocals(0, numBindings,
           top)));
  }
  
  /* Compile the "quote" special form.
  
     Syntax: (quote x)
  */
  private Inst compileQuote(List exp, Scope scope, boolean tail, boolean allowDefs, Inst next)
  {
    if (exp.length() != 2)
      throw malformedExp(exp);
    
    if (exp.second() instanceof Symbol s)
      return new LoadConst(s.intern(), next);
    else if (exp.second() instanceof List list)
      return new LoadConst(quoteList(list), next);
    else
      return new LoadConst(exp.second(), next);
  }

  private static List quoteList(List list)
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

  private Inst compileQuasiquote(List exp, Scope scope, boolean tail, boolean allowDefs, Inst next)
  {
    if (exp.length() != 2)
      throw malformedExp(exp);

    var xform = transformQuasiquote(exp.second(), 0);
    augmentSourceMap(xform, positionOf(exp));
    
    if (Config.LOG_DEBUG && Config.SHOW_MACRO_EXPANSION)
      log.info(() -> String.format("quasiquote transform: %s => %s", exp.repr(), xform.repr()));
    
    try {
      return compile(xform, scope, false, false, next);
    }
    catch (Error err) {
      err.setSource(new SourceInfo(exp, positionOf(exp)));
      throw err;
    }
  }

  /**
   * Transforms a quasiquote expression with nested unquote and unquote-splicing
   * forms into an equivalent expression using only quote, adjoin, and concat.
   *
   * Let QQ(exp, N) denote the quasiquote transformation of exp with nesting
   * level N, defined as follows:
   *
   * QQ(x, N) => (quote x), for non-list value x
   * QQ(((unquote x) xs...), 0) => (adjoin x QQ(xs..., 0))
   * QQ(((unquote x) xs...), N) => (adjoin QQ((unquote x), N-1) QQ(xs..., N)), N > 0
   * QQ(((unquote-splicing x) xs...), 0) => (concat x QQ(xs..., 0))
   * QQ(((unquote-splicing x) xs...), N) => (concat QQ((unquote-splicing x), N-1) QQ(xs..., N)), N > 0
   * QQ(((quasiquote x) xs...), N) => (adjoin QQ((quasiquote x), N+1) QQ(xs..., N))
   * QQ((x xs...), N) => (adjoin QQ(x, N) QQ(xs..., N))
   */  
  private List transformQuasiquote(Datum exp, int level)
  {
    if (exp == List.EMPTY)
      return List.EMPTY;
    
    // (quasiquote<N> x) => (quote x), for non-list x
    
    if (!(exp instanceof List list))
      return List.of(Symbol.QUOTE, exp);
    
    // Handle unquote, unquote-splicing, and nested quasiquote forms
    
    if (list.first() instanceof List form && !form.isEmpty())
    {
      if (form.first() instanceof Symbol s && s.equals(Symbol.UNQUOTE)) {
        if (form.length() != 2)
          throw malformedExp(form);
        
        // (quasiquote<0> ((unquote x) xs...)) => (adjoin x (quasiquote<0> xs...))
        
        if (level == 0) {
          return List.adjoin(Symbol.Q_ADJOIN,
                 List.adjoin(form.second(),
                 List.adjoin(transformQuasiquote(list.rest(), 0),
                 List.EMPTY)));
        }
        
        // (quasiquote<N> ((unquote x) xs...)) => (adjoin (quasiquote<N-1> (unquote x)) (quasiquote<N> xs...))
        
        return List.adjoin(Symbol.Q_ADJOIN,
               List.adjoin(transformQuasiquote(form, level - 1),
               List.adjoin(transformQuasiquote(list.rest(), level),
               List.EMPTY)));
      }

      if (form.first() instanceof Symbol s && s.equals(Symbol.UNQUOTE_SPLICING)) {
        if (form.length() != 2)
          throw malformedExp(exp);
        
        // (quasiquote<0> ((unquote-splicing x) xs...)) => (concat x (quasiquote<0> xs...))
        
        if (level == 0) {
          return List.adjoin(Symbol.Q_CONCAT,
                 List.adjoin(form.second(),
                 List.adjoin(transformQuasiquote(list.rest(), level),
                 List.EMPTY)));
        }
        
        // (quasiquote<N> ((unquote-splicing x) xs...)) => (concat (quasiquote<N-1> (unquote-splicing x)) (quasiquote<N> xs...))
        
        return List.adjoin(Symbol.Q_ADJOIN,
               List.adjoin(transformQuasiquote(form, level - 1),
               List.adjoin(transformQuasiquote(list.rest(), level),
               List.EMPTY)));
      }
      
      // (quasiquote<N> ((quasiquote x) xs...)) => (adjoin (quasiquote<N+1> (quasiquote x)) (quasiquote<N> xs...))

      if (form.first() instanceof Symbol s && s.equals(Symbol.QUASIQUOTE)) {
        if (form.length() != 2)
          throw malformedExp(exp);

        return List.adjoin(Symbol.Q_ADJOIN,
               List.adjoin(transformQuasiquote(form, level + 1),
               List.adjoin(transformQuasiquote(list.rest(), level),
               List.EMPTY)));
      }
    }
    
    // General case: recursively transform the car and cdr of the list
    
    // (quasiquote<N> (x xs...)) => (adjoin (quasiquote<N> x) (quasiquote<N> xs...))

    return List.adjoin(Symbol.Q_ADJOIN,
           List.adjoin(transformQuasiquote(list.first(), level),
           List.adjoin(transformQuasiquote(list.rest(), level),
           List.EMPTY)));
  }
  
  /** Given a function parameter list and body expression, return a Procedure
   *  object containing a Signature and the compiled bytecode for the function.
   *
   * @param params the procedure parameters
   * @param body the procedure body
   * @param scope scope of procedure definition
   * @return 
   */  
  private Procedure makeProcedure(List params, List body, Scope scope)
  {
    var parsedParams = parseParamList(params);
    var requiredArgs = parsedParams.requiredArgs();
    var numRequired = requiredArgs.length();
    var optionalArgs = parsedParams.optionalArgs();
    var numOptional = optionalArgs.length();
    var restArg = parsedParams.restArg();
    var numRest = restArg.length(); // 0 or 1
    var numBindings = numRequired + numOptional + numRest;
    var allArgs = List.concat(requiredArgs, extractFirst(optionalArgs), restArg);
    
    // Determine procedure Signature
    // Case 1: N required args
    //   Check: numArgs == N; folds into Signature.fixed(N)
    // Case 2: N required args, M optional args
    //   Check: numArgs >= N && numArgs <= N+M; folds into Signature.variadic(N, M)
    // Case 3: N required args + a rest arg
    //   Check: numArgs >= N; folds into Signature.variadic(N)
    // Case 4: N required args, M optional args + a rest arg
    //   Check: numArgs >= N; folds into Signature.variadic(N)

    var sig = !restArg.isEmpty()
                ? Signature.variadic(numRequired)
                : (!optionalArgs.isEmpty()
                     ? Signature.variadic(numRequired, numOptional)
                     : Signature.fixed(numRequired));
    
    var code = new PushEnv(numBindings,
               compileBindRequired(0, numRequired,
               compileBindOptionals(numRequired, optionalArgs, scope.extend(requiredArgs),
               compileBindRestArg(numRequired + numOptional, numRest != 0,
               compileBody(body, scope.extend(allArgs),
               new PopFrame())))));
    
    return new Procedure(code, sig);
  }

  private Inst compileBindRequired(int offset, int numRequired, Inst next)
  {
    if (offset >= numRequired)
      return next;

    return new PopArg(
           new StoreLocal0(offset,
           compileBindRequired(offset + 1, numRequired,
           next)));
  }

  private Inst compileBindOptionals(int offset, List optionalArgs, Scope scope, Inst next)
  {
    if (optionalArgs.isEmpty())
      return next;    
    var pair = (List) optionalArgs.first();
    var symb = (Symbol) pair.first();
    var init = pair.second();    
    next = new StoreLocal0(offset,
           compileBindOptionals(offset + 1, optionalArgs.rest(), scope.bind(symb),
           next));
    var elseBranch = compile(init, scope, false, false, next);
    return new JumpNoArgs(elseBranch,
           new PopArg(
           new Jump(next,
           elseBranch)));
  }

  private Inst compileBindRestArg(int offset, boolean hasRest, Inst next)
  {
    return !hasRest ? next : new PopRestArgs(
                             new StoreLocal0(offset,
                             next));
  }
  
  
  /* Compile the "defmacro" special form.
     
     Syntax: (defmacro f (param...) body...)
     
     Creates a macro, which is essentially a regular procedure, but
     is intended to generate code rather than data. It receives
     unevaluated expressions as arguments and returns an expression
     which is then compiled and evaluated. 
  */

  private Inst compileDefmacro(List exp, Scope scope, boolean tail, boolean allowDefs, Inst next)
  {
    if (!allowDefs)
      throw defNotAllowed(exp);
    if (!(exp.length() >= 4 && exp.second() instanceof Symbol symbol && symbol.isSimple()
        && exp.third() instanceof List params && checkParamList(params)))
      throw malformedExp(exp);
    var body = exp.drop3();
    var macro = new Macro(makeProcedure(params, body, Scope.EMPTY));
    try {
      currentModule().bind(symbol.intern(), macro);
    }
    catch (MultipleDefinition err) {
      err.setSource(new SourceInfo(exp, positionOf(symbol)));
      throw err;
    }
    return new LoadConst(Nil.VALUE, next);
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
    var args = quoteList(exp.rest());
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

  /* Compile the "match" special form for generalized pattern matching.
      
      Syntax: (match exp
                (pattern1 body1)
                ...
                (patternN bodyN))
      
      <pattern> =>    _                               ; match anything
                    | <symbol>                        ; match anything and bind to symbol
                    | <number>                        ; match numeric value
                    | <string>                        ; match string value
                    | <boolean>                       ; match boolean value
                    | (quote <datum>)                 ; match quoted value
                    | (list <pattern> ...)            ; match list
                    | (list* <pattern> ...)
                    | (vector <pattern> ...)
                    | (record <symbol> <pattern> ...) ;
                    | (and <pattern> ...)
                    | (or <pattern> ...)
     
     Compilation:

           <<exp>>
           push-env N_max
     P1:   jump-no-match pattern1 P2
           <<body1>>
           jump DONE
     P2:   ...
     PN:   jump-no-match patternN FAIL
           <<bodyN>>
           jump DONE
     FAIL: raise               // all patterns failed to match; raise error
     DONE: pop-env
           ...
           
           <<exp>>
           push-env N_max
           jump-match pattern1 L2
           ...
           jump-match patternN LN
           jump FAIL
     L1:   <<body1>>
           jump DONE
           ...
     LN:   <<bodyN>>
           jump DONE
     FAIL: raise               // all patterns failed to match; raise error
     DONE: pop-env
           ...
  */
  private Inst compileMatch(List exp, Scope scope, boolean tail, boolean allowDefs, Inst next)
  {
    if (exp.length() < 3 || !checkMatchClauses(exp.drop2()))
      throw malformedExp(exp);
    
    var clauses = exp.drop2();
    
    return compile(exp.second(), scope, false, false,
           new PushEnv(matchEnvSize(clauses),
           compileMatchClauses(exp, clauses, scope, tail,
           new PopEnv(next))));
  }

  private Inst compileMatchClauses(List matchExp, List clauses, Scope scope, boolean tail, Inst next)
  {
    if (clauses.isEmpty())
      return new Raise(new MatchFailure(new SourceInfo(matchExp, positionOf(matchExp))), next);

    var clause = (List) clauses.first();
    var patt = clause.first();
    var body = clause.rest();
    var vars = patternVars(patt);
    var extendedScope = scope.extend(vars);
    
    var nextClause = compileMatchClauses(matchExp, clauses.rest(), scope, tail, next);
    return new JumpNoMatch(compilePattern(patt, vars), nextClause,
           compileSequence(body, extendedScope, tail,
           new Jump(next,
           nextClause)));
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
    if (pattern instanceof List list && !list.isEmpty() && list.first() instanceof Symbol first) {
      if (first.equals(Symbol.QUOTE) && checkQuotePattern(list))
        return compileQuotePattern(list);
      if (first.equals(Symbol.LIST))
        return compileListPattern(list.rest(), vars);
      if (first.equals(Symbol.LIST_STAR))
        return compileListStarPattern(list.rest(), vars);
      if (first.equals(Symbol.VECTOR))
        return compileVectorPattern(list.rest(), vars);
      if (first.equals(Symbol.RECORD) && checkRecordPattern(list))
        return compileRecordPattern(list, vars);
      if (first.equals(Symbol.AND))
        return compileAndPattern(list.rest(), vars);
      if (first.equals(Symbol.OR) && checkOrPattern(list.rest()))
        return compileOrPattern(list.rest(), vars);
    }
    throw malformedExp(pattern);
  }
  
  private IPattern compileQuotePattern(List pattern)
  {
    var datum = (IEq)pattern.second();
    datum = datum instanceof Symbol s ? s.intern() : datum;
    return new MatchEqual(datum);
  }
  
  private IPattern compileListPattern(List patterns, List vars)
  {
    if (patterns.isEmpty())
      return new MatchEmpty();
    else
      return new MatchList(compilePattern(patterns.first(), vars),
                           compileListPattern(patterns.rest(), vars));
  }
  
  private IPattern compileListStarPattern(List patterns, List vars)
  {
    if (!patterns.rest().isEmpty() && patterns.drop2().isEmpty())
      return new MatchList(compilePattern(patterns.first(), vars),
                           compilePattern(patterns.second(), vars));
    else
      return new MatchList(compilePattern(patterns.first(), vars),
                           compileListStarPattern(patterns.rest(), vars));
  }
  
  private IPattern compileVectorPattern(List patterns, List vars)
  {
    return new MatchVector(patterns.stream().map(pat -> compilePattern(pat, vars)).toArray(IPattern[]::new));
  }
  
  private IPattern compileRecordPattern(List pattern, List vars)
  {
    var typeName = (Symbol) pattern.second();
    var fieldPatterns = pattern.drop2();
    var rtd = spartan.Runtime.lookupRTD(typeName).get();
    return new MatchRecord(rtd, fieldPatterns.stream().map(pat -> compilePattern(pat, vars)).toArray(IPattern[]::new));
  }
  
  private IPattern compileAndPattern(List patterns, List vars)
  {
    if (patterns.isEmpty())
      return new MatchAny();
    else
      return new MatchAnd(compilePattern(patterns.first(), vars),
                          compileAndPattern(patterns.rest(), vars));
  }
  
  private IPattern compileOrPattern(List patterns, List vars)
  {
    if (patterns.isEmpty())
      return new MatchFail();
    else
      return new MatchOr(compilePattern(patterns.first(), vars),
                         compileOrPattern(patterns.rest(), vars));
  }
  
  /* Compile a compound expression (i.e., special forms, function application, or macro usage.)
     
     Evaluation rules:
     
     A form (<exp0> <exp1> ... <expN>) is evaluated using the following rules:
     
     1. If <exp0> is a symbol that names a special form, then the rules of that
        special form apply.
        
     2. If <exp0> is a symbol bound to a macro, then macroexpansion occurs.
        
     3. Otherwise, <exp0> must evaluate to a function and the rules of function
        application apply.
  */
  
  private Inst compileCompound(List exp, Scope scope, boolean tail, boolean allowDefs, Inst next)
  {
    if (exp.first() instanceof Symbol s) {
      return Optional.ofNullable(specialForms.get(s))
             .map(form -> form.compile(exp, scope, tail, allowDefs, next))
             .or(() -> lookupMacro(s).map(macro -> compile(expand(macro, exp), scope, tail, allowDefs, next)))
             .orElseGet(() -> compileApply(exp, scope, tail, next));
    }
    return compileApply(exp, scope, tail, next);
  }
  
  /* This is the generalized compilation function that dispatches on the type of the expression. */
  
  private Inst compile(Datum exp, Scope scope, boolean tail, boolean allowDefs, Inst next)
  {
    if (isSelfEval(exp))
      return compileSelfEval(exp, next);
    else if (exp instanceof Symbol s)
      return compileVarRef(s, scope, next);
    else 
      return compileCompound((List)exp, scope, tail, allowDefs, next);
  }
  
  private static interface ISpecialForm
  {
    Inst compile(List exp, Scope scope, boolean tail, boolean allowDefs, Inst next);
  }
  
  private final Map<Symbol, ISpecialForm> specialForms = Map.ofEntries(
    Map.entry(Symbol.DEF, this::compileDef),
    Map.entry(Symbol.DEFUN, this::compileDefun),
    Map.entry(Symbol.DEFMACRO, this::compileDefmacro),
    Map.entry(Symbol.FUN, this::compileFun),
    Map.entry(Symbol.IF, this::compileIf),
    Map.entry(Symbol.COND, this::compileCond),
    Map.entry(Symbol.LET, this::compileLet),
    Map.entry(Symbol.LETSTAR, this::compileLetStar),
    Map.entry(Symbol.LETREC, this::compileLetRec),
    Map.entry(Symbol.DO, this::compileDo),
    Map.entry(Symbol.WHILE, this::compileWhile),
    Map.entry(Symbol.SET, this::compileSet),
    Map.entry(Symbol.REP, this::compileRep),
    Map.entry(Symbol.QUOTE, this::compileQuote),
    Map.entry(Symbol.QUASIQUOTE, this::compileQuasiquote),
    Map.entry(Symbol.OR, this::compileOr),
    Map.entry(Symbol.AND, this::compileAnd),
    Map.entry(Symbol.MATCH, this::compileMatch)
  );
  
  private PositionMap positionMap;
  private final VirtualMachine vm;
  private static final Logger log = Logger.getLogger(Compiler.class.getName());
}
