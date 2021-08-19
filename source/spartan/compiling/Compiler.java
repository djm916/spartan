package spartan.compiling;

import spartan.runtime.*;
import spartan.data.*;
import spartan.parsing.SourceDatum;
import spartan.parsing.PositionMap;
import spartan.errors.CompileError;
import spartan.errors.MalformedExpression;
import spartan.errors.MultipleDefinition;

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
  
  // (defun f (a b c ...) body...)
  
  private Inst compileDefun(List exp, Scope scope, Inst next) throws CompileError
  {
    if (exp.length() < 4 || exp.cadr().type() != Type.Symbol)
      throw malformedExp(exp);
    
    return compile(transformDefun(exp.cdr()), scope, false, next);
  }
  
  // (if test sub)
  //
  // (if test sub alt)
  
  private Inst compileIf(List exp, Scope scope, boolean tail, Inst next) throws CompileError
  {
    int length = exp.length();
    
    if (length < 3 || length > 4)
      throw malformedExp(exp);
    
    var test = exp.cadr();
    var sub = exp.caddr();
    var alt = length == 3 ? Nil.Instance : exp.cadddr();
    
    return compile(test, scope, false,
           new Branch(compile(sub, scope, tail, next),
                      compile(alt, scope, tail, next)));
  }
  
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
  
  private Inst compileLet(List exp, Scope scope, boolean tail, Inst next) throws CompileError
  {
    if (exp.length() < 3 || exp.cadr().type() != Type.List)
      throw malformedExp(exp);
    
    var bindings = (List) exp.cadr();
    var body = exp.cddr();
    
    if (!checkLetBindings(bindings))
      throw malformedExp(bindings);
    
    int numBindings = bindings.length();

    return evalInitializers(bindings, scope,
           new PushLocal(numBindings,
           performBindings(0, numBindings,
           compileSequence(body, extendLetScope(bindings, scope), tail,
           new PopLocal(next)))));
  }
  
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
    
    return new PushLocal(numBindings,
           evalInitializers(bindings, scope,
           performBindings(0, numBindings,
           compileSequence(body, scope, tail,
           new PopLocal(next)))));
  }
  
  private Inst compileLetStar(List exp, Scope scope, boolean tail, Inst next) throws CompileError
  {
    if (exp.length() < 3 || exp.cadr().type() != Type.List)
      throw malformedExp(exp);
    
    var bindings = (List) exp.cadr();
    var body = exp.cddr();
    
    if (!checkLetBindings(bindings))
      throw malformedExp(bindings);
    
    return compile(transformLetStar(bindings, body), scope, tail, next);
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
  
  private Inst evalInitializers(List bindings, Scope scope, Inst next) throws CompileError
  {
    if (bindings == List.Empty)
      return next;

    var binding = (List) bindings.car();
    
    return evalInitializers(bindings.cdr(), scope,
           compile(binding.cadr(), scope, false,
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
  
  // (defun f (a b c ...) body...)
  //
  // (def f (fun (a b c ...) body...))
  
  private List transformDefun(List exp)
  {
    return List.of(Symbol.get("def"),
                   exp.car(),
                   List.cons(Symbol.get("fun"), List.cons(exp.cadr(), exp.cdr().cdr())));
                           
  }
  
  private List transformLetStar(List bindings, List body)
  {
    return List.cons(Symbol.get("let"),
           List.cons(List.of(bindings.car()),
           bindings.cdr() == List.Empty ? body
                                        : List.of(transformLetStar(bindings.cdr(), body))));
  }
  
  // (f a1 a2 ... aN)
  //
  // push-frame
  // <<aN>>
  // push-arg
  // ...
  // <<a1>>
  // push-arg
  // <<f>>
  // apply
  
  private Inst compileApply(List exp, Scope scope, boolean tail, Inst next) throws CompileError
  {
    int numArgs = exp.length() - 1;
    
    return tail ? compilePushArgs(exp.cdr(), scope,
                compile(exp.car(), scope, false,
                new Apply(numArgs, positionMap.get(exp))))
        
              : new PushFrame(next,
                compilePushArgs(exp.cdr(), scope,
                compile(exp.car(), scope, false,
                new Apply(numArgs, positionMap.get(exp)))));
  }

  private Inst compilePushArgs(List args, Scope scope, Inst next) throws CompileError
  {
    if (args == List.Empty)
      return next;

    return compilePushArgs(args.cdr(), scope,
           compile(args.car(), scope, false,
           new PushArg(next)));
  }
  
  // (fun (p1 p2 ... pN) body ...)
  //
  // push-local N
  // pop-arg
  // store-local 0
  // ...
  // pop-arg
  // store-local N-1
  // <<body>>
  // pop-frame
 
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
    
    var code = isVariadic  ? new PushLocal(numParams,
                             compilePopArgsVariadic(0, numParams,
                             compileSequence(body, extendFunScope(params, scope), true,
                             new PopFrame())))
                           
                           : new PushLocal(numParams,
                             compilePopArgs(0, numParams,
                             compileSequence(body, extendFunScope(params, scope), true,
                             new PopFrame())));
   
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
  
  private Inst compilePopArgs(int offset, int numParams, Inst next)
  {
    if (offset == numParams)
      return next;

    return new PopArg(
           new StoreLocal(0, offset,
           compilePopArgs(offset + 1, numParams,
           next)));
  }
  
  private Inst compilePopArgsVariadic(int offset, int numParams, Inst next)
  {
    if (offset == numParams - 1)
      return new PopArgs(
             new StoreLocal(0, offset,
             next));
    
    return new PopArg(
           new StoreLocal(0, offset,
           compilePopArgsVariadic(offset + 1, numParams,
           next)));
  }
  
  // (or a b ...)
  
  private Inst compileOr(List exp, Scope scope, boolean tail, Inst next) throws CompileError
  {
    if (exp.length() < 2)
      throw malformedExp(exp);
    
    return compileOrArgs(exp.cdr(), scope, tail, next);
  }
  
  private Inst compileOrArgs(List exp, Scope scope, boolean tail, Inst next) throws CompileError
  {
    if (exp == List.Empty)
      return next;

    return compile(exp.car(), scope, tail && (exp.cdr() == List.Empty),
           new Branch(next,
                      compileOrArgs(exp.cdr(), scope, tail, next)));
  }
  
  // (and a b ...)
  
  private Inst compileAnd(List exp, Scope scope, boolean tail, Inst next) throws CompileError
  {
    if (exp.length() < 2)
      throw malformedExp(exp);
    
    return compileAndArgs(exp.cdr(), scope, tail, next);
  }
  
  private Inst compileAndArgs(List exp, Scope scope, boolean tail, Inst next) throws CompileError
  {
    if (exp == List.Empty)
      return next;

    return compile(exp.car(), scope, tail && (exp.cdr() == List.Empty),
           new Branch(compileAndArgs(exp.cdr(), scope, tail, next),
                      next));
  }
  
  private Inst compileDo(List exp, Scope scope, boolean tail, Inst next) throws CompileError
  {
    if (exp.length() < 2)
      throw malformedExp(exp);
    
    return compileSequence(exp.cdr(), scope, tail, next);
  }
  
  private Inst compileSequence(List exp, Scope scope, boolean tail, Inst next) throws CompileError
  {
    if (exp == List.Empty)
      return next;

    return compile(exp.car(), scope, (tail && exp.cdr() == List.Empty),
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
  
  // (while test body...)
  
  private Inst compileWhile(List exp, Scope scope, boolean tail, Inst next) throws CompileError
  {
    if (exp.length() < 3)
      throw malformedExp(exp);
    
    Datum test = exp.cadr();
    List body = exp.cddr();
    Jump jump = new Jump();
    Inst code = compile(test, scope, false,
                new Branch(compileSequence(body, scope, tail, jump),
                           new LoadConst(Nil.Instance, next)));
    jump.setTarget(code);
    return code;
  }
  
  // (delay exp...)
  
  private Inst compileDelay(List exp, Scope scope, boolean tail, Inst next) throws CompileError
  {
    if (exp.length() < 2)
      throw malformedExp(exp);
    
    Inst body = new LoadLocal(0, 0,
                new Branch(new PopFrame(),
                           compileSequence(exp.cdr(), new Scope(scope), false,
                           new StoreLocal(0, 0,
                           new PopFrame()))));
    
    return new PushLocal(1,
           new LoadConst(Nil.Instance,
           new StoreLocal(0, 0,
           new MakePromise(body,
           new PopLocal(
           next)))));
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
    else if (exp.car() == Symbol.get("fun"))
      return compileFun(exp, scope, next);
    else if (exp.car() == Symbol.get("quote"))
      return compileQuote(exp, next);
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
}
