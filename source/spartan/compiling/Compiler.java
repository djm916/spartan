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
  private PositionMap positionMap;
  
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
    return exp.type() == Type.Bool
        || exp.type() == Type.Int
        || exp.type() == Type.Real
        || exp.type() == Type.Vector
        || exp.type() == Type.Record
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
    
    return new LoadConst(exp.cdr().car(), next);
  }
  
  private Inst compileVarRef(Symbol symb, Scope scope, Inst next)
  {
    DeBruijnIndex index = null;
    
    if (scope != null)
      index = scope.lookup(symb);
    
    if (index == null)
      return new LoadGlobal(symb, positionMap.get(symb), next);
    else
      return new LoadLocal(index.depth, index.offset, next);
  }
  
  // (def symb init)
  
  private Inst compileDef(List exp, Scope scope, Inst next) throws CompileError
  {
    if (exp.length() != 3 || exp.cdr().car().type() != Type.Symbol)
      throw malformedExp(exp);
    
    Symbol symb = (Symbol)exp.cdr().car();
    Datum init = exp.cdr().cdr().car();    
    
    return compile(init, scope, false,
           new StoreGlobal(symb, next));
  }
  
  // (defun f (a b c ...) body...)
  
  private Inst compileDefun(List exp, Scope scope, Inst next) throws CompileError
  {
    if (exp.length() < 4 || exp.cdr().car().type() != Type.Symbol)
      throw malformedExp(exp);
    
    return compile(transformDefun(exp.cdr()), scope, false, next);
  }
  
  // (if e1 e2)
  //
  // (if e1 e2 e3)
  
  private Inst compileIf(List exp, Scope scope, boolean tc, Inst next) throws CompileError
  {
    int length = exp.length();
    
    if (length < 3 || length > 4)
      throw malformedExp(exp);
    
    Datum test = exp.cdr().car();
    Datum ifTrue = exp.cdr().cdr().car();
    Datum ifFalse = length == 3 ? Nil.Instance
                                : exp.cdr().cdr().cdr().car();
    
    return compile(test, scope, false,
           new Branch(compile(ifTrue, scope, tc, next),
                      compile(ifFalse, scope, tc, next)));
  }
  
  // (cond (test body...)...)
  
  private Inst compileCond(List exp, Scope scope, boolean tc, Inst next) throws CompileError
  {
    if (exp.length() < 2 || !checkCondClauses(exp.cdr()))
      throw malformedExp(exp);
    
    return compileCondClauses(exp.cdr(), scope, tc, next);
  }
  
  private boolean checkCondClauses(List exp)
  {
    for (; exp != List.Empty; exp = exp.cdr()) {
      if (exp.car().type() != Type.List)
        return false;
      if (((List)exp.car()).length() < 2)
        return false;
    }
    return true;
  }
  
  private Inst compileCondClauses(List exp, Scope scope, boolean tc, Inst next) throws CompileError
  {
    if (exp == List.Empty)
      return new LoadConst(Nil.Instance, next);
    else {
      List clause = (List)exp.car();
      Datum test = clause.car();
      List body = clause.cdr();
      
      return compile(test, scope, false,
             new Branch(compileSequence(body, scope, tc, next),
                        compileCondClauses(exp.cdr(), scope, tc, next)));
    }
  }
  
  private Inst compileLet(List exp, Scope scope, boolean tc, Inst next) throws CompileError
  {
    if (exp.length() < 3 || exp.cdr().car().type() != Type.List)
      throw malformedExp(exp);
    
    List bindings = (List)exp.cdr().car();
    int numBindings = bindings.length();
    List body = exp.cdr().cdr();

    if (!checkBindings(bindings))
      throw malformedExp(bindings);
    
    return evalInitializers(bindings, scope,
           new PushLocal(numBindings,
           performBindings(0, numBindings,
           compileSequence(body, extendLetScope(bindings, scope), tc,
           new PopLocal(next)))));
  }
  
  private Inst compileLetRec(List exp, Scope scope, boolean tc, Inst next) throws CompileError
  {
    if (exp.length() < 3 || exp.cdr().car().type() != Type.List)
      throw malformedExp(exp);
    
    List bindings = (List)exp.cdr().car();
    int numBindings = bindings.length();
    List body = exp.cdr().cdr();

    if (!checkBindings(bindings))
      throw malformedExp(exp);
    
    Scope extendedScope = extendLetScope(bindings, scope);
    
    return new PushLocal(numBindings,
           evalInitializers(bindings, extendedScope,
           performBindings(0, numBindings,
           compileSequence(body, extendedScope, tc,
           new PopLocal(next)))));
  }
  
  private Inst compileLetStar(List exp, Scope scope, boolean tc, Inst next) throws CompileError
  {
    if (exp.length() < 3 || exp.cdr().car().type() != Type.List)
      throw malformedExp(exp);
    
    List bindings = (List)exp.cdr().car();
    List body = exp.cdr().cdr();

    return compile(transformLetStar(bindings, body), scope, tc, next);
  }
  
  private boolean checkBindings(List bindings)
  {
    for (; bindings != List.Empty; bindings = bindings.cdr()) {
      if (bindings.car().type() != Type.List)
        return false;
      List binding = (List)bindings.car();
      if (binding.length() != 2 || binding.car().type() != Type.Symbol)
        return false;
    }
    return true;
  }
  
  private Scope extendLetScope(List bindings, Scope parent) throws MultipleDefinition
  {
    Scope scope = new Scope(parent);
    for (; bindings != List.Empty; bindings = bindings.cdr()) {
      List binding = (List)bindings.car();
      Symbol symb = (Symbol)binding.car();
      if (!scope.bind(symb))
        throw multipleDef(symb);
    }
    return scope;
  }
  
  private Inst evalInitializers(List bindings, Scope scope, Inst next) throws CompileError
  {
    if (bindings == List.Empty)
      return next;
    else {
      List binding = (List)bindings.car();
      return evalInitializers(bindings.cdr(), scope,
             compile(binding.cdr().car(), scope, false,
             new PushArg(next)));
    }
  }
  
  private Inst performBindings(int offset, int numBindings, Inst next)
  {
    if (offset >= numBindings)
      return next;
    else
      return new PopArg(
             new StoreLocal(0, offset,
             performBindings(offset + 1, numBindings, next)));
  }
  
  // (defun f (a b c ...) body...)
  //
  // (def f (fun (a b c ...) body...))
  
  private List transformDefun(List exp)
  {
    return new List(Symbol.get("def"),
           new List(exp.car(),
           new List(new List(Symbol.get("fun"),
                    new List(exp.cdr().car(),
                    exp.cdr().cdr())),
           List.Empty)));
  }
  
  private List transformLetStar(List bindings, List body)
  {
    if (bindings == List.Empty)
      return new List(Symbol.get("let"), new List(List.Empty, body));
    else
      return new List(Symbol.get("let"),
             new List(new List(bindings.car(), List.Empty),
             bindings.cdr() == List.Empty
               ? body
               : new List(transformLetStar(bindings.cdr(), body), List.Empty)));
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
  
  private Inst compileApply(List exp, Scope scope, boolean tc, Inst next) throws CompileError
  {
    int numArgs = exp.length() - 1;
    
    if (tc)
      return compilePushArgs(exp.cdr(), scope,
             compile(exp.car(), scope, false,
             new Apply(numArgs, positionMap.get(exp))));
    else
      return new PushFrame(next,
             compilePushArgs(exp.cdr(), scope,
             compile(exp.car(), scope, false,
             new Apply(numArgs, positionMap.get(exp)))));
  }

  private Inst compilePushArgs(List args, Scope scope, Inst next) throws CompileError
  {
    if (args == List.Empty)
      return next;
    else
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
    if (exp.length() < 3 || exp.cdr().car().type() != Type.List)
      throw malformedExp(exp);
    
    List params = (List)exp.cdr().car();
    List body = exp.cdr().cdr();    
    
    if (!checkParams(params))
      throw malformedExp(exp);
    
    int numParams = params.length();
    boolean isVariadic = numParams != 0 && isRestParam((Symbol)params.at(numParams - 1));

    Inst code = isVariadic ? new PushLocal(numParams,
                             compilePopArgsVariadic(0, numParams,
                             compileSequence(body, extendFunScope(params, scope), true,
                             new PopFrame())))
                           
                           : new PushLocal(numParams,
                             compilePopArgs(0, numParams,
                             compileSequence(body, extendFunScope(params, scope), true,
                             new PopFrame())));

    int requiredArgs = isVariadic ? numParams - 1 : numParams;
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
      if (isRestParam((Symbol)params.car()) && params.cdr() != List.Empty)
        return false;
    }
    return true;
  }
  
  private Scope extendFunScope(List params, Scope parent) throws MultipleDefinition
  {
    Scope scope = new Scope(parent);
    for (; params != List.Empty; params = params.cdr()) {
      Symbol symb = (Symbol)params.car();
      if (!scope.bind(symb))
        throw multipleDef(symb);
    }
    return scope;
  }
  
  private Inst compilePopArgs(int offset, int numParams, Inst next)
  {
    if (offset == numParams)
      return next;
    else
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
    else
      return new PopArg(
             new StoreLocal(0, offset,
             compilePopArgsVariadic(offset + 1, numParams,
             next)));
  }
  
  // (or a b ...)
  
  private Inst compileOr(List exp, Scope scope, boolean tc, Inst next) throws CompileError
  {
    if (exp.length() < 2)
      throw malformedExp(exp);
    
    return compileOrArgs(exp.cdr(), scope, tc, next);
  }
  
  private Inst compileOrArgs(List exp, Scope scope, boolean tc, Inst next) throws CompileError
  {
    if (exp == List.Empty)
      return next;
    else
      return compile(exp.car(), scope, tc && (exp.cdr() == List.Empty),
             new Branch(next,
                        compileOrArgs(exp.cdr(), scope, tc, next)));
  }
  
  // (and a b ...)
  
  private Inst compileAnd(List exp, Scope scope, boolean tc, Inst next) throws CompileError
  {
    if (exp.length() < 2)
      throw malformedExp(exp);
    
    return compileAndArgs(exp.cdr(), scope, tc, next);
  }
  
  private Inst compileAndArgs(List exp, Scope scope, boolean tc, Inst next) throws CompileError
  {
    if (exp == List.Empty)
      return next;
    else
      return compile(exp.car(), scope, tc && (exp.cdr() == List.Empty),
             new Branch(compileAndArgs(exp.cdr(), scope, tc, next),
                        next));
  }
  
  private Inst compileDo(List exp, Scope scope, boolean tc, Inst next) throws CompileError
  {
    if (exp.length() < 2)
      throw malformedExp(exp);
    
    return compileSequence(exp.cdr(), scope, tc, next);
  }
  
  private Inst compileSequence(List exp, Scope scope, boolean tc, Inst next) throws CompileError
  {
    if (exp == List.Empty)
      return next;
    else
      return compile(exp.car(), scope, (tc && exp.cdr() == List.Empty),
             compileSequence(exp.cdr(), scope, tc, next));
  }
  
  // (set! symbol init)
  
  private Inst compileSet(List exp, Scope scope, boolean tc, Inst next) throws CompileError
  {
    if (exp.length() != 3 || exp.cdr().car().type() != Type.Symbol)
      throw malformedExp(exp);
    
    Symbol symb = (Symbol)exp.cdr().car();
    Datum init = exp.cdr().cdr().car();
    
    DeBruijnIndex index = null;
    
    if (scope != null)
      index = scope.lookup(symb);
    
    if (index == null)
      return compile(init, scope, false,
             new StoreGlobal(symb,
             next));
    else
      return compile(init, scope, false,
             new StoreLocal(index.depth, index.offset,
             next));
  }
  
  // (while test body...)
  
  private Inst compileWhile(List exp, Scope scope, boolean tc, Inst next) throws CompileError
  {
    if (exp.length() < 3)
      throw malformedExp(exp);
    
    Datum test = exp.cdr().car();
    List body = exp.cdr().cdr();
    Jump jump = new Jump();
    Inst code = compile(test, scope, false,
                new Branch(compileSequence(body, scope, tc, jump),
                           new LoadConst(Nil.Instance, next)));
    jump.setTarget(code);
    return code;
  }
  
  // (delay exp...)
  
  private Inst compileDelay(List exp, Scope scope, boolean tc, Inst next) throws CompileError
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
  
  private Inst compileCombination(List exp, Scope scope, boolean tc, Inst next) throws CompileError
  {
    if (exp.car() == Symbol.get("if"))
      return compileIf(exp, scope, tc, next);
    else if (exp.car() == Symbol.get("let"))
      return compileLet(exp, scope, tc, next);
    else if (exp.car() == Symbol.get("let*"))
      return compileLetStar(exp, scope, tc, next);
    else if (exp.car() == Symbol.get("letrec"))
      return compileLetRec(exp, scope, tc, next);
    else if (exp.car() == Symbol.get("def"))
      return compileDef(exp, scope, next);
    else if (exp.car() == Symbol.get("defun"))
      return compileDefun(exp, scope, next);
    else if (exp.car() == Symbol.get("fun"))
      return compileFun(exp, scope, next);
    else if (exp.car() == Symbol.get("quote"))
      return compileQuote(exp, next);
    else if (exp.car() == Symbol.get("or"))
      return compileOr(exp, scope, tc, next);
    else if (exp.car() == Symbol.get("and"))
      return compileAnd(exp, scope, tc, next);
    else if (exp.car() == Symbol.get("cond"))
      return compileCond(exp, scope, tc, next);
    else if (exp.car() == Symbol.get("do"))
      return compileDo(exp, scope, tc, next);
    else if (exp.car() == Symbol.get("set!"))
      return compileSet(exp, scope, tc, next);
    else if (exp.car() == Symbol.get("while"))
      return compileWhile(exp, scope, tc, next);
    else if (exp.car() == Symbol.get("delay"))
      return compileDelay(exp, scope, tc, next);
    else
      return compileApply(exp, scope, tc, next);
  }
  
  private Inst compile(Datum exp, Scope scope, boolean tc, Inst next) throws CompileError
  {
    if (isSelfEval(exp))
      return compileSelfEval(exp, next);
    else if (exp.type() == Type.Symbol)
      return compileVarRef((Symbol)exp, scope, next);
    else
      return compileCombination((List)exp, scope, tc, next);
  }
  
  public Inst compile(SourceDatum sourceDatum) throws CompileError
  {
    positionMap = sourceDatum.positionMap;
    return compile(sourceDatum.datum, null, false, null);
  }
}
