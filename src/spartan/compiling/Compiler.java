package spartan.compiling;

import spartan.runtime.*;
import spartan.data.*;
import spartan.parsing.SourceDatum;
import spartan.parsing.PositionMap;
import spartan.errors.CompileError;

public class Compiler
{
  private PositionMap positionMap;
  
  private static boolean isSelfEval(Datum sexp)
  {
    return sexp.type() == Type.Bool
        || sexp.type() == Type.Int
        || sexp.type() == Type.Real
        || sexp.type() == Type.Vector
        || sexp.type() == Type.Record
        || sexp.type() == Type.Text
        || sexp == List.Empty
        || sexp == Nil.Instance;
  }

  private Inst compileSelfEval(Datum sexp, Inst next)
  {
    return new LoadConst(sexp, next);
  }
  
  private Inst compileQuote(List list, Inst next) throws CompileError
  {
    if (list.length() != 2)
      throw new CompileError("malformed expression", positionMap.get(list));
    
    return new LoadConst(list.cdr().car(), next);
  }
  
  private Inst compileSymbol(Symbol symb, Scope scope, Inst next) throws CompileError
  {
    DeBruijnIndex index = null;
    
    if (scope != null)
      index = scope.lookup(symb);
    
    if (index == null)
      return new LoadGlobal(symb, positionMap.get(symb), next);
    else
      return new LoadLocal(index.depth, index.offset, next);
  }
  
  private Inst compileDef(List list, Scope scope, Inst next) throws CompileError
  {
    if (list.length() != 3 || list.cdr().car().type() != Type.Symbol)
      throw new CompileError("malformed expression", positionMap.get(list));    
    
    Symbol symb = (Symbol)list.cdr().car();
    Datum init = list.cdr().cdr().car();    
    
    return compile(init, scope, false,
           new StoreGlobal(symb, next));
  }
  
  // (defun f (a b c ...) body...)
  
  private Inst compileDefun(List list, Scope scope, Inst next) throws CompileError
  {
    if (list.length() < 4 || list.cdr().car().type() != Type.Symbol)
      throw new CompileError("malformed expression", positionMap.get(list));
    
    return compile(transformDefun(list.cdr()), scope, false, next);
  }
  
  // (defun f (a b c ...) body...)
  //
  // (def f (fun (a b c ...) body...))
  
  private List transformDefun(List list)
  {
    return new List(Symbol.get("def"),
           new List(list.car(),
           new List(new List(Symbol.get("fun"),
                    new List(list.cdr().car(),
                    list.cdr().cdr())),
           List.Empty)));
  }
  
  private Inst compileSequence(List list, Scope scope, boolean tc, Inst next) throws CompileError
  {
    if (list == List.Empty)
      return next;
    else
      return compile(list.car(), scope, (tc && list.cdr() == List.Empty),
             compileSequence(list.cdr(), scope, tc, next));
  }
  
  // (if e1 e2)
  //
  // (if e1 e2 e3)
  
  private Inst compileIf(List list, Scope scope, boolean tc, Inst next) throws CompileError
  {
    int length = list.length();
    
    if (length == 3) {
      Datum cond = list.cdr().car();
      Datum trueBranch = list.cdr().cdr().car();

      return compile(cond, scope, false,
             new Branch(
               compile(trueBranch, scope, tc, next),
               new LoadConst(Nil.Instance, next)));
    }
    else if (length == 4) {
      Datum cond = list.cdr().car();
      Datum trueBranch = list.cdr().cdr().car();
      Datum falseBranch = list.cdr().cdr().cdr().car();
      
      return compile(cond, scope, false,
             new Branch(
               compile(trueBranch, scope, tc, next),
               compile(falseBranch, scope, tc, next)));
    }
    else
      throw new CompileError("malformed expression", positionMap.get(list));
  }
  
  private Inst compileCond(List list, Scope scope, boolean tc, Inst next) throws CompileError
  {
    if (list.length() < 2 || !checkCondClauses(list.cdr()))
      throw new CompileError("malformed expression", positionMap.get(list));
    
    return compileCondClauses(list.cdr(), scope, tc, next);
  }
  
  private boolean checkCondClauses(List list)
  {
    for (; list != List.Empty; list = list.cdr()) {
      if (list.car().type() != Type.List)
        return false;
      List clause = (List)list.car();
      if (clause.length() < 2)
        return false;
    }
    return true;
  }
  
  private Inst compileCondClauses(List list, Scope scope, boolean tc, Inst next) throws CompileError
  {
    if (list == List.Empty)
      return next;
    else {
      List clause = (List)list.car();
      Datum test = clause.car();
      List body = clause.cdr();
      
      return compile(test, scope, false,
             new Branch(compileSequence(body, scope, tc, next),
                        list.cdr() == List.Empty
                          ? new LoadConst(Nil.Instance, next)
                          : compileCondClauses(list.cdr(), scope, tc, next)));
    }
  }
  
  private Inst compileLet(List list, Scope scope, boolean tc, Inst next) throws CompileError
  {
    if (list.length() < 3 || list.cdr().car().type() != Type.List)
      throw new CompileError("malformed expression", positionMap.get(list));
    
    List bindings = (List)list.cdr().car();
    int numBindings = bindings.length();
    List body = list.cdr().cdr();

    if (!checkBindings(bindings))
      throw new CompileError("malformed expression", positionMap.get(list));
    
    return evalInitializers(bindings, scope,
           new PushLocal(numBindings,
           performBindings(0, numBindings,
           compileSequence(body, extendLetScope(bindings, scope), tc,
           new PopLocal(next)))));
  }
  
  private Inst compileLetRec(List list, Scope scope, boolean tc, Inst next) throws CompileError
  {
    if (list.length() < 3 || list.cdr().car().type() != Type.List)
      throw new CompileError("malformed expression", positionMap.get(list));
    
    List bindings = (List)list.cdr().car();
    int numBindings = bindings.length();
    List body = list.cdr().cdr();

    if (!checkBindings(bindings))
      throw new CompileError("malformed expression", positionMap.get(list));
    
    Scope extendedScope = extendLetScope(bindings, scope);
    
    return new PushLocal(numBindings,
           evalInitializers(bindings, extendedScope,
           performBindings(0, numBindings,
           compileSequence(body, extendedScope, tc,
           new PopLocal(next)))));
  }
  
  private Inst compileLetStar(List list, Scope scope, boolean tc, Inst next) throws CompileError
  {
    if (list.length() < 3 || list.cdr().car().type() != Type.List)
      throw new CompileError("malformed expression", positionMap.get(list));
    
    List bindings = (List)list.cdr().car();
    List body = list.cdr().cdr();

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
  
  private Scope extendLetScope(List bindings, Scope parent) throws CompileError
  {
    Scope scope = new Scope(parent);
    for (; bindings != List.Empty; bindings = bindings.cdr()) {
      List binding = (List)bindings.car();
      Symbol symb = (Symbol)binding.car();
      if (!scope.bind(symb))
        throw new CompileError("multiple definition of " + symb.repr(), positionMap.get(symb));
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
  
  private Inst performBindings(int offset, int numBindings, Inst next) throws CompileError
  {
    if (offset >= numBindings)
      return next;
    else
      return new PopArg(
             new StoreLocal(0, offset,
             performBindings(offset + 1, numBindings, next)));
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
  
  private Inst compileApply(List list, Scope scope, boolean tc, Inst next) throws CompileError
  {
    int numArgs = list.length() - 1;
    
    if (tc)
      return compilePushArgs(list.cdr(), scope,
             compile(list.car(), scope, false,
             new Apply(numArgs, positionMap.get(list))));
    else
      return new PushFrame(next,
             compilePushArgs(list.cdr(), scope,
             compile(list.car(), scope, false,
             new Apply(numArgs, positionMap.get(list)))));
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

  private Inst compileFun(List list, Scope scope, Inst next) throws CompileError
  {
    if (list.length() < 3 || list.cdr().car().type() != Type.List)
      throw new CompileError("malformed expression", positionMap.get(list));
    
    List params = (List)list.cdr().car();
    List body = list.cdr().cdr();    
    
    if (!checkParams(params))
      throw new CompileError("malformed expression", positionMap.get(list));
    
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
  
  private Scope extendFunScope(List params, Scope parent) throws CompileError
  {
    Scope scope = new Scope(parent);
    for (; params != List.Empty; params = params.cdr()) {
      Symbol symb = (Symbol)params.car();
      if (!scope.bind(symb))
        throw new CompileError("multiple definition of " + symb.repr(), positionMap.get(symb));
    }
    return scope;
  }
  
  private Inst compilePopArgs(int offset, int numParams, Inst next) throws CompileError
  {
    if (offset == numParams)
      return next;
    else
      return new PopArg(
             new StoreLocal(0, offset,
             compilePopArgs(offset + 1, numParams,
             next)));
  }
  
  private Inst compilePopArgsVariadic(int offset, int numParams, Inst next) throws CompileError
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
  
  private Inst compileOr(List list, Scope scope, boolean tc, Inst next) throws CompileError
  {
    if (list.length() < 2)
      throw new CompileError("malformed expression", positionMap.get(list));
    
    return compileOrArgs(list.cdr(), scope, tc, next);
  }
  
  private Inst compileOrArgs(List list, Scope scope, boolean tc, Inst next) throws CompileError
  {
    if (list == List.Empty)
      return next;
    else
      return compile(list.car(), scope, tc && (list.cdr() == List.Empty),
             new Branch(
               next,
               compileOrArgs(list.cdr(), scope, tc, next)));
  }
  
  // (and a b ...)
  
  private Inst compileAnd(List list, Scope scope, boolean tc, Inst next) throws CompileError
  {
    if (list.length() < 2)
      throw new CompileError("malformed expression", positionMap.get(list));
    
    return compileAndArgs(list.cdr(), scope, tc, next);
  }
  
  private Inst compileAndArgs(List list, Scope scope, boolean tc, Inst next) throws CompileError
  {
    if (list == List.Empty)
      return next;
    else
      return compile(list.car(), scope, tc && (list.cdr() == List.Empty),
             new Branch(
               compileAndArgs(list.cdr(), scope, tc, next),
               next));
  }
  
  private Inst compileDo(List list, Scope scope, boolean tc, Inst next) throws CompileError
  {
    if (list.length() < 2)
      throw new CompileError("malformed expression", positionMap.get(list));
    
    return compileSequence(list.cdr(), scope, tc, next);
  }
  
  // (set! symbol init)
  
  private Inst compileSet(List list, Scope scope, boolean tc, Inst next) throws CompileError
  {
    if (list.length() != 3 || list.cdr().car().type() != Type.Symbol)
      throw new CompileError("malformed expression", positionMap.get(list));
    
    Symbol symb = (Symbol)list.cdr().car();
    Datum init = list.cdr().cdr().car();
    
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
  
  private Inst compileWhile(List list, Scope scope, boolean tc, Inst next) throws CompileError
  {
    if (list.length() < 3)
      throw new CompileError("malformed expression", positionMap.get(list));
    
    Datum test = list.cdr().car();
    List body = list.cdr().cdr();
    Jump jump = new Jump();
    Inst code = compile(test, scope, false,
                new Branch(compileSequence(body, scope, tc, jump),
                           new LoadConst(Nil.Instance, next)));
    jump.setTarget(code);
    return code;
  }
  
  private Inst compileList(List list, Scope scope, boolean tc, Inst next) throws CompileError
  {
    if (list.car() == Symbol.get("if"))
      return compileIf(list, scope, tc, next);
    else if (list.car() == Symbol.get("let"))
      return compileLet(list, scope, tc, next);
    else if (list.car() == Symbol.get("let*"))
      return compileLetStar(list, scope, tc, next);
    else if (list.car() == Symbol.get("letrec"))
      return compileLetRec(list, scope, tc, next);
    else if (list.car() == Symbol.get("def"))
      return compileDef(list, scope, next);
    else if (list.car() == Symbol.get("defun"))
      return compileDefun(list, scope, next);
    else if (list.car() == Symbol.get("fun"))
      return compileFun(list, scope, next);
    else if (list.car() == Symbol.get("quote"))
      return compileQuote(list, next);
    else if (list.car() == Symbol.get("or"))
      return compileOr(list, scope, tc, next);
    else if (list.car() == Symbol.get("and"))
      return compileAnd(list, scope, tc, next);
    else if (list.car() == Symbol.get("cond"))
      return compileCond(list, scope, tc, next);
    else if (list.car() == Symbol.get("do"))
      return compileDo(list, scope, tc, next);
    else if (list.car() == Symbol.get("set!"))
      return compileSet(list, scope, tc, next);
    else if (list.car() == Symbol.get("while"))
      return compileWhile(list, scope, tc, next);
    else
      return compileApply(list, scope, tc, next);
  }
  
  private Inst compile(Datum sexp, Scope scope, boolean tc, Inst next) throws CompileError
  {
    if (isSelfEval(sexp))
      return compileSelfEval(sexp, next);
    else if (sexp.type() == Type.Symbol)
      return compileSymbol((Symbol)sexp, scope, next);
    else // sexp.type() == Type.List
      return compileList((List)sexp, scope, tc, next);
  }
  
  public Inst compile(SourceDatum sourceDatum) throws CompileError
  {
    positionMap = sourceDatum.positionMap;
    return compile(sourceDatum.datum, null, false, null);
  }
}
