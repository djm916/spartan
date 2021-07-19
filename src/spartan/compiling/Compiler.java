package spartan.compiling;

import spartan.runtime.*;
import spartan.data.*;
import spartan.parsing.SourceValue;
import spartan.parsing.PositionMap;
import spartan.Position;
import spartan.errors.CompileError;

public class Compiler
{
  private PositionMap positionMap;
  
  private static boolean isSelfEval(Value sexp)
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

  private Inst compileSelfEval(Value sexp, Inst next)
  {
    return new LoadConst(sexp, next);
  }
  
  private Inst compileQuote(List list, Inst next) throws CompileError
  {
    if (list.length() != 2)
      throw new CompileError("malformed expression", positionMap.get(list));
    
    return new LoadConst(list.rest.first, next);
  }
  
  private Inst compileSymbol(Symbol symb, Scope scope, Inst next) throws CompileError
  {
    DeBruijnIndex index = null;
    
    if (scope != null)
      index = scope.lookup(symb);
    
    if (index == null)
      return new LoadGlobal(positionMap.get(symb), symb, next);
    else
      return new LoadLocal(index.depth, index.offset, next);
  }
  
  private Inst compileDefine(List list, Scope scope, Inst next) throws CompileError
  {
    if (list.length() != 3 || list.rest.first.type() != Type.Symbol)
      throw new CompileError("malformed expression", positionMap.get(list));    
    
    Symbol symb = (Symbol)list.rest.first;
    Value init = list.rest.rest.first;    
    
    return compile(init, scope, false,
           new StoreGlobal(symb, next));
  }
  
  private Inst compileSequence(List list, Scope scope, boolean tc, Inst next) throws CompileError
  {
    if (list == List.Empty)
      return next;
    else
      return compile(list.first, scope, (tc && list.rest == List.Empty),
             compileSequence(list.rest, scope, tc, next));
  }
  
  // (if e1 e2)
  //
  // (if e1 e2 e3)
  
  private Inst compileIf(List list, Scope scope, boolean tc, Inst next) throws CompileError
  {
    int length = list.length();
    
    if (length == 3) {
      Value cond = list.rest.first;
      Value trueBranch = list.rest.rest.first;

      return compile(cond, scope, false,
             new Branch(
               compile(trueBranch, scope, tc, next),
               new LoadConst(Nil.Instance, next)));
    }
    else if (length == 4) {
      Value cond = list.rest.first;
      Value trueBranch = list.rest.rest.first;
      Value falseBranch = list.rest.rest.rest.first;
      
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
    if (list.length() < 2 || !checkCondClauses(list.rest))
      throw new CompileError("malformed expression", positionMap.get(list));
    
    return compileCondClauses(list.rest, scope, tc, next);
  }
  
  private boolean checkCondClauses(List list)
  {
    for (; list != List.Empty; list = list.rest) {
      if (list.first.type() != Type.List)
        return false;
      List clause = (List)list.first;
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
      List clause = (List)list.first;
      Value test = clause.first;
      List body = clause.rest;
      
      return compile(test, scope, false,
             new Branch(compileSequence(body, scope, tc, next),
                        list.rest == List.Empty
                          ? new LoadConst(Nil.Instance, next)
                          : compileCondClauses(list.rest, scope, tc, next)));
    }
  }
  
  private Inst compileLet(List list, Scope scope, boolean tc, Inst next) throws CompileError
  {
    if (list.length() < 3 || list.rest.first.type() != Type.List)
      throw new CompileError("malformed expression", positionMap.get(list));
    
    List bindings = (List)list.rest.first;
    int numBindings = bindings.length();
    List body = list.rest.rest;

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
    if (list.length() < 3 || list.rest.first.type() != Type.List)
      throw new CompileError("malformed expression", positionMap.get(list));
    
    List bindings = (List)list.rest.first;
    int numBindings = bindings.length();
    List body = list.rest.rest;

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
    if (list.length() < 3 || list.rest.first.type() != Type.List)
      throw new CompileError("malformed expression", positionMap.get(list));
    
    List bindings = (List)list.rest.first;
    List body = list.rest.rest;

    return compile(transformLetStar(bindings, body), scope, tc, next);
  }
  
  private boolean checkBindings(List bindings)
  {
    for (; bindings != List.Empty; bindings = bindings.rest) {
      if (bindings.first.type() != Type.List)
        return false;
      List binding = (List)bindings.first;
      if (binding.length() != 2 || binding.first.type() != Type.Symbol)
        return false;
    }
    return true;
  }
  
  private Scope extendLetScope(List bindings, Scope parent) throws CompileError
  {
    Scope scope = new Scope(parent);
    for (; bindings != List.Empty; bindings = bindings.rest) {
      List binding = (List)bindings.first;
      Symbol symb = (Symbol)binding.first;
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
      List binding = (List)bindings.first;
      return evalInitializers(bindings.rest, scope,
             compile(binding.rest.first, scope, false,
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
             new List(new List(bindings.first, List.Empty),
             bindings.rest == List.Empty
               ? body
               : new List(transformLetStar(bindings.rest, body), List.Empty)));
  }
  
  // (f a1 a2 ... aN)
  // push-frame
  // <<aN>>
  // push-arg
  // ...
  // <<a1>>
  // push-arg
  // <<f>>
  // push-arg
  // apply
  
  private Inst compileApply(List list, Scope scope, boolean tc, Inst next) throws CompileError
  {
    int numArgs = list.length() - 1;
    
    if (tc)
      return compilePushArgs(list, scope,
             new Apply(numArgs, positionMap.get(list)));
    else
      return new PushFrame(next,
             compilePushArgs(list, scope,
             new Apply(numArgs, positionMap.get(list))));
  }

  private Inst compilePushArgs(List args, Scope scope, Inst next) throws CompileError
  {
    if (args == List.Empty)
      return next;
    else
      return compilePushArgs(args.rest, scope,
             compile(args.first, scope, false,
             new PushArg(next)));
  }
  
  // (fun (p1 p2 ... pN) body ...)
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
    if (list.length() < 3 || list.rest.first.type() != Type.List)
      throw new CompileError("malformed expression", positionMap.get(list));
    
    List params = (List)list.rest.first;
    int numParams = params.length();
    List body = list.rest.rest;
    
    if (!checkParams(params))
      throw new CompileError("malformed expression", positionMap.get(params));
    
    Inst code = new PushLocal(numParams,
                compilePopArgs(0, numParams,
                compileSequence(body, extendFunScope(params, scope), true,
                new PopFrame())));
    
    return new MakeClosure(code, numParams, next);
  }
  
  private boolean checkParams(List params)
  {
    for (; params != List.Empty; params = params.rest)
      if (params.first.type() != Type.Symbol)
        return false;
    return true;
  }
  
  private Scope extendFunScope(List params, Scope parent) throws CompileError
  {
    Scope scope = new Scope(parent);
    for (; params != List.Empty; params = params.rest) {
      Symbol symb = (Symbol)params.first;
      if (!scope.bind(symb))
        throw new CompileError("multiple definition of " + symb.repr(), positionMap.get(symb));
    }
    return scope;
  }
  
  private Inst compilePopArgs(int offset, int numParams, Inst next) throws CompileError
  {
    if (offset >= numParams)
      return next;
    else
      return new PopArg(
             new StoreLocal(0, offset,
             compilePopArgs(offset + 1, numParams,
             next)));
  }
  
  // (or a b ...)
  
  private Inst compileOr(List list, Scope scope, boolean tc, Inst next) throws CompileError
  {
    if (list.length() < 2)
      throw new CompileError("malformed expression", positionMap.get(list));
    
    return compileOrArgs(list.rest, scope, tc, next);
  }
  
  private Inst compileOrArgs(List list, Scope scope, boolean tc, Inst next) throws CompileError
  {
    if (list == List.Empty)
      return next;
    else
      return compile(list.first, scope, tc && (list.rest == List.Empty),
             new Branch(
               next,
               compileOrArgs(list.rest, scope, tc, next)));
  }
  
  // (and a b ...)
  
  private Inst compileAnd(List list, Scope scope, boolean tc, Inst next) throws CompileError
  {
    if (list.length() < 2)
      throw new CompileError("malformed expression", positionMap.get(list));
    
    return compileAndArgs(list.rest, scope, tc, next);
  }
  
  private Inst compileAndArgs(List list, Scope scope, boolean tc, Inst next) throws CompileError
  {
    if (list == List.Empty)
      return next;
    else
      return compile(list.first, scope, tc && (list.rest == List.Empty),
             new Branch(
               compileAndArgs(list.rest, scope, tc, next),
               next));
  }
  
  private Inst compileDo(List list, Scope scope, boolean tc, Inst next) throws CompileError
  {
    if (list.length() < 2)
      throw new CompileError("malformed expression", positionMap.get(list));
    
    return compileSequence(list.rest, scope, tc, next);
  }
  
  // (set! symbol init)
  
  private Inst compileSet(List list, Scope scope, boolean tc, Inst next) throws CompileError
  {
    if (list.length() != 3 || list.rest.first.type() != Type.Symbol)
      throw new CompileError("malformed expression", positionMap.get(list));
    
    Symbol symb = (Symbol)list.rest.first;
    Value init = list.rest.rest.first;
    
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
  
  private Inst compileList(List list, Scope scope, boolean tc, Inst next) throws CompileError
  {
    if (list.first == Symbol.get("if"))
      return compileIf(list, scope, tc, next);
    else if (list.first == Symbol.get("let"))
      return compileLet(list, scope, tc, next);
    else if (list.first == Symbol.get("let*"))
      return compileLetStar(list, scope, tc, next);
    else if (list.first == Symbol.get("letrec"))
      return compileLetRec(list, scope, tc, next);
    else if (list.first == Symbol.get("define"))
      return compileDefine(list, scope, next);
    else if (list.first == Symbol.get("fun"))
      return compileFun(list, scope, next);
    else if (list.first == Symbol.get("quote"))
      return compileQuote(list, next);
    else if (list.first == Symbol.get("or"))
      return compileOr(list, scope, tc, next);
    else if (list.first == Symbol.get("and"))
      return compileAnd(list, scope, tc, next);
    else if (list.first == Symbol.get("cond"))
      return compileCond(list, scope, tc, next);
    else if (list.first == Symbol.get("do"))
      return compileDo(list, scope, tc, next);
    else if (list.first == Symbol.get("set!"))
      return compileSet(list, scope, tc, next);
    else
      return compileApply(list, scope, tc, next);
  }
  
  private Inst compile(Value sexp, Scope scope, boolean tc, Inst next) throws CompileError
  {
    if (isSelfEval(sexp))
      return compileSelfEval(sexp, next);
    else if (sexp.type() == Type.Symbol)
      return compileSymbol((Symbol)sexp, scope, next);
    else // sexp.type() == Type.List
      return compileList((List)sexp, scope, tc, next);
  }
  
  public Inst compile(SourceValue sourceValue) throws CompileError
  {
    positionMap = sourceValue.positionMap;
    return compile(sourceValue.value, null, false, null);
  }
}
