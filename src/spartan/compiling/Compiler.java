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
        || sexp == List.Empty;
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
    else {
      //System.out.println("Lookup to " + symb.repr() + " at (" + index.depth + " " + index.offset + ")");
      return new LoadLocal(index.depth, index.offset, next);
    }
  }
  
  private Inst compileDefine(List list, Scope scope, Inst next) throws CompileError
  {
    if (list.length() != 3 || list.rest.first.type() != Type.Symbol)
      throw new CompileError("malformed expression", positionMap.get(list));
    
    Symbol symb = (Symbol)list.rest.first;
    Value init = list.rest.rest.first;
    
    return compile(init, scope, new StoreGlobal(symb, next));
  }
  
  private Inst compileSequence(List list, Scope scope, Inst next) throws CompileError
  {
    if (list == List.Empty)
      return next;
    else
      return compile(list.first, scope,
             compileSequence(list.rest, scope, next));
  }
  
  private Inst compileIf(List list, Scope scope, Inst next) throws CompileError
  {
    if (list.length() != 4)
      throw new CompileError("malformed expression", positionMap.get(list));
    
    Value cond = list.rest.first;
    Value trueBranch = list.rest.rest.first;
    Value falseBranch = list.rest.rest.rest.first;
    
    return compile(cond, scope,
           new Branch(positionMap.get(list),
           compile(trueBranch, scope, next),
           compile(falseBranch, scope, next)));
  }
  
  private Inst compileLet(List list, Scope scope, Inst next) throws CompileError
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
           compileSequence(body, extendLetScope(bindings, scope),
           new PopLocal(next)))));
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
  
  private Inst performBindings(int offset, int numBindings, Inst next) throws CompileError
  {
    if (offset >= numBindings)
      return next;
    else
      return new PopArg(
             new StoreLocal(0, offset,
             performBindings(offset + 1, numBindings, next)));
  }
  
  private Inst evalInitializers(List bindings, Scope scope, Inst next) throws CompileError
  {
    if (bindings == List.Empty)
      return next;
    else {
      List binding = (List)bindings.first;
      return evalInitializers(bindings.rest, scope,
             compile(binding.rest.first, scope,
             new PushArg(next)));
    }
  }
  
  private Inst compileLetStar(List list, Scope scope, Inst next) throws CompileError
  {
    if (list.length() < 3 || list.rest.first.type() != Type.List)
      throw new CompileError("malformed expression", positionMap.get(list));
    
    List bindings = (List)list.rest.first;
    List body = list.rest.rest;

    List xform = transformLetStar(bindings, body);
    
    System.out.println("let* transform = " + xform.repr());
    
    return compile(xform, scope, next);
  }
  
  private List transformLetStar(List bindings, List body)
  {
    if (bindings == List.Empty)
      return body;
    else
      return new List(Symbol.get("let"),
             new List(new List(bindings.first, List.Empty),
               bindings.rest == List.Empty
                 ? body
                 : new List(transformLetStar(bindings.rest, body), List.Empty)));
  }
  
  private Inst compileLetRec(List list, Scope scope, Inst next) throws CompileError
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
           compileSequence(body, extendedScope,
           new PopLocal(next)))));
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
  
  private Inst compileApply(List list, Scope scope, Inst next) throws CompileError
  {
    return new PushFrame(next,
           compilePushArgs(list, scope,
           new Apply(list.length() - 1, positionMap.get(list))));
  }
  
  private Inst compilePushArgs(List args, Scope scope, Inst next) throws CompileError
  {
    if (args == List.Empty)
      return next;
    else
      return compilePushArgs(args.rest, scope,
             compile(args.first, scope,
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
                compilePopArgs(params, 0,
                compileSequence(body, extendFunScope(params, scope),
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
  
  private Inst compilePopArgs(List params, int offset, Inst next) throws CompileError
  {
    if (params == List.Empty)
      return next;
    else
      return new PopArg(
             new StoreLocal(0, offset,
             compilePopArgs(params.rest, offset + 1,
             next)));
  }
  
  private Inst compileList(List list, Scope scope, Inst next) throws CompileError
  {
    if (list.first == Symbol.get("if"))
      return compileIf(list, scope, next);
    else if (list.first == Symbol.get("let"))
      return compileLet(list, scope, next);
    else if (list.first == Symbol.get("let*"))
      return compileLetStar(list, scope, next);
    else if (list.first == Symbol.get("letrec"))
      return compileLetRec(list, scope, next);
    else if (list.first == Symbol.get("define"))
      return compileDefine(list, scope, next);
    else if (list.first == Symbol.get("fun"))
      return compileFun(list, scope, next);
    else if (list.first == Symbol.get("quote"))
      return compileQuote(list, next);
    else
      return compileApply(list, scope, next);
  }
  
  private Inst compile(Value sexp, Scope scope, Inst next) throws CompileError
  {
    if (isSelfEval(sexp))
      return compileSelfEval(sexp, next);
    else if (sexp.type() == Type.Symbol)
      return compileSymbol((Symbol)sexp, scope, next);
    else // sexp.type() == Type.List
      return compileList((List)sexp, scope, next);
  }
  
  public Inst compile(SourceValue sourceValue) throws CompileError
  {
    positionMap = sourceValue.positionMap;
    return compile(sourceValue.value, null, null);
  }
}
