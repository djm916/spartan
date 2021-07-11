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
  
  private static boolean isAtom(Value sexp)
  {
    return sexp.type() == Type.Bool
        || sexp.type() == Type.Int
        || sexp.type() == Type.Real
        || sexp.type() == Type.Vector
        || sexp.type() == Type.Record
        || sexp.type() == Type.Text
        || sexp == List.Empty;
  }

  private Inst compileAtom(Value atom, Inst next)
  {
    return new LoadConst(atom, next);
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
    
    return compile(init, scope,
                   new StoreGlobal(symb, next));
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
  
  // (let ((symb init) ...) body)
  
  private Inst compileLet(List list, Scope scope, Inst next) throws CompileError
  {
    if (list.length() < 3 || list.rest.first.type() != Type.List)
      throw new CompileError("malformed expression", positionMap.get(list));
    
    List bindings = (List)list.rest.first;
    List body = list.rest.rest;

    checkBindings(bindings, positionMap.get(list));
    
    return new PushLocal(bindings.length(),
                         compileLetBindings(bindings, 0, scope,
                         compileSequence(body, extendScope(bindings, scope),
                         new PopLocal(next))));
  }
  
  private void checkBindings(List bindings, Position position) throws CompileError
  {
    for (; bindings != List.Empty; bindings = bindings.rest) {
      if (bindings.first.type() != Type.List)
        throw new CompileError("malformed expression", position);
      List binding = (List)bindings.first;
      if (binding.length() != 2 || binding.first.type() != Type.Symbol)
        throw new CompileError("malformed expression", position);
    }
  }
  
  private Scope extendScope(List bindings, Scope parent) throws CompileError
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
  
  private Inst compileLetBindings(List bindings, int offset, Scope scope, Inst next) throws CompileError
  {
    if (bindings == List.Empty)
      return next;
    else
      return compile(((List)bindings.first).rest.first, scope,
             new StoreLocal(0, offset,
             compileLetBindings(bindings.rest, offset + 1, scope, next)));
  }
  
  private Inst compileSequence(List list, Scope scope, Inst next) throws CompileError
  {
    if (list == List.Empty)
      return next;
    else
      return compile(list.first, scope,
             compileSequence(list.rest, scope, next));
  }
  
  private Inst compileLetStar(List list, Scope scope, Inst next) throws CompileError
  {
    return null;
  }

  private Inst compileLetRec(List list, Scope scope, Inst next) throws CompileError
  {
    return null;
  }
  
  private Inst compileApply(List list, Scope scope, Inst next) throws CompileError
  {
    return null;
  }
  
  private Inst compileList(List list, Scope scope, Inst next)
  throws CompileError
  {
    if (list.first.type() == Type.Symbol) {
      if (list.first == Symbol.get("if")) {
        return compileIf(list, scope, next);
      }
      else if (list.first == Symbol.get("let")) {
        return compileLet(list, scope, next);
      }
      else if (list.first == Symbol.get("let*")) {
        return compileLetStar(list, scope, next);
      }
      else if (list.first == Symbol.get("letrec")) {
        return compileLetRec(list, scope, next);
      }
      else if (list.first == Symbol.get("define")) {
        return compileDefine(list, scope, next);
      }
    }
    return compileApply(list, scope, next);
  }
  
  public Inst compile(Value sexp, Scope scope, Inst next) throws CompileError
  {
    if (isAtom(sexp)) {
      return compileAtom(sexp, next);
    }
    else if (sexp.type() == Type.Symbol) {
      return compileSymbol((Symbol)sexp, scope, next);
    }
    else if (sexp.type() == Type.List) {
      return compileList((List)sexp, scope, next);
    }
    else {
      throw new CompileError("unrecognized expression " + sexp.repr(), positionMap.get(sexp));
    }
  }
  
  public Inst compile(SourceValue sourceValue) throws CompileError
  {
    this.positionMap = sourceValue.positionMap;
    return compile(sourceValue.value, null, null);
  }
}
