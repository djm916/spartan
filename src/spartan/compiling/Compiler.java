package spartan.compiling;

import spartan.runtime.*;
import spartan.data.*;
import spartan.parsing.SourceValue;
import spartan.parsing.PositionMap;
import spartan.errors.CompileError;

public class Compiler
{
  private PositionMap positionMap;
  
  private static Symbol Define = Symbol.get("define");
  
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
  
  // (define symb sexp)
  
  private void checkDefine(List list) throws CompileError
  {
    if (list.length() != 3)
      throw new CompileError("define form requires 2 arguments", positionMap.get(list));
    if (list.rest.first.type() != Type.Symbol)
      throw new CompileError("symbol expected", positionMap.get(list.rest.first));
  }
  
  private Inst compileDefine(List list, Scope scope, Inst next) throws CompileError
  {
    checkDefine(list);
    Symbol symb = (Symbol)list.rest.first;
    Value init = list.rest.rest.first;
    return compile(init, scope,
                   new StoreGlobal(symb, next));
  }
  
  private Inst compileSymbol(Symbol symb, Scope scope, Inst next) throws CompileError
  {
    return null;
  }
  
  private Inst compileIf(List list, Scope scope, Inst next) throws CompileError
  {
    return null;
  }
  
  private Inst compileLet(List list, Scope scope, Inst next) throws CompileError
  {
    return null;
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
