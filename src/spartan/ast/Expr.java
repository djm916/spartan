package spartan.ast;

import spartan.Position;
import spartan.runtime.Inst;
import spartan.errors.CompileError;

public abstract class Expr
{
  public final Position position;
  
  public abstract String sexp();
  
  public abstract void analyze(GlobalEnv globals, LocalEnv locals, boolean inLambda) throws CompileError;
  
  public abstract Inst compile(boolean tailContext, Inst next);
  
  protected Expr(Position position)
  {
    this.position = position;
  }
}
