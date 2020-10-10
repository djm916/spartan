package spartan.ast;

import spartan.Position;
import spartan.runtime.Inst;
import spartan.errors.CompileError;

public abstract class Expr
{
  public final Position position;
  
  public abstract String sexp();
  
  public abstract void analyze(GlobalEnv globals, LocalEnv locals) throws CompileError;
  
  public abstract Inst compile(Inst next, boolean tailContext);
  
  protected Expr(Position position)
  {
    this.position = position;
  }
}
