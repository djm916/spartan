package spartan.ast;

import spartan.Position;
import spartan.data.Value;
import spartan.runtime.Inst;
import spartan.runtime.LoadConst;
import spartan.errors.CompileError;

public class Const extends Expr
{
  public final Value value;
  
  public Const(Value value, Position position)
  {
    super(position);
    this.value = value;
  }
  
  public String sexp()
  {
    return String.format("(Const %s)", value.repr());
  }
  
  public void analyze(GlobalEnv globals, LocalEnv locals) throws CompileError
  {
    
  }
  
  public Inst compile(Inst next)
  {
    return new LoadConst(value, next);
  }
}
