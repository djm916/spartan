package spartan.ast;

import spartan.Position;
import spartan.data.Value;
import spartan.runtime.Inst;
import spartan.runtime.LoadConst;
import spartan.errors.CompileError;

public class Op extends Expr
{
  public final String symbol;
  public final Value value;
  
  public Op(String symbol, Value value, Position position)
  {
    super(position);
    this.symbol = symbol;
    this.value = value;
  }
  
  public String sexp()
  {
    return String.format("%s", symbol);
  }
  
  public void analyze(GlobalEnv globals, LocalEnv locals, boolean inLambda) throws CompileError
  {
    
  }
  
  public Inst compile(boolean tailContext, Inst next)
  {
    return new LoadConst(value, next);
  }
}
