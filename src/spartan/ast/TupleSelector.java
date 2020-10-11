package spartan.ast;

import spartan.Position;
import spartan.runtime.Inst;
import spartan.runtime.LoadConst;
import spartan.errors.CompileError;

public class TupleSelector extends Expr
{
  private final int index;
  
  public TupleSelector(int index, Position position)
  {
    super(position);
    this.index = index;
  }
  
  public String sexp()
  {
    return String.format("(Select %d)", index);
  }
  
  public void analyze(GlobalEnv globals, LocalEnv locals) throws CompileError
  {
    
  }
  
  public Inst compile(boolean tailContext, Inst next)
  {
    return new LoadConst(new spartan.data.TupleSelector(index), next);
  }
}

