package spartan.ast;

import spartan.Position;
import spartan.runtime.Inst;
import spartan.runtime.LoadConst;
import spartan.errors.CompileError;

public class RecordSelector extends Expr
{
  private final String label;
  
  public RecordSelector(String label, Position position)
  {
    super(position);
    this.label = label;
  }
  
  public String sexp()
  {
    return String.format("(Select %s)", label);
  }
  
  public void analyze(Scope locals) throws CompileError
  {
    
  }
  
  public Inst compile(boolean tailContext, Inst next)
  {
    return new LoadConst(new spartan.data.RecordSelector(label.intern()), next);
  }
}
