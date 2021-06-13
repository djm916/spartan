package spartan.ast;

import spartan.Position;
import spartan.runtime.Inst;
import spartan.runtime.LoadLocal;
import spartan.runtime.LoadGlobal;
import spartan.errors.CompileError;
import java.util.Optional;

public class VarRef extends Expr
{
  private final String id;
  private Integer depth;
  
  public VarRef(String id, Position position)
  {
    super(position);
    this.id = id;
  }
  
  public String sexp()
  {
    return String.format("(Ref %s)", id);
  }
  
  public void analyze(Scope locals) throws CompileError
  {
    if (locals != null)
      depth = locals.lookup(id);
  }
  
  public Inst compile(boolean tailContext, Inst next)
  {
    return depth != null ? new LoadLocal(depth, next)
                         : new LoadGlobal(position, id.intern(), next);
  }
}
