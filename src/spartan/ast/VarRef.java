package spartan.ast;

import spartan.Position;
import spartan.runtime.Inst;
import spartan.runtime.LoadLocal;
import spartan.runtime.LoadGlobal;
import spartan.errors.CompileError;

public class VarRef extends Expr
{
  private final String id;
  private Index index;
  
  public VarRef(String id, Position position)
  {
    super(position);
    this.id = id;
  }
  
  public String sexp()
  {
    return String.format("(Ref %s)", id);
  }
  
  public void analyze(GlobalEnv globals, LocalEnv locals) throws CompileError
  {
    index = locals.lookup(id);
    if (index == null)
      index = globals.lookup(id);
    if (index == null)
      throw new CompileError("variable \"" + id + "\" not bound", position);
  }
  
  public Inst compile(Inst next)
  {
    return index.global ? new LoadGlobal(index.depth, next)
                        : new LoadLocal(index.depth, next);
  }
}
