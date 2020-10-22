package spartan.ast;

import spartan.Position;
import spartan.runtime.Inst;
import spartan.runtime.LoadLocal;
import spartan.runtime.LoadGlobal;
import spartan.errors.CompileError;

public class VarRef extends Expr
{
  private final String id;
  private Variable var;
  
  public VarRef(String id, Position position)
  {
    super(position);
    this.id = id;
  }
  
  public String sexp()
  {
    return String.format("(Ref %s)", id);
  }
  
  public void analyze(GlobalEnv globals, LocalEnv locals, boolean inLambda) throws CompileError
  {
    var = locals.lookup(id).orElseGet(() -> globals.lookup(id).orElse(null));
    
    if (var == null)
      throw new CompileError("variable \"" + id + "\" not bound", position);
      
    if (!inLambda && !var.hasValue())
      throw new CompileError("forward reference to variable \"" + id + "\"", position);
  }
  
  public Inst compile(boolean tailContext, Inst next)
  {
    return var.global() ? new LoadGlobal(var.depth(), next)
                        : new LoadLocal(var.depth(), next);
  }
}
