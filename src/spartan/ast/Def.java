package spartan.ast;

import spartan.runtime.Inst;
import spartan.runtime.StoreGlobal;
import spartan.errors.CompileError;

public class Def extends Expr
{
  public final Binding binding;
  
  public Def(Binding binding)
  {
    super(binding.position);
    this.binding = binding;
  }
  
  public String sexp()
  {
    return String.format("(Def %s)", binding.sexp());
  }
  
  public void analyze(Scope locals) throws CompileError
  {
    binding.init.analyze(locals);
  }
  
  public Inst compile(boolean tailContext, Inst next)
  {
    return binding.init.compile(tailContext,
           new StoreGlobal(binding.id, next));
  }
}
