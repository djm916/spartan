package spartan.ast;

import spartan.Position;
import spartan.runtime.Inst;
import spartan.runtime.PushFrame;
import spartan.runtime.PushArg;
import spartan.errors.CompileError;

public class Apply extends Expr
{
  private final Expr fun;
  private final Expr arg;
  
  public Apply(Expr fun, Expr arg, Position position)
  {
    super(position);
    this.fun = fun;
    this.arg = arg;
  }
  
  public String sexp()
  {
    return String.format("(Apply %s %s)", fun.sexp(), arg.sexp());
  }
  
  public void analyze(GlobalEnv globals, LocalEnv locals) throws CompileError
  {
    fun.analyze(globals, locals);
    arg.analyze(globals, locals);
  }
  
  public Inst compile(boolean tailContext, Inst next)
  {
    if (tailContext)
      return fun.compile(false,
             new PushArg(
             arg.compile(false,
             new PushArg(
             new spartan.runtime.Apply(position)))));
    else
      return new PushFrame(next,
             fun.compile(false,
             new PushArg(
             arg.compile(false,
             new PushArg(
             new spartan.runtime.Apply(position))))));
  }
}
