package spartan.ast;

import spartan.Position;
import spartan.runtime.Inst;
import spartan.runtime.PopFrame;
import spartan.runtime.MakeClosure;
import spartan.errors.CompileError;

public class Fun extends Expr
{
  public Fun(String param, Expr body, Position position)
  {
    super(position);
    this.param = param;
    this.body = body;
  }
  
  public String sexp()
  {
    return String.format("(Fun %s %s)", param, body.sexp());
  }
  
  public void analyze(GlobalEnv globals, LocalEnv locals) throws CompileError
  {
    body.analyze(globals, locals.bind(param));
  }
  
  public Inst compile(Inst next)
  {
    return new MakeClosure(body.compile(new PopFrame()), next);
  }
  
  private final String param;
  private final Expr body;
}
