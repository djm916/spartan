package spartan.ast;

import spartan.Position;
import spartan.runtime.Inst;
import spartan.runtime.PopFrame;
import spartan.runtime.PopArg;
import spartan.runtime.PushLocal;
import spartan.runtime.StoreLocal;
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
  
  public void analyze(GlobalEnv globals, LocalEnv locals, boolean inLambda) throws CompileError
  {
    locals.bind(param, true);
    body.analyze(globals, locals, true);
    locals.remove(1);
  }
  
  public Inst compile(boolean tailContext, Inst next)
  {
/*      return new MakeClosure(
             new PopArg(
             new PushLocal(1,
             new StoreLocal(0,
             body.compile(true,
             new PopFrame())))),
           next);
     
 */  
    return new MakeClosure(body.compile(true, new PopFrame()), next);
  }
  
  private final String param;
  private final Expr body;
}
