package spartan.ast;

import spartan.Position;
import spartan.runtime.Inst;
import spartan.errors.CompileError;
import java.util.List;
import java.util.Iterator;
import java.util.stream.Collectors;

public class Seq extends Expr
{
  private final List<Expr> elems;
  
  public Seq(List<Expr> elems, Position position)
  {
    super(position);
    this.elems = elems;
  }
  
  public String sexp()
  {
    return String.format("(Seq %s)",
      elems.stream().map(e -> e.sexp())
                    .collect(Collectors.joining(" ")));
  }
  
  public void analyze(GlobalEnv globals, LocalEnv locals, boolean inLambda) throws CompileError
  {
    for (Expr e : elems)
      e.analyze(globals, locals, inLambda);
  }
  
  public Inst compile(boolean tailContext, Inst next)
  {
    return compile(tailContext, elems.iterator(), next);
  }
  
  private Inst compile(boolean tailContext, Iterator<Expr> elems, Inst next)
  {
    if (!elems.hasNext())
      return next;
    else
      return elems.next().compile(tailContext && !elems.hasNext(),
             compile(tailContext, elems, next));
  }
}
