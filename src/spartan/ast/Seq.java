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
  
  public void analyze(GlobalEnv globals, LocalEnv locals) throws CompileError
  {
    for (Expr e : elems)
      e.analyze(globals, locals);
  }
  
  public Inst compile(Inst next)
  {
    return compile(elems.iterator(), next);
  }
  
  private Inst compile(Iterator<Expr> elems, Inst next)
  {
    if (!elems.hasNext())
      return next;
    else
      return elems.next().compile(compile(elems, next));
  }
}
