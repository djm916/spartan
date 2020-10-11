package spartan.ast;

import spartan.Position;
import spartan.runtime.Inst;
import spartan.runtime.PushArg;
import spartan.runtime.MakeTuple;
import spartan.errors.CompileError;
import java.util.List;
import java.util.Iterator;
import java.util.stream.Collectors;

public class Tuple extends Expr
{
  private final List<Expr> elems;
  
  public Tuple(List<Expr> elems, Position position)
  {
    super(position);
    this.elems = elems;
  }
  
  public String sexp()
  {
    return String.format("(Tuple %s)",
      elems.stream().map(e -> e.sexp())
                    .collect(Collectors.joining(" ")));
  }
  
  public void analyze(GlobalEnv globals, LocalEnv locals) throws CompileError
  {
    for (Expr e : elems)
      e.analyze(globals, locals);
  }
  
  public Inst compile(boolean tailContext, Inst next)
  {
    return compile(elems.iterator(), new MakeTuple(elems.size(), next));
  }
  
  private Inst compile(Iterator<Expr> elems, Inst next)
  {
    if (!elems.hasNext())
      return next;
    else
      return elems.next().compile(false,
             new PushArg(compile(elems, next)));
  }
}
