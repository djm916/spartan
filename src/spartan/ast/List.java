package spartan.ast;

import spartan.Position;
import spartan.runtime.Inst;
import spartan.runtime.PushArg;
import spartan.runtime.MakeList;
import spartan.errors.CompileError;
import java.util.stream.Collectors;
import java.util.Iterator;

public class List extends Expr
{
  private final java.util.List<Expr> elems;
  
  public List(java.util.List<Expr> elems, Position position)
  {
    super(position);
    this.elems = elems;
  }
  
  public String sexp()
  {
    return String.format("(List %s)",
      elems.stream().map(e -> e.sexp())
                    .collect(Collectors.joining(" ")));
  }
  
  public void analyze(Scope locals) throws CompileError
  {
    for (Expr e : elems)
      e.analyze(locals);
  }
  
  public Inst compile(boolean tailContext, Inst next)
  {
    return compile(elems.iterator(), new MakeList(elems.size(), next));
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
