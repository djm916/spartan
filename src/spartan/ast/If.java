package spartan.ast;

import spartan.Position;
import spartan.runtime.Inst;
import spartan.runtime.LoadConst;
import spartan.errors.CompileError;
import java.util.List;
import java.util.stream.Collectors;
import java.util.Iterator;

public class If extends Expr
{
  public If(List<Branch> thenExprs, Expr elseExpr, Position position)
  {
    super(position);
    this.thenExprs = thenExprs;
    this.elseExpr = elseExpr;
  }
  
  public String sexp()
  {
    return String.format("(If %s %s)",
      thenExprs.stream().map(b -> b.sexp()).collect(Collectors.joining(" ")),
      elseExpr.sexp());
  }
  
  public void analyze(GlobalEnv globals, LocalEnv locals) throws CompileError
  {
    for (Branch b : thenExprs) {
      b.test.analyze(globals, locals);
      b.body.analyze(globals, locals);
    }
    
    elseExpr.analyze(globals, locals);
  }
  
  public Inst compile(Inst next)
  {
    return compile(thenExprs.iterator(), next);
  }
  
  private Inst compile(Iterator<Branch> thenExprs, Inst next)
  {
    if (!thenExprs.hasNext()) {
      return elseExpr.compile(next);
    }
    else {
      Branch b = thenExprs.next();
      return b.test.compile(
        new spartan.runtime.Branch(b.body.compile(next),
                                   compile(thenExprs, next),
                                   b.position));
    }
  }
  
  private final List<Branch> thenExprs;
  private final Expr elseExpr;
}
