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
  
  public void analyze(GlobalEnv globals, LocalEnv locals, boolean inLambda) throws CompileError
  {
    for (Branch b : thenExprs) {
      b.test.analyze(globals, locals, inLambda);
      b.body.analyze(globals, locals, inLambda);
    }
    
    elseExpr.analyze(globals, locals, inLambda);
  }
  
  public Inst compile(boolean tailContext, Inst next)
  {
    return compile(tailContext, thenExprs.iterator(), next);
  }
  
  private Inst compile(boolean tailContext, Iterator<Branch> thenExprs, Inst next)
  {
    if (!thenExprs.hasNext()) {
      return elseExpr.compile(tailContext, next);
    }
    else {
      Branch b = thenExprs.next();
      return b.test.compile(false,
             new spartan.runtime.Branch(b.position,
               b.body.compile(tailContext, next),
               compile(tailContext, thenExprs, next)));
    }
  }
  
  private final List<Branch> thenExprs;
  private final Expr elseExpr;
}
