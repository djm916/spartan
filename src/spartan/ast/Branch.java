package spartan.ast;

import spartan.Position;

public class Branch
{
  public final Expr test;
  public final Expr body;
  public final Position position;
  
  public Branch(Expr test, Expr body, Position position)
  {
    this.test = test;
    this.body = body;
    this.position = position;
  }
  
  public String sexp()
  {
    return String.format("(%s %s)", test.sexp(), body.sexp());
  }
}
