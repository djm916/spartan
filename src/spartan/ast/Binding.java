package spartan.ast;

import spartan.Position;

public class Binding
{
  public final String id;
  public final Expr init;
  public final Position position;
  
  public Binding(String id, Expr init, Position position)
  {
    this.id = id;
    this.init = init;
    this.position = position;
  }
  
  public String sexp()
  {
    return String.format("(%s %s)", id, init.sexp());
  }
}
