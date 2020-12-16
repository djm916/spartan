package spartan.ast;

import spartan.Position;

public class Binding
{
  public final String id;
  public final Expr init;
  public final Position pos;
  
  public Binding(String id, Expr init, Position pos)
  {
    this.id = id;
    this.init = init;
    this.pos = pos;
  }
  
  public String sexp()
  {
    return String.format("(%s %s)", id, init.sexp());
  }
}
