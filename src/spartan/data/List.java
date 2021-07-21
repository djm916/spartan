package spartan.data;

import spartan.builtins.Builtins;
import spartan.errors.TypeMismatch;

public class List extends Datum
{
  public static final List Empty = new List(null, null);
  
  public final Type type()
  {
    return Type.List;
  }
  
  public final String repr()
  {
    return String.format("(%s)", repr(this));
  }
  
  public final Datum first;
  public final List rest;
  
  public List(Datum first, List rest)
  {
    this.first = first;
    this.rest = rest;
  }
  
  public final int length()
  {
    return length(this);
  }
  
  public final boolean eq(List that) throws TypeMismatch
  {
    return eq(this, that);
  }
  
  private static int length(List list)
  {
    int length = 0;
    for (; list != Empty; list = list.rest)
      length += 1;
    return length;
  }
  
  private static boolean eq(List x, List y) throws TypeMismatch
  {
    for (; x != Empty && y != Empty; x = x.rest, y = y.rest)
      if (!Builtins.eq(x.first, y.first))
        return false;
    
    return x == Empty && y == Empty;
  }
  
  private static String repr(List self)
  {
    if (self == Empty)
      return "";
    
    if (self.rest == Empty)
      return self.first.repr();
    
    return String.format("%s %s", self.first.repr(), repr(self.rest));
  }
}
