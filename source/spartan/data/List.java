package spartan.data;

import spartan.builtins.Core;
import spartan.errors.TypeMismatch;

public class List extends Datum
{
  public static final List Empty = new List(null, null);
  
  public static class Builder
  {
    private List head = Empty, tail = Empty;
    
    public Builder add(Datum x)
    {
      if (head == Empty)
        head = tail = new List(x, Empty);
      else {
        tail.rest = new List(x, Empty);
        tail = tail.rest;
      }
      return this;
    }
    
    public List build()
    {
      return head;
    }
  }
  
  public final Type type()
  {
    return Type.List;
  }
  
  public final String repr()
  {
    return String.format("(%s)", repr(this));
  }
  
  public List(Datum first, List rest)
  {
    this.first = first;
    this.rest = rest;
  }
  
  public final Datum car()
  {
    return first;
  }
  
  public final List cdr()
  {
    return rest;
  }
    
  public final int length()
  {
    return length(this);
  }
  
  public final Datum at(int index)
  {
    return at(this, index);
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
  
  private static Datum at(List list, int index)
  {
    while (index-- > 0)
      list = list.rest;
    return list.first;
  }
  
  private static boolean eq(List x, List y) throws TypeMismatch
  {
    for (; x != Empty && y != Empty; x = x.rest, y = y.rest)
      if (!Core.eq(x.first, y.first))
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
    
  private Datum first;
  private List rest;  
}
