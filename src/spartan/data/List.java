package spartan.data;

import spartan.builtins.Builtins;
import spartan.errors.TypeMismatch;

public class List extends Value
{
  /*
  public static class Builder
  {
    private List head;
    private List tail;
    
    public void append(Value v)
    {
      if (head == null) {
        head = tail = new List(v, List.Empty);
      }
      else {
        tail.rest = new List(v, List.Empty);
        tail = tail.rest;
      }
    }
    
    public List build()
    {
      return head;
    }
  }
  */
  
  public static final List Empty = new List(null, null);
  
  public Type type()
  {
    return Type.List;
  }
  
  public String repr()
  {
    return String.format("(%s)", repr(this));
  }
  
  public final Value first;
  public final List rest;
  
  public List(Value first, List rest)
  {
    this.first = first;
    this.rest = rest;
  }
  
  public Value first()
  {
    return first;
  }
  
  public Value rest()
  {
    return rest;
  }
  
  public boolean eq(List that) throws TypeMismatch
  {
    return eq(this, that);
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
