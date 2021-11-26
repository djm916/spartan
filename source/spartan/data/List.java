package spartan.data;

import spartan.builtins.CoreLib;
import spartan.errors.TypeMismatch;

public final class List extends Datum
{
  public static final List Empty = new List(null, null);
  
  public static class Builder
  {
    private List head = Empty, tail = Empty;
    
    public Builder add(Datum x)
    {
      if (head == Empty)
        head = tail = new List(x, Empty);
      else
        tail = tail.rest = new List(x, Empty);
      
      return this;
    }
    
    public List build()
    {
      return head;
    }
  }
  
  public static List cons(Datum first, List rest)
  {
    return new List(first, rest);
  }
  
  public static List of(Datum... elems)
  {
    var result = Empty;
    for (int i = elems.length - 1; i >= 0; --i)
      result = cons(elems[i], result);
    return result;
  }

  public Type type()
  {
    return Type.List;
  }
  
  public String repr()
  {
    return String.format("(%s)", repr(this));
  }
    
  public boolean empty()
  {
    return this == Empty;
  }
  
  public Datum car()
  {
    return first;
  }
  
  public List cdr()
  {
    return rest;
  }
  
  public Datum cadr()
  {
    return rest.first;
  }
  
  public Datum caddr()
  {
    return rest.rest.first;
  }
  
  public Datum cadddr()
  {
    return rest.rest.rest.first;
  }
  
  public List cddr()
  {
    return rest.rest;
  }
  
  public List cdddr()
  {
    return rest.rest.rest;
  }
  
  public int length()
  {
    return length(this);
  }
  
  public Datum at(int index)
  {
    return at(this, index);
  }
  
  public boolean eq(List that) throws TypeMismatch
  {
    return eq(this, that);
  }
  
  public List concat(List that)
  {
    return concat(this, that);
  }
  
  public List append(Datum x)
  {
    return append(this, x);
  }
  
  public List reverse()
  {
    return reverse(this);
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
      if (!CoreLib.eq(x.first, y.first))
        return false;
    
    return x == Empty && y == Empty;
  }
  
  private static List concat(List x, List y)
  {
    var result = new Builder();
    for (; x != Empty; x = x.rest)
      result.add(x.first);
    for (; y != Empty; y = y.rest)
      result.add(y.first);
    return result.build();
  }
  
  private static List append(List x, Datum y)
  {
    var result = new Builder();
    for (; x != Empty; x = x.rest)
      result.add(x.first);
    result.add(y);
    return result.build();
  }
  
  private static List reverse(List x)
  {
    var result = List.Empty;
    for (; x != Empty; x = x.rest)
      result = cons(x.car(), result);
    return result;
  }
  
  private static String repr(List self)
  {
    if (self == Empty)
      return "";
    
    if (self.rest == Empty)
      return self.first.repr();
    
    return String.format("%s %s", self.first.repr(), repr(self.rest));
  }

  private List(Datum first, List rest)
  {
    this.first = first;
    this.rest = rest;
  }
    
  private Datum first;
  private List rest;  
}
