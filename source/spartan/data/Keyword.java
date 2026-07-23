package spartan.data;

import spartan.util.WeakCache;

public final class Keyword implements Datum, IEq
{
  /**
   * Return an interned symbol. May return a new symbol or this (if previously interned)
   */
  public static Keyword of(String name)
  {
    return cache.get(name, () -> new Keyword(name));
  }
  
  @Override // Datum
  public Type type()
  {
    return Type.KEYWORD;
  }
  
  @Override // Datum
  public String repr()
  {
    return name;
  }
  
  public String toString()
  {
    return name;
  }
  
  @Override // Object
  public boolean equals(Object rhs)
  {
    return this == rhs;
  }
  
  @Override // Object
  public int hashCode()
  {
    return name.hashCode();
  }
  
  public boolean equals(String rhs)
  {
    return name.equals(rhs);
  }
  
  @Override // IEq
  public boolean isEqual(Keyword rhs)
  {
    return this == rhs;
  }
  
  private Keyword(String name)
  {
    this.name = name;
  }
  
  private static WeakCache<String, Keyword> cache = new WeakCache<>();
  private final String name;
}
