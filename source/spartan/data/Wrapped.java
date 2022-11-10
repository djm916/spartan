package spartan.data;

public final class Wrapped implements Datum
{
  public Wrapped(Symbol tag, Datum value)
  {
    this.tag = tag;
    this.value = value;
  }
  
  @Override
  public Type type()
  {
    return Type.WRAPPED;
  }
  
  @Override
  public String repr()
  {
    return String.format("#<%s @ 0x%x>", tag.repr(), System.identityHashCode(this));
  }
  
  public Symbol tag()
  {
    return tag;
  }
  
  public Datum value()
  {
    return value;
  }
  
  private final Symbol tag;
  private final Datum value;
}
