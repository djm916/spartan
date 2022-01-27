package spartan.data;

public abstract class Datum
{
  public abstract Type type();
  
  public String repr()
  {
    return String.format("#<%s @ 0x%x>", type().getName(), System.identityHashCode(this));
  }
}
