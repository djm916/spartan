package spartan.data;

public class Port extends Datum
{
  private Port()
  {
  }
  
  public final Type type()
  {
    return Type.Port;
  }
  
  public final String repr()
  {
    return String.format("%s @ 0x%x", Type.Port.name, System.identityHashCode(this));
  }
}
