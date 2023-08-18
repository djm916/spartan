package spartan.data;

public final class Unspecified implements Datum
{
  public static final Unspecified VALUE = new Unspecified();
  
  @Override
  public String type()
  {
    return "unspecified";
  }
  
  private Unspecified() {}
}
