package spartan.data;

public class Bool extends Datum
{
  public static final Bool TRUE = new Bool() {
    public String repr() { return "true"; }
  };
  
  public static final Bool FALSE = new Bool() {
    public String repr() { return "false"; }
  };
  
  public Type type()
  {
    return Type.BOOL;
  }

  private Bool() { }
}
