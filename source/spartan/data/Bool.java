package spartan.data;

public final class Bool extends Datum
{
  public static final Bool True = new Bool(true);  
  public static final Bool False = new Bool(false);
  
  public Type type()
  {
    return Type.Bool;
  }
  
  public String repr()
  {
    return value ? "true" : "false";
  }
  
  public Bool not()
  {
    return value ? False : True;
  }
  
  public Bool or(Bool that)
  {
    return this.value || that.value ? True : False;
  }
  
  public Bool and(Bool that)
  {
    return this.value && that.value ? True : False;
  }
    
  private Bool(boolean value)
  {
    this.value = value;
  }
  
  private final boolean value;
}
