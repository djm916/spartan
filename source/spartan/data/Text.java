package spartan.data;

public final class Text extends Datum
{
  public Text(String value)
  {
    this.value = value;
  }
  
  public Type type()
  {
    return Type.Text;
  }
  
  public String repr()
  {
    return "\"" + value + "\"";
  }
   
  public String value()
  {
    return value;
  }
 
  public int length()
  {
    return value.length();
  }
  
  public static boolean eq(Text x, Text y)
  {
    return x.value.equals(y.value);
  }
  
  public static int compare(Text x, Text y)
  {
    return x.value.compareTo(y.value);
  }
  
  private final String value;
}
