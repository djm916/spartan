package spartan.data;

public class Text extends Value
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
    return value;
  }
  
  public static boolean eq(Text x, Text y)
  {
    return x.value.equals(y.value);
  }
  
  private final String value;
}
