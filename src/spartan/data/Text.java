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
  
  public boolean eq(Text that)
  {
    return this.value.equals(that.value);
  }
  
  public int compare(Text that)
  {
    return this.value.compareTo(that.value);
  }
  
  private final String value;
}
