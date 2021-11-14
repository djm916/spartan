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
  
  public int length()
  {
    return value.length();
  }
  
  public boolean eq(Text that)
  {
    return this.value.equals(that.value);
  }
  
  public int compare(Text that)
  {
    return this.value.compareTo(that.value);
  }
    
  public String toString()
  {
    return value;
  }
  
  private final String value;
}
