package spartan.data;

public class Text extends Datum
{
  public Text(String value)
  {
    this.value = value;
  }
  
  public final Type type()
  {
    return Type.Text;
  }
  
  public final String repr()
  {
    return "\"" + value + "\"";
  }
  
  public final boolean eq(Text that)
  {
    return this.value.equals(that.value);
  }
  
  public final int compare(Text that)
  {
    return this.value.compareTo(that.value);
  }
  
  private final String value;
}
