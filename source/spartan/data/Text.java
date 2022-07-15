package spartan.data;

import spartan.errors.Error;

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
  
  public String str()
  {
    return value;
  }
 
  public int length()
  {
    return value.length();
  }
  
  public int hash()
  {
    return value.hashCode();
  }
  
  public static boolean eq(Text x, Text y)
  {
    return x.value.equals(y.value);
  }
  
  public static int compare(Text x, Text y)
  {
    return x.value.compareTo(y.value);
  }
  
  public Bytes encode(String encoding)
  {
    try {
      return new Bytes(value.getBytes(encoding));
    }
    catch (java.io.UnsupportedEncodingException ex) {
      throw new Error("unsupported encoding " + encoding);
    }
  }
    
  private final String value;
}
