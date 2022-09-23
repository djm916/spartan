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
    return Type.TEXT;
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
  
  public boolean eq(Text other)
  {
    return this.value.equals(other.value);
  }
  
  public int compare(Text other)
  {
    return this.value.compareTo(other.value);
  }
  
  public Bytes encode(String encoding)
  {
    try {
      var bytes = value.getBytes(encoding);
      return new Bytes(bytes);
    }
    catch (java.io.UnsupportedEncodingException ex) {
      throw new Error("unsupported encoding " + encoding);
    }
  }
    
  private final String value;
}
