package spartan.data;

import java.nio.charset.Charset;
import spartan.errors.TypeMismatch;
import spartan.errors.IndexOutOfBounds;

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
  
  public Bytes encode(Charset encoding)
  {
    return new Bytes(value.getBytes(encoding));
  }
  
  public Text join(List args)
  {
    return join(this, args);
  }
  
  private static Text join(Text delimiter, List args)
  {
    var buffer = new StringBuilder();
    for (; !args.empty(); args = args.cdr()) {
      if (args.car().type() != Type.TEXT)
        throw new TypeMismatch();
      buffer.append(((Text)args.car()).str());
      if (args.cdr() != List.EMPTY)
        buffer.append(delimiter.str());
    }
    return new Text(buffer.toString());
  }
  
  public Text concat(List args)
  {
    return concat(this, args);
  }
  
  private static Text concat(Text first, List args)
  {
    var buffer = new StringBuilder(first.value);
    for (; !args.empty(); args = args.cdr()) {
      if (args.car().type() != Type.TEXT)
        throw new TypeMismatch();
      buffer.append(((Text)args.car()).str());
    }
    return new Text(buffer.toString());
  }
  
  public Text substring(int start, int end)
  {
    try {
      return new Text(value.substring(start, end));
    }
    catch (StringIndexOutOfBoundsException ex) {
      throw new IndexOutOfBounds();
    }
  }
  
  private final String value;
}
