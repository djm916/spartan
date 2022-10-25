package spartan.data;

import java.nio.charset.Charset;
import java.nio.CharBuffer;
import spartan.errors.TypeMismatch;
import spartan.errors.IndexOutOfBounds;

public final class Text implements Datum
{
  public Text(String value)
  {
    this.value = value;
  }
    
  @Override
  public Type type()
  {
    return Type.TEXT;
  }
  
  @Override
  public String repr()
  {
    return "\"" + value + "\"";
  }
  
  @Override
  public String str()
  {
    return value;
  }
 
  public int length()
  {
    return value.length();
  }
  
  @Override
  public boolean equals(Object other)
  {
    return (other instanceof Text that) && this.value.equals(that.value);
  }
  
  @Override
  public int hashCode()
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
    //return new Bytes(encoding.encode(CharBuffer.wrap(value)));
    return new Bytes(encoding.encode(value));
  }
  
  public Text join(List args)
  {
    var result = new StringBuilder();
    for (; !args.empty(); args = args.cdr()) {
      if (args.car().type() != Type.TEXT)
        throw new TypeMismatch();
      result.append(((Text)args.car()).value);
      if (args.cdr() != List.EMPTY)
        result.append(this.value);
    }
    return new Text(result.toString());
  }
    
  public Text concat(List args)
  {
    var result = new StringBuilder();
    result.append(this.value);
    for (; !args.empty(); args = args.cdr()) {
      if (args.car().type() != Type.TEXT)
        throw new TypeMismatch();
      result.append(((Text)args.car()).value);
    }
    return new Text(result.toString());
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
  
  public Text reverse()
  {
    return new Text(new StringBuilder(value).reverse().toString());
  }
  
  private final String value;
}
  