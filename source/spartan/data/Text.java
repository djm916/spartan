package spartan.data;

import java.nio.charset.Charset;
import java.nio.CharBuffer;
import spartan.errors.TypeMismatch;
import spartan.errors.IndexOutOfBounds;

public final class Text implements Datum, IEq<Text>, IOrd<Text>, ISize
{
  public Text(String value)
  {
    this.value = value;
  }
    
  @Override // Datum
  public Type type()
  {
    return Type.TEXT;
  }
  
  @Override // Datum
  public String repr()
  {
    return "\"" + value + "\"";
  }
  
  @Override // Datum
  public String str()
  {
    return value;
  }
  
  @Override // ISize
  public int length()
  {
    return value.length();
  }
  
  @Override // Object
  public boolean equals(Object other)
  {
    return (other instanceof Text that) && this.value.equals(that.value);
  }
  
  @Override // Object
  public int hashCode()
  {
    return value.hashCode();
  }
  
  @Override // IEq
  public boolean isEqual(Text other)
  {
    return this.value.equals(other.value);
  }
  
  @Override // IEq
  public int compareTo(Text other)
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
  