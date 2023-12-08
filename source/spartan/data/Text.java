package spartan.data;

import java.nio.charset.Charset;
import java.nio.CharBuffer;
import spartan.errors.TypeMismatch;
import spartan.errors.IndexOutOfBounds;

public final class Text implements Datum, ILen, IEq, IOrd
{
  public Text(String value)
  {
    this.value = value;
  }
    
  @Override // Datum
  public Type type()
  {
    return TypeRegistry.STRING_TYPE;
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
  
  @Override // ILen
  public int length()
  {
    return value.length();
  }
  
  @Override // ILen
  public boolean isEmpty()
  {
    return value.isEmpty();
  }
  
  @Override // Object
  public int hashCode()
  {
    return value.hashCode();
  }
  
  @Override // IOrd
  public int compareTo(Text rhs)
  {
    return value.compareTo(rhs.value);
  }
  
  @Override // IEq
  public boolean isEqual(Text rhs)
  {
    return value.equals(rhs.value);
  }
  
  public Bytes encode(Charset encoding)
  {
    //return new Bytes(encoding.encode(CharBuffer.wrap(value)));
    //return new Bytes(encoding.encode(value));
    return new Bytes(value.getBytes(encoding));
  }
  
  public static Text join(Text delim, List args)
  {
    var result = new StringBuilder();
    for (; !args.isEmpty(); args = args.cdr()) {
      if (args.car() instanceof Text text) {
        result.append(text.value);
        if (! args.cdr().isEmpty())
          result.append(delim.value);
      }
      else throw new TypeMismatch();
    }
    return new Text(result.toString());
  }
  
  public static Text concat(List args)
  {
    var result = new StringBuilder();
    for (var arg : args) {
      if (arg instanceof Text text)
        result.append(text.value);
      else throw new TypeMismatch();
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
