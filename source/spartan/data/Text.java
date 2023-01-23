package spartan.data;

import java.nio.charset.Charset;
import java.nio.CharBuffer;
import spartan.errors.TypeMismatch;
import spartan.errors.IndexOutOfBounds;

public final class Text implements Datum, IEq, IOrd, ISize
{
  public Text(String value)
  {
    this.value = value;
  }
    
  @Override // Datum
  public String type()
  {
    return "string";
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
  public boolean equals(Object rhs)
  {
    return (rhs instanceof Text s) && value.equals(s.value);
  }
  
  @Override // Object
  public int hashCode()
  {
    return value.hashCode();
  }
  
  @Override // IEq
  public boolean isEqual(Text rhs)
  {
    return value.equals(rhs.value);
  }
  
  @Override
  public int compareTo(Text rhs)
  {
    return value.compareTo(rhs.value);
  }
  
  public Bytes encode(Charset encoding)
  {
    //return new Bytes(encoding.encode(CharBuffer.wrap(value)));
    return new Bytes(encoding.encode(value));
  }
  
  public static Text join(Text delim, List args)
  {
    var result = new StringBuilder();
    for (; !args.empty(); args = args.cdr()) {
      if (args.car() instanceof Text text) {
        result.append(text.value);
        if (! args.cdr().empty())
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
  
  //@Override
  public Text get(int index)
  {
    return substring(index, index + 1);
  }
  
  private final String value;
}
  