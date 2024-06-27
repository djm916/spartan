package spartan.data;

import java.nio.charset.Charset;
import java.nio.CharBuffer;
import java.util.regex.Pattern;
import spartan.errors.TypeMismatch;
import spartan.errors.IndexOutOfBounds;
import spartan.errors.NoSuchElement;

public final class Text implements Datum, IEq, IOrd
{
  public static final class Cursor implements Datum, IEq, IOrd
  {
    private int next(int currentOffset)
    {
      if (currentOffset == string.length())
        throw new NoSuchElement();
      int codePoint = string.codePointAt(currentOffset);
      int nextOffset = currentOffset + (Character.isBmpCodePoint(codePoint) ? 1 : 2);
      return nextOffset;
    }
    
    private int prev(int currentOffset)
    {
      if (currentOffset == 0)
        throw new NoSuchElement();
      int codePoint = string.codePointBefore(currentOffset);
      int prevOffset = currentOffset + (Character.isBmpCodePoint(codePoint) ? -1 : -2);
      return prevOffset;
    }
    
    public Cursor next()
    {
      return new Cursor(string, next(offset));
    }
    
    public Cursor prev()
    {
      return new Cursor(string, prev(offset));
    }
    
    public Cursor forward(int n)
    {
      int nextOffset = offset;
      for (; n > 0; n--) {
        nextOffset = next(nextOffset);
      }
      return new Cursor(string, nextOffset);
    }
    
    public int offset()
    {
      return offset;
    }
    
    @Override // Datum
    public Type type()
    {
      return TypeRegistry.STRING_CURSOR_TYPE;
    }
    
    @Override // IOrd
    public int compareTo(Cursor rhs)
    {
      if (this.offset < rhs.offset)
        return -1;
      else if (this.offset > rhs.offset)
        return +1;
      else
        return 0;
    }
    
    @Override // IEq
    public boolean isEqual(Cursor rhs)
    {
      return this.offset == rhs.offset;
    }
    
    private Cursor(String string, int offset)
    {
      this.string = string;
      this.offset = offset;
    }
    
    private final String string;
    private final int offset;
  }

  public static Text fromCodePoints(List codePoints)
  {
    var intArray = codePoints.streamOf(IInt.class, TypeMismatch::new)
                   .mapToInt(c -> c.intValue())
                   .toArray();
    
    return fromCodePoints(intArray);
  }
  
  public static Text fromCodePoints(int[] codePoints)
  {
    return new Text(new String(codePoints, 0, codePoints.length));
  }
  
  public Text(String value)
  {
    this.value = value;
    this.length = value.codePointCount(0, value.length());
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
  
  public int length()
  {
    //return value.length();
    return length;
  }
  
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
  
  public Cursor cursor()
  {
    return new Cursor(value, 0);
  }
  
  public Cursor endCursor()
  {
    return new Cursor(value, value.length());
  }
  
  public IInt get(Cursor position)
  {
    int codePoint = value.codePointAt(position.offset());
    return Int.valueOf(codePoint);
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
      if (!(args.car() instanceof Text text))
        throw new TypeMismatch();
      result.append(text.value);
      if (!args.cdr().isEmpty())
        result.append(delim.value);
    }
    return new Text(result.toString());
  }
  
  public static Text concat(List args)
  {
    var result = new StringBuilder();
    for (var arg : args) {
      if (!(arg instanceof Text text))
        throw new TypeMismatch();
      result.append(text.value);
    }
    return new Text(result.toString());
  }
  
  public Text substring(Cursor start, Cursor end)
  {
    try {
      return new Text(value.substring(start.offset(), end.offset()));
    }
    catch (StringIndexOutOfBoundsException ex) {
      throw new IndexOutOfBounds();
    }
  }
  
  public Text reverse()
  {
    return new Text(new StringBuilder(value).reverse().toString());
  }
  
  public List split(Text separator, int limit)
  {
    var partArray = this.value.split(Pattern.quote(separator.value), limit);
    var result = new List.Builder();
    for (var part : partArray)
      result.add(new Text(part));
    return result.build();
  }
  
  public Cursor find(Text string, Cursor start)
  {
    int i = this.value.indexOf(string.value, start.offset());
    if (i < 0)
      return endCursor();
    else
      return new Cursor(value, i);
  }
  
  public Text replace(Text substring, Text replacement)
  {
    return new Text(this.value.replace(substring.value, replacement.value));
  }
  
  public Text insert(Text substring, Cursor position)
  {
    var left = value.substring(0, position.offset());
    var right = value.substring(position.offset());
    var middle = substring.value;
    var totalLength = left.length() + middle.length() + right.length();
    var inserted = new StringBuilder(totalLength).append(left).append(middle).append(right).toString();
    return new Text(inserted);
  }
  
  public Text delete(Cursor start, Cursor end)
  {
    var left = value.substring(0, start.offset());
    var right = value.substring(end.offset());
    var totalLength = left.length() + right.length();
    var removed = new StringBuilder(totalLength).append(left).append(right).toString();
    return new Text(removed);
  }
  
  private final String value;
  private final int length;
}
