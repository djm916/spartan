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
  
  public String value()
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
  
  public static Text format(Text template, List args)
  {
    return new Text(format(template.value, args));
  }
  
  public ByteVector encode(String encoding)
  {
    try {
      return new ByteVector(value.getBytes(encoding));
    }
    catch (java.io.UnsupportedEncodingException ex) {
      throw new Error("unsupported encoding " + encoding);
    }
  }
  
  private static String format(String template, List args)
  {
    var result = new StringBuilder();
    int n = template.length();
    
    for (int i = 0; i < n; ++i) {
      char c = template.charAt(i);
      if (c == '%') {
        if (i + 1 < n && template.charAt(i + 1) == '%') {
          result.append('%');
          ++i;
        }
        else {
          if (args.empty())
            throw new Error("too few arguments supplied in text format template");
          result.append(args.car().repr());
          args = args.cdr();
        }
      }
      else {
        result.append(c);
      }
    }
    
    return result.toString();
  }
  
  private final String value;
}
