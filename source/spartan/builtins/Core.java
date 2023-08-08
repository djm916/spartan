package spartan.builtins;

import spartan.data.*;
import spartan.errors.Error;
import spartan.errors.TypeMismatch;

public final class Core
{
  public static Bool truth(boolean x)
  {
    return x ? Bool.TRUE : Bool.FALSE;
  }
  
  public static boolean truth(Datum x)
  {
    return !(x == Bool.FALSE || x == Nil.VALUE);
  }
  
  public static Bool not(Datum x)
  {
    return truth(!truth(x));
  }
  
  public static boolean isEqual(Datum x, Datum y)
  {
    if (x instanceof IEq lhs && y instanceof IEq rhs)
      return lhs.isEqual(rhs);
    throw new TypeMismatch();
  }
  
  public static int compareTo(Datum x, Datum y)
  {
    if (x instanceof IOrd lhs && y instanceof IOrd rhs)
      return lhs.compareTo(rhs);
    throw new TypeMismatch();
  }
}
