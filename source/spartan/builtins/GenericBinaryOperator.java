package spartan.builtins;

import java.util.Map;
import java.util.EnumMap;
import java.util.function.BinaryOperator;
import spartan.data.Type;
import spartan.data.Datum;
import spartan.errors.TypeMismatch;

public class GenericBinaryOperator
{
  public final void addMethod(Type type, BinaryOperator<Datum> method)
  {
    methodTable.put(type, method);
  }
  
  public final Datum apply(Datum x, Datum y) throws TypeMismatch
  {
    if (x.type() != y.type())
      throw new TypeMismatch();
    
    var f = methodTable.get(x.type());
    
    if (f == null)
      throw new TypeMismatch();
    
    return f.apply(x, y);
  }

  private final Map<Type, BinaryOperator<Datum>> methodTable =
    new EnumMap<>(Type.class);
}
