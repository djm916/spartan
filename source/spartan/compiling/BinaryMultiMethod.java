package spartan.builtins;

import spartan.data.Type;
import spartan.data.Datum;
import spartan.errors.TypeMismatch;
import java.util.EnumMap;
import java.util.function.BinaryOperator;

public class BinaryMultiMethod
{
  public void add(Type t1, Type t2, BinaryOperator<Datum> f)
  {
    methodTable.get(t1).put(t2, f);
  }
  
  public Datum apply(Datum x, Datum y)
  {
    return methodTable.get(x.type()).get(y.type()).apply(x, y);
  }
  
  private final EnumMap<Type, EnumMap<Type, BinaryOperator<Datum>>> methodTable = new EnumMap<>(Type.class);
  
  {
    for (Type t1 : Type.values()) {
      methodTable.put(t1, new EnumMap<Type, BinaryOperator<Datum>>(Type.class));
      for (Type t2 : Type.values()) {
        methodTable.get(t1).put(t2, (x, y) -> { throw new TypeMismatch(); } );
      }
    }
  }
}
