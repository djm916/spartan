package spartan.data;

import java.util.Map;
import java.util.IdentityHashMap;

public final class TypeRegistry
{
  private static int nextTypeId = 0;
  private static final Map<String, Type> typeMap = new IdentityHashMap<>();
  
  public static final Type NIL_TYPE = register("nil");
  public static final Type BOOLEAN_TYPE = register("boolean");
  public static final Type SYMBOL_TYPE = register("symbol");
  public static final Type INTEGER_TYPE = register("integer");
  public static final Type REAL_TYPE = register("real");
  public static final Type RATIONAL_TYPE = register("rational");
  public static final Type COMPLEX_TYPE = register("complex");
  public static final Type PROCEDURE_TYPE = register("procedure");
  public static final Type LIST_TYPE = register("list");
  public static final Type VECTOR_TYPE = register("vector");
  public static final Type TABLE_TYPE = register("table");  
  public static final Type BYTES_TYPE = register("bytes");
  public static final Type STRING_TYPE = register("string");
  public static final Type PORT_TYPE = register("port");
  
  public static Type register(String typeName)
  {
    if (!typeMap.containsKey(typeName))
      typeMap.put(typeName, new Type(nextTypeId++, typeName));
    return typeMap.get(typeName);
  }
  
  private TypeRegistry() {}
}
