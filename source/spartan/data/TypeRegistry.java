package spartan.data;

import java.util.Map;
import java.util.IdentityHashMap;
import spartan.errors.MultipleDefinition;

public final class TypeRegistry
{
  private static int nextTypeId = 0;
  private static final Map<Symbol, Type> registry = new IdentityHashMap<>();
  
  public static final Type VOID_TYPE = register(Symbol.of("void"));
  public static final Type BOOLEAN_TYPE = register(Symbol.of("boolean"));
  public static final Type SYMBOL_TYPE = register(Symbol.of("symbol"));
  public static final Type INTEGER_TYPE = register(Symbol.of("integer"));
  public static final Type REAL_TYPE = register(Symbol.of("real"));
  public static final Type RATIONAL_TYPE = register(Symbol.of("rational"));
  public static final Type COMPLEX_TYPE = register(Symbol.of("complex"));
  public static final Type PROCEDURE_TYPE = register(Symbol.of("procedure"));
  public static final Type MACRO_TYPE = register(Symbol.of("macro"));
  public static final Type LIST_TYPE = register(Symbol.of("list"));
  public static final Type VECTOR_TYPE = register(Symbol.of("vector"));
  public static final Type TABLE_TYPE = register(Symbol.of("table"));  
  public static final Type BYTES_TYPE = register(Symbol.of("bytes"));
  public static final Type STRING_TYPE = register(Symbol.of("string"));
  public static final Type PORT_TYPE = register(Symbol.of("port"));
  public static final Type PACKAGE_TYPE = register(Symbol.of("package"));
  public static final Type RECORD_DESC_TYPE = register(Symbol.of("record-descriptor"));
  
  public static Type register(Symbol typeName)
  {
    if (registry.containsKey(typeName))
      throw new MultipleDefinition(typeName);
    registry.put(typeName, new Type(nextTypeId++, typeName.name()));
    return registry.get(typeName);
  }
  
  private TypeRegistry() {}
}
