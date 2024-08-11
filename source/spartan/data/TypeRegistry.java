package spartan.data;

import java.util.Map;
import java.util.IdentityHashMap;
import spartan.errors.MultipleDefinition;

/**
 * A global registry of all builtin and user defined types in the system.
 *
 * Each type is identified by a name (symbol) which is unique throughout the system
 */
public final class TypeRegistry
{
  private static int nextTypeId = 0;
  private static final Map<Symbol, Type> registry = new IdentityHashMap<>();
  
  //
  // Define built-in types
  //
  
  /** The builtin "void" type */
  public static final Type VOID_TYPE = register(Symbol.of("void"));
  /** The builtin "boolean" type */
  public static final Type BOOLEAN_TYPE = register(Symbol.of("boolean"));
  /** The builtin "symbol" type */
  public static final Type SYMBOL_TYPE = register(Symbol.of("symbol"));
  /** The builtin "integer" type */
  public static final Type INTEGER_TYPE = register(Symbol.of("integer"));
  /** The builtin "real" type */
  public static final Type REAL_TYPE = register(Symbol.of("real"));
  /** The builtin "rational" type */
  public static final Type RATIONAL_TYPE = register(Symbol.of("rational"));
  /** The builtin "complex" type */
  public static final Type COMPLEX_TYPE = register(Symbol.of("complex"));
  /** The builtin "procedure" type */
  public static final Type PROCEDURE_TYPE = register(Symbol.of("procedure"));
  /** The builtin "macro" type */
  public static final Type MACRO_TYPE = register(Symbol.of("macro"));
  /** The builtin "list" type */
  public static final Type LIST_TYPE = register(Symbol.of("list"));
  /** The builtin "vector" type */
  public static final Type VECTOR_TYPE = register(Symbol.of("vector"));
  /** The builtin "bytes" type */
  public static final Type BYTES_TYPE = register(Symbol.of("bytes"));
  /** The builtin "string" type */
  public static final Type STRING_TYPE = register(Symbol.of("string"));
  /** The builtin "string-cursor" type */
  public static final Type STRING_CURSOR_TYPE = register(Symbol.of("string-cursor"));
  /** The builtin "port" type */
  public static final Type PORT_TYPE = register(Symbol.of("port"));
  /** The builtin "package" type */
  public static final Type PACKAGE_TYPE = register(Symbol.of("package"));
  /** The builtin "record descriptor" type */
  public static final Type RECORD_DESC_TYPE = register(Symbol.of("record-descriptor"));
  
  /** Registers a new type in the system
   *
   * @param typeName the type name, a (possibly qualified) symbol
   * @throws MultipleDefinition if the type has already been registered
   */
  public static Type register(Symbol typeName)
  {
    if (registry.containsKey(typeName))
      throw new MultipleDefinition(typeName);
    registry.put(typeName, new Type(nextTypeId++, typeName.name()));
    return registry.get(typeName);
  }
  
  private TypeRegistry() {}
}
