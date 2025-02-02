package spartan.data;

/** Each {@code Type} instance represents a distinct type of Scheme value
 */
public record Type(int id, String name)
{
  //
  // Define built-in types
  //
  
  /** The builtin "nil" type */
  public static final Type NIL = TypeRegistry.register(Symbol.of("nil"));
  /** The builtin "boolean" type */
  public static final Type BOOLEAN = TypeRegistry.register(Symbol.of("boolean"));
  /** The builtin "symbol" type */
  public static final Type SYMBOL = TypeRegistry.register(Symbol.of("symbol"));
  /** The builtin "integer" type */
  public static final Type INTEGER = TypeRegistry.register(Symbol.of("integer"));
  /** The builtin "real" type */
  public static final Type REAL = TypeRegistry.register(Symbol.of("real"));
  /** The builtin "rational" type */
  public static final Type RATIONAL = TypeRegistry.register(Symbol.of("rational"));
  /** The builtin "complex" type */
  public static final Type COMPLEX = TypeRegistry.register(Symbol.of("complex"));
  /** The builtin "procedure" type */
  public static final Type PROCEDURE = TypeRegistry.register(Symbol.of("procedure"));
  /** The builtin "macro" type */
  public static final Type MACRO = TypeRegistry.register(Symbol.of("macro"));
  /** The builtin "list" type */
  public static final Type LIST = TypeRegistry.register(Symbol.of("list"));
  /** The builtin "vector" type */
  public static final Type VECTOR = TypeRegistry.register(Symbol.of("vector"));
  /** The builtin "bytes" type */
  public static final Type BYTES = TypeRegistry.register(Symbol.of("bytes"));
  /** The builtin "string" type */
  public static final Type STRING = TypeRegistry.register(Symbol.of("string"));
  /** The builtin "string-cursor" type */
  public static final Type STRING_CURSOR = TypeRegistry.register(Symbol.of("string-cursor"));
  /** The builtin "port" type */
  public static final Type PORT = TypeRegistry.register(Symbol.of("port"));
  /** The builtin "namespace" type */
  public static final Type NAMESPACE = TypeRegistry.register(Symbol.of("namespace"));
  /** The builtin "record descriptor" type */
  public static final Type RECORD_DESC = TypeRegistry.register(Symbol.of("record-descriptor"));  
};
