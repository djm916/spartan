package spartan.data;

import static spartan.data.TypeRegistry.register;

/** Each {@code Type} instance represents a distinct type of Scheme value
 */
public record Type(int id, Symbol name)
{
  //
  // Define built-in types
  //
  
  /** The builtin "nil" type */
  public static final Type NIL = register(Symbol.of("spartan.base", "nil"));
  /** The builtin "boolean" type */
  public static final Type BOOLEAN = register(Symbol.of("spartan.base", "boolean"));
  /** The builtin "symbol" type */
  public static final Type SYMBOL = register(Symbol.of("spartan.base", "symbol"));
  /** The builtin "integer" type */
  public static final Type INTEGER = register(Symbol.of("spartan.base", "integer"));
  /** The builtin "real" type */
  public static final Type REAL = register(Symbol.of("spartan.base", "real"));
  /** The builtin "rational" type */
  public static final Type RATIONAL = register(Symbol.of("spartan.base", "rational"));
  /** The builtin "complex" type */
  public static final Type COMPLEX = register(Symbol.of("spartan.base", "complex"));
  /** The builtin "procedure" type */
  public static final Type PROCEDURE = register(Symbol.of("spartan.base", "procedure"));
  /** The builtin "macro" type */
  public static final Type MACRO = register(Symbol.of("spartan.base", "macro"));
  /** The builtin "list" type */
  public static final Type LIST = register(Symbol.of("spartan.base", "list"));
  /** The builtin "vector" type */
  public static final Type VECTOR = register(Symbol.of("spartan.base", "vector"));
  /** The builtin "bytes" type */
  public static final Type BYTES = register(Symbol.of("spartan.base", "bytes"));
  /** The builtin "string" type */
  public static final Type STRING = register(Symbol.of("spartan.base", "string"));
  /** The builtin "string-cursor" type */
  public static final Type STRING_CURSOR = register(Symbol.of("spartan.base", "string-cursor"));
  /** The builtin "port" type */
  public static final Type PORT = register(Symbol.of("spartan.base", "port"));
  /** The builtin "module" type */
  public static final Type MODULE = register(Symbol.of("spartan.base", "module"));
  /** The builtin "record descriptor" type */
  public static final Type RECORD_DESC = register(Symbol.of("spartan.base", "record-descriptor"));  
};
