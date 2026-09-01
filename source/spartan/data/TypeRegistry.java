package spartan.data;

import java.util.Map;
import java.util.IdentityHashMap;
import java.util.Optional;

/**
 * A global registry of all builtin and user defined types in the system.
 *
 * Each type is identified by its qualified name, which is unique throughout the system
 */
public final class TypeRegistry
{
  /** Registers a new type in the system
   *
   * @param typeName the type name, a (possibly qualified) symbol
   */
  public static Type register(Symbol typeName)
  {
    return registry.computeIfAbsent(typeName, (_) -> new Type(nextTypeId++, typeName));
  }
  
  public static Optional<Type> forName(Symbol typeName)
  {
    return Optional.ofNullable(registry.get(typeName));
  }
  
  private TypeRegistry() {}

  private static int nextTypeId = 0;
  private static final Map<Symbol, Type> registry = new IdentityHashMap<>();
}
