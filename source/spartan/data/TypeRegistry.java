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
  
  public static Type forName(Symbol typeName)
  {
    return registry.get(typeName);
  }
  
  private TypeRegistry() {}
}
