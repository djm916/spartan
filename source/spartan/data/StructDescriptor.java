package spartan.data;

import java.util.Map;
import java.util.IdentityHashMap;
import java.util.Optional;
import spartan.errors.MultipleDefinition;
import spartan.errors.TypeMismatch;
import spartan.errors.NoSuchElement;

public class StructDescriptor
{
  public static Optional<StructDescriptor> forName(Symbol name)
  {
    return Optional.ofNullable(registry.get(name));
  }
  
  public static StructDescriptor register(Symbol name, List fields)
  {
    if (registry.containsKey(name))
      throw new MultipleDefinition(name);
    var numSlots = fields.length();
    var result = new StructDescriptor(name, numSlots);
    for (; !fields.isEmpty(); fields = fields.cdr()) {
      if (!(fields.car() instanceof Symbol field))
        throw new TypeMismatch();
      result.slotMap.put(field, result.slotMap.size());
    }
    registry.put(name, result);
    return result;
  }
    
  private StructDescriptor(Symbol name, int numSlots)
  {
    this.name = name;
    this.type = TypeRegistry.register(name.str().intern());
    this.slotMap = new IdentityHashMap<>(numSlots);
  }
  
  int slot(Symbol field)
  {
    var offset = slotMap.get(field);
    if (offset == null)
      throw new NoSuchElement();
    return offset;
  }
  
  int numSlots()
  {
    return slotMap.size();
  }
  
  Type type()
  {
    return type;
  }
  
  private static final Map<Symbol, StructDescriptor> registry = new IdentityHashMap<>();
  private final Symbol name;
  private final Type type;
  private final Map<Symbol, Integer> slotMap;
}
