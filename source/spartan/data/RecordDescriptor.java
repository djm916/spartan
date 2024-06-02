package spartan.data;

import java.util.Map;
import java.util.IdentityHashMap;
import java.util.Optional;
import spartan.errors.MultipleDefinition;
import spartan.errors.TypeMismatch;
import spartan.errors.NoSuchElement;
import spartan.runtime.VirtualMachine;

public class RecordDescriptor
{
  public static Optional<RecordDescriptor> forName(Symbol name)
  {
    return Optional.ofNullable(registry.get(name));
  }
  
  public static RecordDescriptor register(Symbol name, List fields)
  {
    if (registry.containsKey(name))
      throw new MultipleDefinition(name);
    var numSlots = fields.length();
    var result = new RecordDescriptor(name, numSlots);
    for (; !fields.isEmpty(); fields = fields.cdr()) {
      if (!(fields.car() instanceof Symbol field))
        throw new TypeMismatch();
      result.slotMap.put(field, result.slotMap.size());
    }
    registry.put(name, result);
    return result;
  }
    
  private RecordDescriptor(Symbol name, int numSlots)
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
  
  public IFun accessor(Symbol field)
  {
    final int typeId = type().id();
    final int slot = slot(field);
    return new Primitive(Signature.fixed(1)) {
      public void apply(VirtualMachine vm) {
        if (!(vm.popArg() instanceof Record record && record.type().id() == typeId))
          throw new TypeMismatch();
        vm.result = record.get(slot);
        vm.popFrame();
      }
    };
  }
  
  public IFun mutator(Symbol field)
  {
    final int typeId = type().id();
    final int slot = slot(field);
    return new Primitive(Signature.fixed(2)) {
      public void apply(VirtualMachine vm) {
        if (!(vm.popArg() instanceof Record record && record.type().id() == typeId))
          throw new TypeMismatch();
        record.set(slot, vm.popArg());
        vm.result = Void.VALUE;
        vm.popFrame();
      }
    };
  }
  
  public IFun constructor()
  {
    final var type = type();
    final int numSlots = numSlots();
    return new Primitive(Signature.fixed(numSlots)) {
      public void apply(VirtualMachine vm) {
        vm.result = new Record(type, vm.popRestArgs().toArray());
        vm.popFrame();
      }
    };
  }
  
  public IFun predicate()
  {
    final int typeId = type().id();
    return new Primitive(Signature.fixed(1)) {
      public void apply(VirtualMachine vm) {
        vm.result = Bool.valueOf(vm.popArg() instanceof Record record && record.type().id() == typeId);
        vm.popFrame();
      }
    };
  }
  
  public IFun destructor()
  {
    final int typeId = type().id();
    return new Primitive(Signature.fixed(1)) {
      public void apply(VirtualMachine vm) {
        if (!(vm.popArg() instanceof Record record && record.type().id() == typeId))
          throw new TypeMismatch();
        vm.result = List.of(record.fieldValues());
        vm.popFrame();
      }
    };
  }
  
  private static final Map<Symbol, RecordDescriptor> registry = new IdentityHashMap<>();
  private final Symbol name;
  private final Type type;
  private final Map<Symbol, Integer> slotMap;
}
