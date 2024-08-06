package spartan.data;

import java.util.Map;
import java.util.IdentityHashMap;
import java.util.Optional;
import java.util.Set;
import spartan.errors.MultipleDefinition;
import spartan.errors.TypeMismatch;
import spartan.errors.NoSuchElement;
import spartan.runtime.VirtualMachine;

public class RecordDescriptor implements Datum
{
  public RecordDescriptor(Symbol name, Set<Symbol> fields)
  {
    this.instanceType = TypeRegistry.register(name);
    this.fieldSlotMap = new IdentityHashMap<>(fields.size());
    for (var field : fields) {
      fieldSlotMap.put(field, fieldSlotMap.size());
    }
  }
  
  @Override // Datum
  public Type type()
  {
    return TypeRegistry.RECORD_DESC_TYPE;
  }
  
  public Type instanceType()
  {
    return instanceType;
  }
  
  public int slot(Symbol field)
  {
    var offset = fieldSlotMap.get(field);
    if (offset == null)
      throw new NoSuchElement();
    return offset;
  }
  
  public int numFields()
  {
    return fieldSlotMap.size();
  }
  
  public Set<Symbol> fieldNames()
  {
    return fieldSlotMap.keySet();
  }
  
  public IFun accessor(Symbol field)
  {
    final var rtd = this;
    final int slot = slot(field);
    return new Primitive(Signature.fixed(1)) {
      public void apply(VirtualMachine vm) {
        if (!(vm.popArg() instanceof Record record && record.type().id() == rtd.instanceType.id()))
          throw new TypeMismatch();
        vm.result = record.get(slot);
        vm.popFrame();
      }
    };
  }
  
  public IFun mutator(Symbol field)
  {
    final var rtd = this;
    final int slot = slot(field);
    return new Primitive(Signature.fixed(2)) {
      public void apply(VirtualMachine vm) {
        if (!(vm.popArg() instanceof Record record && record.type().id() == rtd.instanceType.id()))
          throw new TypeMismatch();
        record.set(slot, vm.popArg());
        vm.result = Void.VALUE;
        vm.popFrame();
      }
    };
  }
  
  public IFun constructor()
  {
    final var rtd = this;
    return new Primitive(Signature.fixed(rtd.numFields())) {
      public void apply(VirtualMachine vm) {
        vm.result = new Record(rtd, vm.popRestArgs().toArray());
        vm.popFrame();
      }
    };
  }
  
  public IFun predicate()
  {
    final var rtd = this;
    return new Primitive(Signature.fixed(1)) {
      public void apply(VirtualMachine vm) {
        vm.result = Bool.valueOf(vm.popArg() instanceof Record record && record.type().id() == rtd.instanceType.id());
        vm.popFrame();
      }
    };
  }
  
  public IFun destructor()
  {
    final var rtd = this;
    return new Primitive(Signature.fixed(1)) {
      public void apply(VirtualMachine vm) {
        if (!(vm.popArg() instanceof Record record && record.type().id() == rtd.instanceType.id()))
          throw new TypeMismatch();
        vm.result = List.of(record.fieldValues());
        vm.popFrame();
      }
    };
  }
  
  private final Type instanceType;
  private final Map<Symbol, Integer> fieldSlotMap;
}
