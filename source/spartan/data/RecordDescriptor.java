package spartan.data;

import spartan.errors.TypeMismatch;
import spartan.errors.NoSuchElement;
import spartan.runtime.VirtualMachine;

/** A record type descriptor.
 *
 * A record type descriptor (RTD) stores metadata about a class of records, and
 * provides a procedural interface for manipulating records.
 *
 * Currently, the following metadata is available:
 * <ul>
 *   <li>The record type name</li>
 *   <li>The record field names</li>
 *   <li>Constructor, predicate, field accessor, field mutator procedures</li>
 * </ul>
 */
public record RecordDescriptor(Type instanceType, Symbol name, Symbol[] fields) implements Datum
{
  @Override // Datum
  public Type type()
  {
    return Type.RECORD_DESC;
  }
    
  public int slot(Symbol field)
  {
    for (int i = 0; i < fields.length; ++i)
      if (fields[i] == field)
        return i;
    throw new NoSuchElement();
  }
  
  public IFun accessor(Symbol field)
  {
    final var rtd = this;
    final int slot = slot(field);
    return new Primitive(Signature.fixed(1)) {
      public void apply(VirtualMachine vm) {
        if (!(vm.popArg() instanceof Record record && record.type().id() == rtd.instanceType().id()))
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
        if (!(vm.popArg() instanceof Record record && record.type().id() == rtd.instanceType().id()))
          throw new TypeMismatch();
        record.set(slot, vm.popArg());
        vm.result = Nil.VALUE;
        vm.popFrame();
      }
    };
  }
  
  public IFun constructor()
  {
    final var rtd = this;
    return new Primitive(Signature.fixed(rtd.fields().length)) {
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
        vm.result = Bool.valueOf(vm.popArg() instanceof Record record && record.type().id() == rtd.instanceType().id());
        vm.popFrame();
      }
    };
  }  
}
