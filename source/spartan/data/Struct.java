package spartan.data;

import java.util.stream.Collectors;
import spartan.errors.TypeMismatch;
import spartan.runtime.VirtualMachine;
import spartan.compiling.Signature;

public final class Struct implements Datum, IAssoc, IFun
{
  public Struct(StructDescriptor desc, List values)
  {
    this.desc = desc;
    this.slots = values.toArray();
  }
  
  @Override // Datum
  public Type type()
  {
    return desc.type();
  }
  
  @Override // IAssoc
  public Datum get(Datum key)
  {
    if (!(key instanceof Symbol s))
      throw new TypeMismatch();
    return slots[desc.slot(s)];
  }
  
  @Override // IAssoc
  public void set(Datum key, Datum value)
  {
    if (!(key instanceof Symbol s))
      throw new TypeMismatch();
    slots[desc.slot(s)] = value;
  }
  
  @Override // IFun
  public void apply(VirtualMachine vm)
  {
    vm.result = get(vm.popArg());
    vm.popFrame();
  }
  
  @Override // IFun
  public Signature signature()
  {
    return SIG;
  }
  
  private static final Signature SIG = new Signature(1, false);
  private final StructDescriptor desc;
  private final Datum[] slots;
}
