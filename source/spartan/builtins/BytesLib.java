package spartan.builtins;

import spartan.errors.TypeMismatch;
import spartan.data.Primitive;
import spartan.data.Bytes;
import spartan.data.IInt;
import spartan.runtime.VirtualMachine;

public final class BytesLib
{
  // (bytes ...)
  
  public static final Primitive FROM_LIST = new Primitive(0, true) {
    public void apply(VirtualMachine vm) {      
      vm.result = Bytes.fromList(vm.popRestArgs());
      vm.popFrame();
    }
  };
  
  // (bytes/make capacity)
  
  public static final Primitive MAKE = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof IInt capacity))
        throw new TypeMismatch();
      vm.result = new Bytes(capacity.intValue());
      vm.popFrame();
    }
  };
  
  private BytesLib() {}
}
