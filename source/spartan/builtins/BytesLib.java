package spartan.builtins;

import spartan.errors.TypeMismatch;
import spartan.data.Primitive;
import spartan.data.Bytes;
import spartan.data.IInt;
import spartan.data.Int;
import spartan.data.Nil;
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
    
  // (bytes/make n)
  
  public static final Primitive NEW = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof IInt n))
        throw new TypeMismatch();
      vm.result = new Bytes(n.intValue());
      vm.popFrame();
    }
  };
  
  private BytesLib() {}
}
