package spartan.builtins;

import spartan.data.*;
import spartan.data.Void; // shadows java.lang.Void
import spartan.errors.TypeMismatch;
import spartan.runtime.VirtualMachine;

public final class BytesLib
{
  // (bytes ...)
  
  public static final Primitive FROM_LIST = new Primitive(Signature.variadic(0)) {
    public void apply(VirtualMachine vm) {      
      vm.result = Bytes.fromList(vm.popRestArgs());
      vm.popFrame();
    }
  };
    
  // (make-bytes n)
  
  public static final Primitive NEW = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof IInt n))
        throw new TypeMismatch();
      vm.result = new Bytes(n.intValue());
      vm.popFrame();
    }
  };
  
  private BytesLib() {}
}
