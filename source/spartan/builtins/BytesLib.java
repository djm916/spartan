package spartan.builtins;

import spartan.errors.TypeMismatch;
import spartan.data.Type;
import spartan.data.Primitive;
import spartan.data.Bytes;
import spartan.data.Int;
import spartan.runtime.VirtualMachine;

public final class BytesLib
{  
  public static final Primitive MAKE_BYTES = new Primitive(1, true) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.INT)
        throw new TypeMismatch();
      vm.result = new Bytes(((Int) vm.popArg()).value);
      vm.popFrame();
    }
  };
  
  public static final Primitive BYTES_REF = new Primitive(2, true) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.BYTES)
        throw new TypeMismatch();
      var bytes = (Bytes) vm.popArg();
      if (vm.peekArg().type() != Type.INT)
        throw new TypeMismatch();
      var index = (Int) vm.popArg();
      vm.result = bytes.get(index);
      vm.popFrame();
    }
  };
  
  private BytesLib() {}
}
