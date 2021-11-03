package spartan.builtins;

import spartan.data.*;
import spartan.errors.TypeMismatch;
import spartan.errors.NoSuchElement;
import spartan.runtime.VirtualMachine;

public final class VectorLib
{
  public static final Primitive New = new Primitive(2, false) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
      if (vm.peekArg().type() != Type.Int)
        throw new TypeMismatch();
      var size = (Int) vm.popArg();
      var init = vm.popArg();
      vm.result = Vector.create(size, init);
      vm.popFrame();
    }
  };
  
  public static final Primitive Copy = new Primitive(1, false) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
      if (vm.peekArg().type() != Type.Vector)
        throw new TypeMismatch();
      vm.result = ((Vector) vm.popArg()).copy();
      vm.popFrame();
    }
  };
  
  public static final Primitive Set = new Primitive(3, false) {
    public void apply(VirtualMachine vm) throws TypeMismatch, NoSuchElement {
      if (vm.peekArg().type() != Type.Vector)
        throw new TypeMismatch();
      var vector = (Vector) vm.popArg();
      if (vm.peekArg().type() != Type.Int)
        throw new TypeMismatch();
      var index = (Int) vm.popArg();
      var value = vm.popArg();
      vm.result = vector.set(index, value);
      vm.popFrame();
    }
  };
}
