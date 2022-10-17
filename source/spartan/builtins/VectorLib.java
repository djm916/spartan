package spartan.builtins;

import spartan.data.*;
import spartan.errors.TypeMismatch;
import spartan.runtime.VirtualMachine;

public final class VectorLib
{  
  public static final Primitive FROM_LIST = new Primitive(0, true) {
    public void apply(VirtualMachine vm) {
      vm.result = Vector.fromList(vm.popRestArgs());
      vm.popFrame();
    }
  };
  
  public static final Primitive LENGTH = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.VECTOR)
        throw new TypeMismatch();
      var vector = (Vector) vm.popArg();
      vm.result = new Int(vector.length());
      vm.popFrame();
    }
  };
  
  public static final Primitive NEW = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      if (!vm.peekArg().type().isInt())
        throw new TypeMismatch();
      var length = ((Integral) vm.popArg()).intValue();
      var init = vm.popArg();
      vm.result = Vector.create(length, init);
      vm.popFrame();
    }
  };
  
  public static final Primitive COPY = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.VECTOR)
        throw new TypeMismatch();
      vm.result = new Vector((Vector) vm.popArg());
      vm.popFrame();
    }
  };
  
  public static final Primitive REF = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.VECTOR)
        throw new TypeMismatch();
      var vector = (Vector) vm.popArg();
      if (!vm.peekArg().type().isInt())
        throw new TypeMismatch();
      var index = ((Integral) vm.popArg()).intValue();
      vm.result = vector.get(index);
      vm.popFrame();
    }
  };
  
  public static final Primitive SET = new Primitive(3, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.VECTOR)
        throw new TypeMismatch();
      var vector = (Vector) vm.popArg();
      if (!vm.peekArg().type().isInt())
        throw new TypeMismatch();
      var index = ((Integral) vm.popArg()).intValue();
      var value = vm.popArg();
      vector.set(index, value);
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
  
  public static final Primitive APPEND = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.VECTOR)
        throw new TypeMismatch();
      var vector = (Vector) vm.popArg();
      vector.append(vm.popArg());
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
}
