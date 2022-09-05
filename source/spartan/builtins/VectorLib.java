package spartan.builtins;

import spartan.data.*;
import spartan.errors.TypeMismatch;
import spartan.runtime.VirtualMachine;

public final class VectorLib
{  
  public static final Primitive MakeVector = new Primitive(0, true) {
    public void apply(VirtualMachine vm) {
      vm.result = Vector.fromList(vm.popRestArgs());
      vm.popFrame();
    }
  };
  
  public static final Primitive Length = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.Vector)
        throw new TypeMismatch();
      vm.result = new Int(((Vector)vm.popArg()).length());
      vm.popFrame();
    }
  };
  
  public static final Primitive New = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.Int)
        throw new TypeMismatch();
      var length = (Int) vm.popArg();
      var init = vm.popArg();
      vm.result = Vector.create(length, init);
      vm.popFrame();
    }
  };
  
  public static final Primitive Copy = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.Vector)
        throw new TypeMismatch();
      vm.result = ((Vector) vm.popArg()).copy();
      vm.popFrame();
    }
  };
  
  public static final Primitive Ref = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.Vector)
        throw new TypeMismatch();
      var vector = (Vector) vm.popArg();
      if (vm.peekArg().type() != Type.Int)
        throw new TypeMismatch();
      var index = (Int) vm.popArg();
      vm.result = vector.get(index);
      vm.popFrame();
    }
  };
  
  public static final Primitive Set = new Primitive(3, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.Vector)
        throw new TypeMismatch();
      var vector = (Vector) vm.popArg();
      if (vm.peekArg().type() != Type.Int)
        throw new TypeMismatch();
      var index = (Int) vm.popArg();
      var value = vm.popArg();
      vector.set(index, value);
      vm.result = Nil.Value;
      vm.popFrame();
    }
  };
  
  public static final Primitive Append = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.Vector)
        throw new TypeMismatch();
      var vector = (Vector) vm.popArg();
      vector.append(vm.popArg());
      vm.result = Nil.Value;
      vm.popFrame();
    }
  };
}
