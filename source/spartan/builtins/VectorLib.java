package spartan.builtins;

import spartan.data.*;
import spartan.errors.TypeMismatch;
import spartan.runtime.VirtualMachine;

public final class VectorLib
{  
  // (vector e...)
  
  public static final Primitive FROM_LIST = new Primitive(0, true) {
    public void apply(VirtualMachine vm) {
      vm.result = Vector.fromList(vm.popRestArgs());
      vm.popFrame();
    }
  };
  
  // (vector/make n e)
  
  public static final Primitive MAKE = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof IInt n))
        throw new TypeMismatch();      
      vm.result = new Vector(n.toInt32(), vm.popArg());
      vm.popFrame();      
    }
  };
  
  // (vector/ref v i)
  
  public static final Primitive REF = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Vector vector && vm.popArg() instanceof IInt index))
        throw new TypeMismatch();
      vm.result = vector.ref(index.toInt32());
      vm.popFrame();
    }
  };
  
  // (vector/set! v i e)
  
  public static final Primitive SET = new Primitive(3, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Vector vector && vm.popArg() instanceof IInt index))
        throw new TypeMismatch();
      var item = vm.popArg();
      vector.set(index.toInt32(), item);
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
  
  // (vector/length v)
  
  public static final Primitive LENGTH = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Vector vector))
        throw new TypeMismatch();
      vm.result = Int.valueOf(vector.length());
      vm.popFrame();
    }
  };
  
  // (vector/copy v)
  
  public static final Primitive COPY = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Vector vector))
        throw new TypeMismatch();      
      vm.result = new Vector(vector);
      vm.popFrame();
    }
  };
  
  // (vector/append! v e)
  
  public static final Primitive APPEND = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Vector v))
        throw new TypeMismatch();
      var e = vm.popArg();
      v.append(e);
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
  
  // (vector/insert! v i e)
  
  public static final Primitive INSERT = new Primitive(3, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Vector v && vm.popArg() instanceof IInt i))
        throw new TypeMismatch();
      var e = vm.popArg();
      v.insert(i.toInt32(), e);
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
  
  // (vector/remove! v i)
  
  public static final Primitive REMOVE = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Vector v && vm.popArg() instanceof IInt i))
        throw new TypeMismatch();
      v.remove(i.toInt32());
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
}
