package spartan.builtins;

import spartan.data.*;
import spartan.errors.TypeMismatch;
import spartan.runtime.VirtualMachine;

public final class VectorLib
{
  // (vector ...)
  
  public static final Primitive FROM_LIST = new Primitive(Signature.variadic(0)) {
    public void apply(VirtualMachine vm) {
      vm.result = Vector.fromList(vm.popRestArgs());
      vm.popFrame();
    }
  };
  
  // (make-vector length [init])
  
  public static final Primitive MAKE = new Primitive(Signature.variadic(1, 1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof IInt length))
        throw new TypeMismatch();
      var init = vm.args.isEmpty() ? Nil.VALUE : vm.popArg();
      vm.result = new Vector(length.intValue(), init);
      vm.popFrame();      
    }
  };
  
  // (vector-ref vec idx)
  
  public static final Primitive REF = new Primitive(Signature.fixed(2)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Vector vector && vm.popArg() instanceof IInt index))
        throw new TypeMismatch();
      vm.result = vector.get(index.intValue());
      vm.popFrame();
    }
  };
  
  // (vector-set! vec idx val)
  
  public static final Primitive SET = new Primitive(Signature.fixed(3)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Vector vector && vm.popArg() instanceof IInt index))
        throw new TypeMismatch();
      var item = vm.popArg();
      vector.set(index.intValue(), item);
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
  
  // (vector-length vec)
  
  public static final Primitive LENGTH = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Vector vector))
        throw new TypeMismatch();
      vm.result = Int.valueOf(vector.length());
      vm.popFrame();
    }
  };
  
  // (vector-copy vec)
  
  public static final Primitive COPY = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Vector vector))
        throw new TypeMismatch();      
      vm.result = new Vector(vector);
      vm.popFrame();
    }
  };
  
  public static final Primitive APPEND = new Primitive(Signature.fixed(2)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Vector v))
        throw new TypeMismatch();
      var e = vm.popArg();
      v.append(e);
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
  
  public static final Primitive INSERT = new Primitive(Signature.fixed(3)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Vector v && vm.popArg() instanceof IInt i))
        throw new TypeMismatch();
      var e = vm.popArg();
      v.insert(i.intValue(), e);
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
  
  public static final Primitive REMOVE = new Primitive(Signature.fixed(2)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Vector v && vm.popArg() instanceof IInt i))
        throw new TypeMismatch();
      v.remove(i.intValue());
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
}
