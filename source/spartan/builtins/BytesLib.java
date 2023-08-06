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
  
  // (bytes/ref v i)
  
  public static final Primitive REF = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Bytes v && vm.popArg() instanceof IInt i))
        throw new TypeMismatch();
      vm.result = new Int(v.ref(i.intValue()));
      vm.popFrame();
    }
  };
  
  // (bytes/set! v i e)
  
  public static final Primitive SET = new Primitive(3, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Bytes v && vm.popArg() instanceof IInt i && vm.popArg() instanceof IInt e))
        throw new TypeMismatch();
      v.set(i.intValue(), (byte) e.intValue());
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
  
  // (bytes/length v)
  
  public static final Primitive LENGTH = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Bytes v))
        throw new TypeMismatch();
      vm.result = new Int(v.length());
      vm.popFrame();
    }
  };
  
  // (bytes/make n)
  
  public static final Primitive MAKE = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof IInt n))
        throw new TypeMismatch();
      vm.result = new Bytes(n.intValue());
      vm.popFrame();
    }
  };
  
  private BytesLib() {}
}
