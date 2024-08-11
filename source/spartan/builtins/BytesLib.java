package spartan.builtins;

import spartan.data.*;
import spartan.data.Void; // shadows java.lang.Void
import spartan.errors.TypeMismatch;
import spartan.errors.InvalidArgument;
import spartan.runtime.VirtualMachine;

/**
 * Contains implementations of {@code bytes}-related builtin procedures.
 */
public final class BytesLib
{
  // (bytes ...)
  
  public static final Primitive FROM_LIST = new Primitive(Signature.variadic(0)) {
    public void apply(VirtualMachine vm) {      
      vm.result = Bytes.fromList(vm.popRestArgs());
      vm.popFrame();
    }
  };
    
  // (make-bytes len [fill])
  
  public static final Primitive MAKE = new Primitive(Signature.variadic(1, 1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof IInt len))
        throw new TypeMismatch();
      if (!((vm.args.isEmpty() ? Int.valueOf(0) : vm.popArg()) instanceof IInt fill))
        throw new TypeMismatch();
      vm.result = new Bytes(len.intValue(), fill.byteValue());
      vm.popFrame();
    }
  };
  
  // (bytes-ref bytes i)
  
  public static final Primitive REF = new Primitive(Signature.fixed(2)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Bytes b))
        throw new TypeMismatch();
      if (!(vm.popArg() instanceof IInt i))
        throw new TypeMismatch();
      vm.result = Int.valueOf(b.get(i.intValue()));
      vm.popFrame();
    }
  };
  
  // (bytes-set! bytes i x)
  
  public static final Primitive SET = new Primitive(Signature.fixed(3)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Bytes b))
        throw new TypeMismatch();
      if (!(vm.popArg() instanceof IInt i))
        throw new TypeMismatch();
      if (!(vm.popArg() instanceof IInt x))
        throw new TypeMismatch();
      b.set(i.intValue(), x.byteValue());
      vm.result = Void.VALUE;
      vm.popFrame();
    }
  };
  
  // (bytes-length bytes)
  
  public static final Primitive LENGTH = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Bytes b))
        throw new TypeMismatch();
      vm.result = Int.valueOf(b.length());
      vm.popFrame();
    }
  };
  
  private BytesLib() {}
}
