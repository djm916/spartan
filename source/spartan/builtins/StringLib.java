package spartan.builtins;

import spartan.data.*;
import spartan.runtime.VirtualMachine;
import spartan.errors.TypeMismatch;

public final class StringLib
{
  // (string:concat ...)
  
  public static final Primitive CONCAT = new Primitive(Signature.variadic(0)) {
    public void apply(VirtualMachine vm) {
      vm.result = Text.concat(vm.popRestArgs());
      vm.popFrame();
    }
  };
  
  // (string:join delim ...)
  
  public static final Primitive JOIN = new Primitive(Signature.variadic(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Text delim))
        throw new TypeMismatch();
      vm.result = Text.join(delim, vm.popRestArgs());
      vm.popFrame();      
    }
  };
  
  // (string:substring string start [end])
  
  public static final Primitive SUBSTR = new Primitive(Signature.variadic(2, 1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Text str))
        throw new TypeMismatch();
      if (!(vm.popArg() instanceof IInt start))
        throw new TypeMismatch();
      if (!((vm.args.isEmpty() ? str.length() : vm.popArg()) instanceof IInt end))
        throw new TypeMismatch();
      vm.result = str.substring(start.intValue(), end.intValue());
      vm.popFrame();
    }
  };
  
  // (string:reverse string)
  
  public static final Primitive REVERSE = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Text text))
        throw new TypeMismatch();
      vm.result = text.reverse();
      vm.popFrame();
    }
  };
  
  // (string:hash string)
  
  public static final Primitive HASH = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Text text))
        throw new TypeMismatch();
      vm.result = Int.valueOf(text.hashCode());
      vm.popFrame();
    }
  };
  
  /**
  
  // (string-ref str idx)
  
  public static final Primitive REF = new Primitive(Signature.fixed(2)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Text str && vm.popArg() instanceof IInt index))
        throw new TypeMismatch();
      vm.result = Text.get(index.intValue());
      vm.popFrame();
    }
  };
  
  // (string-set! str idx val)
  
  public static final Primitive SET = new Primitive(Signature.fixed(3)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Text str && vm.popArg() instanceof IInt index))
        throw new TypeMismatch();
      var item = vm.popArg();
      str.set(index.intValue(), item);
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
  
  */
  
  // (string:length string)
  
  public static final Primitive LENGTH = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Text text))
        throw new TypeMismatch();
      vm.result = Int.valueOf(text.length());
      vm.popFrame();
    }
  };
}
