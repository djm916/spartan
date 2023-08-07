package spartan.builtins;

import spartan.data.*;
import spartan.runtime.VirtualMachine;
import spartan.errors.TypeMismatch;

public final class StringLib
{  
  public static final Primitive CONCAT = new Primitive(0, true) {
    public void apply(VirtualMachine vm) {
      vm.result = Text.concat(vm.popRestArgs());
      vm.popFrame();
    }
  };
  
  public static final Primitive JOIN = new Primitive(1, true) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Text delim))
        throw new TypeMismatch();
      vm.result = Text.join(delim, vm.popRestArgs());
      vm.popFrame();      
    }
  };
  
  public static final Primitive SUBSTR = new Primitive(3, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Text text && vm.popArg() instanceof IInt start && vm.popArg() instanceof IInt end))
        throw new TypeMismatch();
      vm.result = text.substring(start.intValue(), end.intValue());
      vm.popFrame();
    }
  };
  
  public static final Primitive REVERSE = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Text text))
        throw new TypeMismatch();
      vm.result = text.reverse();
      vm.popFrame();
    }
  };
  
  public static final Primitive HASH = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Text text))
        throw new TypeMismatch();
      vm.result = Int.valueOf(text.hashCode());
      vm.popFrame();
    }
  };
  
  public static final Primitive LENGTH = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Text text))
        throw new TypeMismatch();
      vm.result = Int.valueOf(text.length());
      vm.popFrame();
    }
  };
}
