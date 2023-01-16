package spartan.builtins;

import spartan.data.Type;
import spartan.data.Primitive;
import spartan.data.Text;
import spartan.data.List;
import spartan.data.Int;
import spartan.data.IInt;
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
      if (vm.popArg() instanceof Text delim) {
        vm.result = Text.join(delim, vm.popRestArgs());
        vm.popFrame();
      }
      else throw new TypeMismatch();
    }
  };
  
  public static final Primitive SUBSTR = new Primitive(3, false) {
    public void apply(VirtualMachine vm) {
      if (vm.popArg() instanceof Text text &&
          vm.popArg() instanceof IInt start &&
          vm.popArg() instanceof IInt end) {
        vm.result = text.substring(start.intValue(), end.intValue());
        vm.popFrame();
      }
      else throw new TypeMismatch();
    }
  };
  
  public static final Primitive REVERSE = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.popArg() instanceof Text text) {
        vm.result = text.reverse();
        vm.popFrame();
      }
      else throw new TypeMismatch();
    }
  };
  
  public static final Primitive HASH = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.popArg() instanceof Text text) {
        vm.result = new Int(text.hashCode());
        vm.popFrame();
      }
      else throw new TypeMismatch();
    }
  };
}
