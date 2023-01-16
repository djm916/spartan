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
  public static final Primitive CONCAT = new Primitive(1, true) {
    public void apply(VirtualMachine vm) {
      if (! (vm.peekArg() instanceof Text))
        throw new TypeMismatch();
      var text = (Text) vm.popArg();
      vm.result = text.concat(vm.popRestArgs());
      vm.popFrame();
    }
  };
  
  public static final Primitive JOIN = new Primitive(1, true) {
    public void apply(VirtualMachine vm) {
      if (! (vm.peekArg() instanceof Text))
        throw new TypeMismatch();
      var delimiter = (Text) vm.popArg();
      vm.result = delimiter.join(vm.popRestArgs());
      vm.popFrame();
    }
  };
  
  public static final Primitive SUBSTR = new Primitive(3, false) {
    public void apply(VirtualMachine vm) {
      if (! (vm.peekArg() instanceof Text))
        throw new TypeMismatch();
      var text = (Text) vm.popArg();
      if (! (vm.peekArg() instanceof IInt))
        throw new TypeMismatch();
      var start = ((IInt) vm.popArg()).intValue();
      if (! (vm.peekArg() instanceof IInt))
        throw new TypeMismatch();
      var end = ((IInt) vm.popArg()).intValue();
      vm.result = text.substring(start, end);
      vm.popFrame();
    }
  };
  
  public static final Primitive REVERSE = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (! (vm.peekArg() instanceof Text))
        throw new TypeMismatch();
      var text = (Text) vm.popArg();
      vm.result = text.reverse();
      vm.popFrame();
    }
  };
  
  public static final Primitive HASH = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (! (vm.peekArg() instanceof Text))
        throw new TypeMismatch();
      vm.result = new Int(((Text)vm.popArg()).hashCode());
      vm.popFrame();
    }
  };
}
