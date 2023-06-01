package spartan.builtins;

import spartan.runtime.VirtualMachine;
import spartan.data.Primitive;
import spartan.errors.TypeMismatch;
import spartan.data.Table;

public final class TableLib
{  
  public static final Primitive FROM_LIST = new Primitive(0, true) {
    public void apply(VirtualMachine vm) {
      vm.result = Table.fromList(vm.popRestArgs());
      vm.popFrame();
    }
  };
    
  public static final Primitive KEYS = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Table t))
        throw new TypeMismatch();
      vm.result = t.keys();
      vm.popFrame();
    }
  };
  
  public static final Primitive VALUES = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Table t))
        throw new TypeMismatch();
      vm.result = t.values();
      vm.popFrame();
    }
  };
  
  public static final Primitive ENTRIES = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Table t))
        throw new TypeMismatch();
      vm.result = t.entries();
      vm.popFrame();
    }
  };
}
