package spartan.builtins;

import spartan.runtime.VirtualMachine;
import spartan.data.Primitive;
import spartan.errors.TypeMismatch;
import spartan.data.Map;

public final class MapLib
{  
  public static final Primitive FROM_LIST = new Primitive(0, true) {
    public void apply(VirtualMachine vm) {
      vm.result = Map.fromList(vm.popRestArgs());
      vm.popFrame();
    }
  };
    
  public static final Primitive KEYS = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Map map))
        throw new TypeMismatch();
      vm.result = map.keys();
      vm.popFrame();
    }
  };
  
  public static final Primitive VALUES = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Map map))
        throw new TypeMismatch();
      vm.result = map.values();
      vm.popFrame();
    }
  };
  
  public static final Primitive ENTRIES = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Map map))
        throw new TypeMismatch();
      vm.result = map.entries();
      vm.popFrame();
    }
  };
}
