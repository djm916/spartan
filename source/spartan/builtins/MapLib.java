package spartan.builtins;

import spartan.runtime.VirtualMachine;
import spartan.data.Primitive;
import spartan.errors.TypeMismatch;
import spartan.data.Map;
import spartan.data.Int;
import spartan.data.Nil;

public final class MapLib
{  
  public static final Primitive FROM_LIST = new Primitive(0, true) {
    public void apply(VirtualMachine vm) {
      vm.result = Map.fromList(vm.popRestArgs());
      vm.popFrame();
    }
  };
  
  public static final Primitive LENGTH = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Map map))
        throw new TypeMismatch();
      vm.result = new Int(map.length());
      vm.popFrame();
    }
  };
  
  public static final Primitive GET = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Map map))
        throw new TypeMismatch();
      var key = vm.popArg();
      vm.result = map.get(key);
      vm.popFrame();
    }
  };
  
  public static final Primitive SET = new Primitive(3, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Map map))
        throw new TypeMismatch();
      var key = vm.popArg();
      var val = vm.popArg();
      map.put(key, val);
      vm.result = Nil.VALUE;
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
