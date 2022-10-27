package spartan.builtins;

import spartan.data.*;
import spartan.errors.TypeMismatch;
import spartan.runtime.VirtualMachine;

public final class MappingLib
{  
  public static final Primitive FROM_LIST = new Primitive(0, true) {
    public void apply(VirtualMachine vm) {
      vm.result = Mapping.fromList(vm.popRestArgs());
      vm.popFrame();
    }
  };
  
  public static final Primitive LENGTH = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.MAPPING)
        throw new TypeMismatch();
      var map = (Mapping) vm.popArg();
      vm.result = new Int(map.length());
      vm.popFrame();
    }
  };
  
  public static final Primitive GET = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.MAPPING)
        throw new TypeMismatch();
      var map = (Mapping) vm.popArg();
      var key = vm.popArg();
      vm.result = map.get(key);
      vm.popFrame();
    }
  };
  
  public static final Primitive SET = new Primitive(3, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.MAPPING)
        throw new TypeMismatch();
      var map = (Mapping) vm.popArg();
      var key = vm.popArg();
      var value = vm.popArg();
      map.put(key, value);
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
  
  public static final Primitive KEYS = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.MAPPING)
        throw new TypeMismatch();
      var map = (Mapping) vm.popArg();
      vm.result = map.keys();
      vm.popFrame();
    }
  };
  
  public static final Primitive VALUES = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.MAPPING)
        throw new TypeMismatch();
      var map = (Mapping) vm.popArg();
      vm.result = map.values();
      vm.popFrame();
    }
  };
  
  public static final Primitive ENTRIES = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.MAPPING)
        throw new TypeMismatch();
      var map = (Mapping) vm.popArg();
      vm.result = map.entries();
      vm.popFrame();
    }
  };
}
