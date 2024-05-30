package spartan.builtins;

import spartan.data.*;
import spartan.data.Void; // shadows java.lang.Void
import spartan.runtime.VirtualMachine;
import spartan.errors.TypeMismatch;

public final class TableLib
{  
  public static final Primitive FROM_LIST = new Primitive(Signature.variadic(0)) {
    public void apply(VirtualMachine vm) {
      vm.result = Table.fromList(vm.popRestArgs());
      vm.popFrame();
    }
  };
  
  public static final Primitive CONTAINS = new Primitive(Signature.fixed(2)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Table t))
        throw new TypeMismatch();
      var k = vm.popArg();
      vm.result = Bool.valueOf(t.contains(k));
      vm.popFrame();
    }
  };
  
  public static final Primitive KEYS = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Table t))
        throw new TypeMismatch();
      vm.result = t.keys();
      vm.popFrame();
    }
  };
  
  public static final Primitive VALUES = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Table t))
        throw new TypeMismatch();
      vm.result = t.values();
      vm.popFrame();
    }
  };
  
  public static final Primitive ENTRIES = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Table t))
        throw new TypeMismatch();
      vm.result = t.entries();
      vm.popFrame();
    }
  };
}
