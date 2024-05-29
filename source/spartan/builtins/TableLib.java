package spartan.builtins;

import spartan.runtime.VirtualMachine;
import spartan.data.Primitive;
import spartan.data.Signature;
import spartan.data.Table;
import spartan.data.Nil;
import spartan.data.Int;
import spartan.data.Bool;
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
