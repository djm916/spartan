package spartan.builtins;

import spartan.runtime.VirtualMachine;
import spartan.data.Primitive;
import spartan.errors.TypeMismatch;
import spartan.data.Table;
import spartan.data.Nil;
import spartan.data.Int;
import static spartan.builtins.Core.truth;

public final class TableLib
{  
  public static final Primitive FROM_LIST = new Primitive(0, true) {
    public void apply(VirtualMachine vm) {
      vm.result = Table.fromList(vm.popRestArgs());
      vm.popFrame();
    }
  };
  
  // (table/length t)
  
  public static final Primitive LENGTH = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Table t))
        throw new TypeMismatch();
      vm.result = new Int(t.length());
      vm.popFrame();
    }
  };
  
  // (table/contains? t k)
  
  public static final Primitive CONTAINS = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Table t))
        throw new TypeMismatch();
      var k = vm.popArg();
      vm.result = truth(t.contains(k));
      vm.popFrame();
    }
  };
  
  // (table/find t k)
  
  public static final Primitive FIND = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Table t))
        throw new TypeMismatch();
      var k = vm.popArg();
      vm.result = t.find(k);
      vm.popFrame();
    }
  };
  
  // (table/find-or t k v)
  
  public static final Primitive FIND_OR = new Primitive(3, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Table t))
        throw new TypeMismatch();
      var k = vm.popArg();
      var v = vm.popArg();
      vm.result = t.findOrElse(k, v);
      vm.popFrame();
    }
  };
  
  // (table/assoc! t k v)
  
  public static final Primitive ASSOC = new Primitive(3, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Table t))
        throw new TypeMismatch();
      var k = vm.popArg();
      var v = vm.popArg();
      t.assoc(k, v);
      vm.result = Nil.VALUE;
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
