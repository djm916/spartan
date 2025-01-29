package spartan.builtins;

import spartan.data.Primitive;
import spartan.data.Signature;
import spartan.data.List;
import spartan.data.Nil;
import spartan.data.Int;
import spartan.data.IInt;
import spartan.data.Bool;
import spartan.runtime.VirtualMachine;
import spartan.errors.TypeMismatch;

public final class ListLib
{
  public static final Primitive MAKE_LIST = new Primitive(Signature.variadic(0)) {
    public void apply(VirtualMachine vm) {
      vm.result = vm.popRestArgs();
      vm.popFrame();
    }
  };
    
  public static final Primitive FIRST = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof List list))
        throw new TypeMismatch();
      vm.result = list.first();
      vm.popFrame();
    }
  };
  
  public static final Primitive SECOND = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof List list))
        throw new TypeMismatch();
      vm.result = list.second();
      vm.popFrame();      
    }
  };
  
  public static final Primitive THIRD = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof List list))
        throw new TypeMismatch();
      vm.result = list.third();
      vm.popFrame();  
    }
  };
  
  public static final Primitive REST = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof List list))
        throw new TypeMismatch();
      vm.result = list.rest();
      vm.popFrame();  
    }
  };
    
  public static final Primitive ADJOIN = new Primitive(Signature.fixed(2)) {
    public void apply(VirtualMachine vm) {
      var first = vm.popArg();
      if (!(vm.popArg() instanceof List rest))
        throw new TypeMismatch();
      vm.result = List.adjoin(first, rest);
      vm.popFrame();
    }
  };
  
  public static final Primitive APPEND = new Primitive(Signature.fixed(2)) {
    public void apply(VirtualMachine vm) {
      var elem = vm.popArg();
      if (!(vm.popArg() instanceof List list))
        throw new TypeMismatch();
      vm.result = list.append(elem); 
      vm.popFrame();  
    }
  };
  
  public static final Primitive CONCAT = new Primitive(Signature.variadic(0)) {
    public void apply(VirtualMachine vm) {
      vm.result = List.concat(vm.popRestArgs());
      vm.popFrame();
    }
  };  
  
  public static final Primitive REVERSE = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof List list))
        throw new TypeMismatch();
      vm.result = list.reverse();
      vm.popFrame();  
    }
  };  
  
  public static final Primitive SET_FIRST = new Primitive(Signature.fixed(2)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof List list))
        throw new TypeMismatch();
      list.setFirst(vm.popArg());
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
  
  public static final Primitive SET_REST = new Primitive(Signature.fixed(2)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof List list && vm.popArg() instanceof List rest))
        throw new TypeMismatch();
      list.setRest(rest);
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
  
  public static final Primitive TAKE = new Primitive(Signature.fixed(2)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof IInt n && vm.popArg() instanceof List list))
        throw new TypeMismatch();
      vm.result = list.take(n.intValue());
      vm.popFrame();
    }
  };
  
  public static final Primitive DROP = new Primitive(Signature.fixed(2)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof IInt n && vm.popArg() instanceof List list))
        throw new TypeMismatch();
      vm.result = list.drop(n.intValue());
      vm.popFrame();
    }
  };
  
  public static final Primitive LENGTH = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof List list))
        throw new TypeMismatch();
      vm.result = Int.valueOf(list.length());
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_EMPTY = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      vm.result = Bool.valueOf(vm.popArg() == List.EMPTY);
      vm.popFrame();
    }
  };
  
  public static final Primitive NTH = new Primitive(Signature.fixed(2)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof IInt n && vm.popArg() instanceof List list))
        throw new TypeMismatch();      
      vm.result = list.nth(n.intValue());
      vm.popFrame();
    }
  };
}
