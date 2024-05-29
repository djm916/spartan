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
    
  public static final Primitive CAR = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof List list))
        throw new TypeMismatch();
      vm.result = list.car();
      vm.popFrame();
    }
  };
  
  public static final Primitive CADR = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof List list))
        throw new TypeMismatch();
      vm.result = list.cadr();
      vm.popFrame();      
    }
  };
  
  public static final Primitive CADDR = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof List list))
        throw new TypeMismatch();
      vm.result = list.caddr();
      vm.popFrame();  
    }
  };
  
  public static final Primitive CDR = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof List list))
        throw new TypeMismatch();
      vm.result = list.cdr();
      vm.popFrame();  
    }
  };
  
  public static final Primitive CDDR = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof List list))
        throw new TypeMismatch();
      vm.result = list.cddr();
      vm.popFrame();     
    }
  };
  
  public static final Primitive CDDDR = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof List list))
        throw new TypeMismatch();
      vm.result = list.cdddr();
      vm.popFrame();  
    }
  };
  
  public static final Primitive CONS = new Primitive(Signature.fixed(2)) {
    public void apply(VirtualMachine vm) {
      var car = vm.popArg();
      if (!(vm.popArg() instanceof List cdr))
        throw new TypeMismatch();
      vm.result = List.cons(car, cdr);
      vm.popFrame();
    }
  };
  
  public static final Primitive APPEND = new Primitive(Signature.fixed(2)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof List list))
        throw new TypeMismatch();
      vm.result = list.append(vm.popArg()); 
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
  
  public static final Primitive SET_CAR = new Primitive(Signature.fixed(2)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof List list))
        throw new TypeMismatch();
      list.setCar(vm.popArg());
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
  
  public static final Primitive SET_CDR = new Primitive(Signature.fixed(2)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof List list && vm.popArg() instanceof List cdr))
        throw new TypeMismatch();
      list.setCdr(cdr);
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
    
  public static final Primitive NTH_CDR = new Primitive(Signature.fixed(2)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof List list && vm.popArg() instanceof IInt index))
        throw new TypeMismatch();
      vm.result = list.tail(index.intValue());
      vm.popFrame();
    }
  };
  
  public static final Primitive SET_NTH_CDR = new Primitive(Signature.fixed(3)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof List list && vm.popArg() instanceof IInt index && vm.popArg() instanceof List tail))
        throw new TypeMismatch();
      list.setTail(index.intValue(), tail);
      vm.result = Nil.VALUE;
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
      if (!(vm.popArg() instanceof List list))
        throw new TypeMismatch();
      vm.result = Bool.valueOf(list.isEmpty());
      vm.popFrame();
    }
  };
}
