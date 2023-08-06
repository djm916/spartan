package spartan.builtins;

import spartan.data.Primitive;
import spartan.data.List;
import spartan.data.Nil;
import spartan.data.Int;
import spartan.runtime.VirtualMachine;
import spartan.errors.TypeMismatch;
import static spartan.builtins.Core.truth;

public final class ListLib
{
  public static final Primitive MAKE_LIST = new Primitive(0, true) {
    public void apply(VirtualMachine vm) {
      vm.result = vm.popRestArgs();
      vm.popFrame();
    }
  };
    
  public static final Primitive CAR = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof List list))
        throw new TypeMismatch();
      vm.result = list.car();
      vm.popFrame();
    }
  };
  
  public static final Primitive CADR = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof List list))
        throw new TypeMismatch();
      vm.result = list.cadr();
      vm.popFrame();      
    }
  };
  
  public static final Primitive CADDR = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof List list))
        throw new TypeMismatch();
      vm.result = list.caddr();
      vm.popFrame();  
    }
  };
  
  public static final Primitive CDR = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof List list))
        throw new TypeMismatch();
      vm.result = list.cdr();
      vm.popFrame();  
    }
  };
  
  public static final Primitive CDDR = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof List list))
        throw new TypeMismatch();
      vm.result = list.cddr();
      vm.popFrame();     
    }
  };
  
  public static final Primitive CDDDR = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof List list))
        throw new TypeMismatch();
      vm.result = list.cdddr();
      vm.popFrame();  
    }
  };
  
  public static final Primitive CONS = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      var car = vm.popArg();
      if (!(vm.popArg() instanceof List cdr))
        throw new TypeMismatch();
      vm.result = List.cons(car, cdr);
      vm.popFrame();
    }
  };
  
  public static final Primitive APPEND = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof List list))
        throw new TypeMismatch();
      vm.result = list.append(vm.popArg()); 
      vm.popFrame();  
    }
  };
  
  public static final Primitive CONCAT = new Primitive(0, true) {
    public void apply(VirtualMachine vm) {
      vm.result = List.concat(vm.popRestArgs());
      vm.popFrame();
    }
  };  
  
  public static final Primitive REVERSE = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof List list))
        throw new TypeMismatch();
      vm.result = list.reverse();
      vm.popFrame();  
    }
  };  
  
  public static final Primitive SET_CAR = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof List list))
        throw new TypeMismatch();
      list.setCar(vm.popArg());
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
  
  public static final Primitive SET_CDR = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof List list && vm.popArg() instanceof List cdr))
        throw new TypeMismatch();
      list.setCdr(cdr);
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
    
  public static final Primitive LENGTH = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof List list))
        throw new TypeMismatch();
      vm.result = new Int(list.length());
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_EMPTY = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof List list))
        throw new TypeMismatch();
      vm.result = truth(list.empty());
      vm.popFrame();
    }
  };

}
