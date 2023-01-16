package spartan.builtins;

import spartan.data.Datum;
import spartan.data.Primitive;
import spartan.data.List;
import spartan.data.Nil;
import spartan.runtime.VirtualMachine;
import spartan.errors.TypeMismatch;
import spartan.errors.InvalidArgument;

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
      if (vm.popArg() instanceof List list) {
        vm.result = list.car();
        vm.popFrame();
      }
      else throw new TypeMismatch();      
    }
  };
  
  public static final Primitive CADR = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.popArg() instanceof List list) {
        vm.result = list.cadr();
        vm.popFrame();
      }
      else throw new TypeMismatch();   
    }
  };
  
  public static final Primitive CADDR = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.popArg() instanceof List list) {
        vm.result = list.caddr();
        vm.popFrame();
      }
      else throw new TypeMismatch();   
    }
  };
  
  public static final Primitive CDR = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.popArg() instanceof List list) {
        vm.result = list.cdr();
        vm.popFrame();
      }
      else throw new TypeMismatch();
    }
  };
  
  public static final Primitive CDDR = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.popArg() instanceof List list) {
        vm.result = list.cddr();
        vm.popFrame();
      }
      else throw new TypeMismatch();   
    }
  };
  
  public static final Primitive CDDDR = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.popArg() instanceof List list) {
        vm.result = list.cdddr();
        vm.popFrame();
      }
      else throw new TypeMismatch();   
    }
  };
  
  public static final Primitive CONS = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      var car = vm.popArg();
      if (vm.popArg() instanceof List cdr) {
        vm.result = List.cons(car, cdr);
        vm.popFrame();
      }
      else throw new TypeMismatch();
    }
  };
  
  public static final Primitive APPEND = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      if (vm.popArg() instanceof List list) {
        vm.result = list.append(vm.popArg());
        vm.popFrame();
      }
      else throw new TypeMismatch();
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
      if (vm.popArg() instanceof List list) {
        vm.result = list.reverse();
        vm.popFrame();
      }
      else throw new TypeMismatch();
    }
  };  
  
  public static final Primitive SET_CAR = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      if (vm.popArg() instanceof List list) {
        list.setCar(vm.popArg());
        vm.result = Nil.VALUE;
        vm.popFrame();
      }
      else throw new TypeMismatch();
    }
  };
  
  public static final Primitive SET_CDR = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      if (vm.popArg() instanceof List list &&
          vm.popArg() instanceof List cdr) {
        list.setCdr(cdr);
        vm.result = Nil.VALUE;
        vm.popFrame();
      }
      else throw new TypeMismatch();
    }
  };
}
