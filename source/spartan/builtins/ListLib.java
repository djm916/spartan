package spartan.builtins;

import spartan.data.Datum;
import spartan.data.Type;
import spartan.data.Primitive;
import spartan.data.List;
import spartan.data.Nil;
import spartan.runtime.VirtualMachine;
import spartan.errors.TypeMismatch;
import spartan.errors.InvalidArgument;

public final class ListLib
{
  public static List cons(Datum first, Datum rest)
  {
    if (rest.type() != Type.LIST)
      throw new TypeMismatch();
    return List.cons(first, (List)rest);
  }

  public static Datum car(Datum x)
  {
    if (x.type() != Type.LIST)
      throw new TypeMismatch();
    if (x == List.EMPTY)
      throw new InvalidArgument();
    return ((List)x).car();
  }
  
  public static List cdr(Datum x)
  {
    if (x.type() != Type.LIST)
      throw new TypeMismatch();
    if (x == List.EMPTY)
      throw new InvalidArgument();
    return ((List)x).cdr();
  }

  public static void setCar(Datum x, Datum y)
  {
    if (x.type() != Type.LIST)
      throw new TypeMismatch();
    if (x == List.EMPTY)
      throw new InvalidArgument();
    ((List)x).setCar(y);
  }
  
  public static void setCdr(Datum x, Datum y)
  {
    if (x.type() != Type.LIST || y.type() != Type.LIST)
      throw new TypeMismatch();
    if (x == List.EMPTY)
      throw new InvalidArgument();
    ((List)x).setCdr((List)y);
  }
  
  public static List append(Datum x, Datum y)
  {
    if (x.type() != Type.LIST)
      throw new TypeMismatch();
    return ((List)x).append(y);
  }
  
  public static List concat(List args)
  {
    var builder = new List.Builder();
    for (; args != List.EMPTY; args = args.cdr()) {
      if (args.car().type() != Type.LIST)
        throw new TypeMismatch();
      for (var list = (List)args.car(); list != List.EMPTY; list = list.cdr())
        builder.add(list.car());
    }
    return builder.build();
  }
  
  public static final Primitive MAKE_LIST = new Primitive(0, true) {
    public void apply(VirtualMachine vm) {
      vm.result = vm.popRestArgs();
      vm.popFrame();
    }
  };
    
  public static final Primitive CAR = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = car(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static final Primitive CAAR = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = car(car(vm.popArg()));
      vm.popFrame();
    }
  };
  
  public static final Primitive CADR = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = car(cdr(vm.popArg()));
      vm.popFrame();
    }
  };
  
  public static final Primitive CADDR = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = car(cdr(cdr(vm.popArg())));
      vm.popFrame();
    }
  };
  
  public static final Primitive CDR = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = cdr(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static final Primitive CDDR = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = cdr(cdr(vm.popArg()));
      vm.popFrame();
    }
  };
  
  public static final Primitive CDDDR = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = cdr(cdr(cdr(vm.popArg())));
      vm.popFrame();
    }
  };
  
  public static final Primitive CONS = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = cons(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };
  
  public static final Primitive APPEND = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = append(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };
    
  public static final Primitive CONCAT = new Primitive(0, true) {
    public void apply(VirtualMachine vm) {
      vm.result = concat(vm.popRestArgs());
      vm.popFrame();
    }
  };  
  
  public static final Primitive REVERSE = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.LIST)
        throw new TypeMismatch();
      vm.result = ((List)vm.popArg()).reverse();
      vm.popFrame();
    }
  };
  
  public static final Primitive SET_CAR = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      setCar(vm.popArg(), vm.popArg());
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
  
  public static final Primitive SET_CDR = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      setCdr(vm.popArg(), vm.popArg());
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
}
