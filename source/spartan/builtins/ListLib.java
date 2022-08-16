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
    if (rest.type() != Type.List)
      throw new TypeMismatch();
    return List.cons(first, (List)rest);
  }

  public static Datum car(Datum x)
  {
    if (x.type() != Type.List)
      throw new TypeMismatch();
    if (x == List.Empty)
      throw new InvalidArgument();
    return ((List)x).car();
  }
  
  public static List cdr(Datum x)
  {
    if (x.type() != Type.List)
      throw new TypeMismatch();
    if (x == List.Empty)
      throw new InvalidArgument();
    return ((List)x).cdr();
  }

  public static void setCar(Datum x, Datum y)
  {
    if (x.type() != Type.List)
      throw new TypeMismatch();
    if (x == List.Empty)
      throw new InvalidArgument();
    ((List)x).setCar(y);
  }
  
  public static void setCdr(Datum x, Datum y)
  {
    if (x.type() != Type.List || y.type() != Type.List)
      throw new TypeMismatch();
    if (x == List.Empty)
      throw new InvalidArgument();
    ((List)x).setCdr((List)y);
  }
  
  public static List append(Datum x, Datum y)
  {
    if (x.type() != Type.List)
      throw new TypeMismatch();
    return ((List)x).append(y);
  }
  
  public static List concat(List args)
  {
    var builder = new List.Builder();
    for (; args != List.Empty; args = args.cdr()) {
      if (args.car().type() != Type.List)
        throw new TypeMismatch();
      for (var list = (List)args.car(); list != List.Empty; list = list.cdr())
        builder.add(list.car());
    }
    return builder.build();
  }
  
  public static final Primitive MakeList = new Primitive(0, true) {
    public void apply(VirtualMachine vm) {
      vm.result = vm.popRestArgs();
      vm.popFrame();
    }
  };
    
  public static final Primitive Car = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = car(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static final Primitive Caar = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = car(car(vm.popArg()));
      vm.popFrame();
    }
  };
  
  public static final Primitive Cadr = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = car(cdr(vm.popArg()));
      vm.popFrame();
    }
  };
  
  public static final Primitive Caddr = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = car(cdr(cdr(vm.popArg())));
      vm.popFrame();
    }
  };
  
  public static final Primitive Cdr = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = cdr(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static final Primitive Cddr = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = cdr(cdr(vm.popArg()));
      vm.popFrame();
    }
  };
  
  public static final Primitive Cdddr = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = cdr(cdr(cdr(vm.popArg())));
      vm.popFrame();
    }
  };
  
  public static final Primitive Cons = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = cons(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };
  
  public static final Primitive Append = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = append(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };
    
  public static final Primitive Concat = new Primitive(0, true) {
    public void apply(VirtualMachine vm) {
      vm.result = concat(vm.popRestArgs());
      vm.popFrame();
    }
  };  
  
  public static final Primitive Reverse = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.List)
        throw new TypeMismatch();
      vm.result = ((List)vm.popArg()).reverse();
      vm.popFrame();
    }
  };
  
  public static final Primitive SetCar = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      setCar(vm.popArg(), vm.popArg());
      vm.result = Nil.Value;
      vm.popFrame();
    }
  };
  
  public static final Primitive SetCdr = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      setCdr(vm.popArg(), vm.popArg());
      vm.result = Nil.Value;
      vm.popFrame();
    }
  };
}
