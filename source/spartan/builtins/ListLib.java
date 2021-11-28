package spartan.builtins;

import spartan.data.Datum;
import spartan.data.Type;
import spartan.data.Primitive;
import spartan.data.List;
import spartan.data.Nil;
import spartan.runtime.VirtualMachine;
import spartan.errors.TypeMismatch;

public final class ListLib
{
  public static Datum car(Datum x) throws TypeMismatch
  {
    if (x.type() != Type.List || x == List.Empty)
      throw new TypeMismatch();
    return ((List)x).car();
  }
  
  public static List cdr(Datum x) throws TypeMismatch
  {
    if (x.type() != Type.List || x == List.Empty)
      throw new TypeMismatch();
    return ((List)x).cdr();
  }
  
  public static List cons(Datum first, Datum rest) throws TypeMismatch
  {
    if (rest.type() != Type.List)
      throw new TypeMismatch();
    return List.cons(first, (List)rest);
  }

  public static List append(Datum x, Datum y) throws TypeMismatch
  {
    if (x.type() != Type.List)
      throw new TypeMismatch();
    return ((List)x).append(y);
  }
  
  public static List concat(List args) throws TypeMismatch
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
  
  public static final Primitive Car = new Primitive(1, false) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
      vm.result = car(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static final Primitive Cadr = new Primitive(1, false) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
      vm.result = car(cdr(vm.popArg()));
      vm.popFrame();
    }
  };
  
  public static final Primitive Caddr = new Primitive(1, false) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
      vm.result = car(cdr(cdr(vm.popArg())));
      vm.popFrame();
    }
  };
  
  public static final Primitive Cdr = new Primitive(1, false) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
      vm.result = cdr(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static final Primitive Cddr = new Primitive(1, false) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
      vm.result = cdr(cdr(vm.popArg()));
      vm.popFrame();
    }
  };
  
  public static final Primitive Cdddr = new Primitive(1, false) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
      vm.result = cdr(cdr(cdr(vm.popArg())));
      vm.popFrame();
    }
  };
  
  public static final Primitive Cons = new Primitive(2, false) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
      vm.result = cons(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };
  
  public static final Primitive Append = new Primitive(2, false) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
      vm.result = append(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };
    
  public static final Primitive Concat = new Primitive(0, true) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
      vm.result = concat(vm.popRestArgs());
      vm.popFrame();
    }
  };  
  
  public static final Primitive Reverse = new Primitive(1, false) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
      if (vm.peekArg().type() != Type.List)
        throw new TypeMismatch();
      vm.result = ((List)vm.popArg()).reverse();
      vm.popFrame();
    }
  };
  
  public static final Primitive SetCar = new Primitive(2, false) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
      if (vm.peekArg().type() != Type.List)
        throw new TypeMismatch();
      var list = (List)vm.popArg();
      var car = vm.popArg();
      list.setCar(car);
      vm.result = Nil.Instance;
      vm.popFrame();
    }
  };
  
  public static final Primitive SetCdr = new Primitive(2, false) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
      if (vm.peekArg().type() != Type.List)
        throw new TypeMismatch();
      var list = (List)vm.popArg();
      if (vm.peekArg().type() != Type.List)
        throw new TypeMismatch();
      var cdr = (List)vm.popArg();
      list.setCdr(cdr);
      vm.result = Nil.Instance;
      vm.popFrame();
    }
  };
}
