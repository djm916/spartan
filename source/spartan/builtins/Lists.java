package spartan.builtins;

import spartan.data.Datum;
import spartan.data.Type;
import spartan.data.Primitive;
import spartan.data.List;
import spartan.runtime.VirtualMachine;
import spartan.errors.TypeMismatch;

public final class Lists
{
  public static Datum car(Datum x) throws TypeMismatch
  {
    if (x.type() != Type.List || x == List.Empty)
      throw new TypeMismatch();
    return ((List)x).car();
  }
  
  public static final Primitive Car = new Primitive(1, false) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
      vm.result = car(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static List cdr(Datum x) throws TypeMismatch
  {
    if (x.type() != Type.List || x == List.Empty)
      throw new TypeMismatch();
    return ((List)x).cdr();
  }
  
  public static final Primitive Cdr = new Primitive(1, false) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
      vm.result = cdr(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static final Primitive Cadr = new Primitive(1, false) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
      vm.result = car(cdr(vm.popArg()));
      vm.popFrame();
    }
  };
  
  public static List cons(Datum first, Datum rest) throws TypeMismatch
  {
    if (rest.type() != Type.List)
      throw new TypeMismatch();
    return List.cons(first, (List)rest);
  }
  
  public static final Primitive Cons = new Primitive(2, false) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
      vm.result = cons(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };
  
  public static final List concat(Datum x, Datum y) throws TypeMismatch
  {
    if (x.type() != Type.List || y.type() != Type.List)
      throw new TypeMismatch();
    return ((List)x).concat((List)y);
  }
  
  public static final Primitive Concat = new Primitive(2, false) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
      vm.result = concat(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };
}
