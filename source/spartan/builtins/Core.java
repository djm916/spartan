package spartan.builtins;

import spartan.data.*;
import spartan.errors.Error;
import spartan.runtime.VirtualMachine;
import spartan.errors.TypeMismatch;

public final class Core
{
  public static Bool truth(boolean x)
  {
    return x ? Bool.True : Bool.False;
  }
  
  public static boolean truth(Datum x)
  {
    return !(x == Bool.False || x == Nil.Instance);
  }
  
  public static Bool not(Datum x) throws TypeMismatch
  {
    if (x.type() == Type.Bool)
      return ((Bool)x).not();
    throw new TypeMismatch();
  }
  
  public static final Primitive Not = new Primitive(1, false) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
      vm.result = not(vm.popArg());
      vm.popFrame();
    }
  };

  public static boolean eq(Datum x, Datum y) throws TypeMismatch
  {
    if (x.type() == y.type()) {
      switch (x.type()) {
        case Int: return Int.eq((Int)x, (Int)y);
        case Real: return ((Real)x).eq((Real)y);
        case Text: return ((Text)x).eq((Text)y);
        case Vector: return ((Vector)x).eq((Vector)y);
        case List: return ((List)x).eq((List)y);
        case Map: return ((Map)x).eq((Map)y);
        case Bool: 
        case Symbol: return x == y;
      }
    }
    throw new TypeMismatch();
  }
  
  public static final Primitive Eq = new Primitive(2, false) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
      vm.result = truth(eq(vm.popArg(), vm.popArg()));
      vm.popFrame();
    }
  };

  public static boolean ne(Datum x, Datum y) throws TypeMismatch
  {
    return !eq(x, y);
  }
  
  public static final Primitive Ne = new Primitive(2, false) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
      vm.result = truth(ne(vm.popArg(), vm.popArg()));
      vm.popFrame();
    }
  };

  public static boolean lt(Datum x, Datum y) throws TypeMismatch
  {
    if (x.type() == y.type()) {
      switch (x.type()) {
        case Int: return Int.compare((Int)x, (Int)y) < 0;
        case Real: return ((Real)x).compare((Real)y) < 0;
        case Text: return ((Text)x).compare((Text)y) < 0;
      }
    }
    throw new TypeMismatch();
  }
  
  public static final Primitive Lt = new Primitive(2, false) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
      vm.result = truth(lt(vm.popArg(), vm.popArg()));
      vm.popFrame();
    }
  };

  public static boolean le(Datum x, Datum y) throws TypeMismatch
  {
    if (x.type() == y.type()) {
      switch (x.type()) {
        case Int: return Int.compare((Int)x, (Int)y) <= 0;
        case Real: return ((Real)x).compare((Real)y) <= 0;
        case Text: return ((Text)x).compare((Text)y) <= 0;
      }
    }
    throw new TypeMismatch();
  }
  
  public static final Primitive Le = new Primitive(2, false) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
      vm.result = truth(le(vm.popArg(), vm.popArg()));
      vm.popFrame();
    }
  };

  public static boolean gt(Datum x, Datum y) throws TypeMismatch
  {
    if (x.type() == y.type()) {
      switch (x.type()) {
        case Int: return Int.compare((Int)x, (Int)y) > 0;
        case Real: return ((Real)x).compare((Real)y) > 0;
        case Text: return ((Text)x).compare((Text)y) > 0;
      }
    }
    throw new TypeMismatch();
  }
  
  public static final Primitive Gt = new Primitive(2, false) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
      vm.result = truth(gt(vm.popArg(), vm.popArg()));
      vm.popFrame();
    }
  };

  public static boolean ge(Datum x, Datum y) throws TypeMismatch
  {
    if (x.type() == y.type()) {
      switch (x.type()) {
        case Int: return Int.compare((Int)x, (Int)y) >= 0;
        case Real: return ((Real)x).compare((Real)y) >= 0;
        case Text: return ((Text)x).compare((Text)y) >= 0;
      }
    }
    throw new TypeMismatch();
  }
  
  public static final Primitive Ge = new Primitive(2, false) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
      vm.result = truth(ge(vm.popArg(), vm.popArg()));
      vm.popFrame();
    }
  };
  
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
  
  public static final Primitive MakeList = new Primitive(0, true) {
    public void apply(VirtualMachine vm) {
      vm.result = vm.popArgs();
      vm.popFrame();
    }
  };
  
  public static final Primitive MakeVector = new Primitive(0, true) {
    public void apply(VirtualMachine vm) {
      vm.result = Vector.fromList(vm.popArgs());
      vm.popFrame();
    }
  };
  
  public static final Primitive MakeRecord = new Primitive(0, true) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
      vm.result = Map.fromList(vm.popArgs());
      vm.popFrame();
    }
  };
  
  public static final Complex makeComplex(Datum x, Datum y) throws TypeMismatch
  {
    if (x.type() != Type.Real || y.type() != Type.Real)
      throw new TypeMismatch();
    return new Complex((Real)x, (Real)y);
  }
  
  public static final Primitive MakeComplex = new Primitive(2, false) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
      vm.result = makeComplex(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };
  
  public static final Primitive Apply = new Primitive(2, false) {
    public void apply(VirtualMachine vm) throws Error {
      vm.result = vm.popArg();
      if (vm.peekArg().type() != Type.List)
        throw new TypeMismatch();
      vm.args = (List)vm.popArg();
      vm.apply(vm.args.length());
    }
  };
  
  public static final Primitive Print = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      System.out.println(vm.popArg().repr());
      vm.result = Nil.Instance;
      vm.popFrame();
    }
  };
  
  public static int length(Datum x) throws TypeMismatch
  {
    switch (x.type()) {
      case List: return ((List)x).length();
      case Vector: return ((Vector)x).length();
      case Map: return ((Map)x).length();
      case Text: return ((Text)x).length();
    }
    throw new TypeMismatch();
  }
  
  public static final Primitive Length = new Primitive(1, false) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
      vm.result = new Int(length(vm.popArg()));
      vm.popFrame();
    }
  };
  
  public static final Primitive TypeOf = new Primitive(1, false) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
      vm.result = Symbol.get(vm.popArg().type().name);
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
