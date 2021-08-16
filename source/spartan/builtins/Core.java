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
  
  public static final PrimFun Not = new PrimFun(1, false) {
    public void doApply(VirtualMachine vm) throws TypeMismatch {
      vm.result = not(vm.popArg());
      vm.popFrame();
    }
  };

  public static boolean eq(Datum x, Datum y) throws TypeMismatch
  {
    if (x.type() == y.type()) {
      switch (x.type()) {
        case Int: return ((Int)x).eq((Int)y);
        case Real: return ((Real)x).eq((Real)y);
        case Text: return ((Text)x).eq((Text)y);
        case Vector: return ((Vector)x).eq((Vector)y);
        case List: return ((List)x).eq((List)y);
        case Record: return ((Record)x).eq((Record)y);
        case Bool: 
        case Symbol: return x == y;
      }
    }
    throw new TypeMismatch();
  }
  
  public static final PrimFun Eq = new PrimFun(2, false) {
    public void doApply(VirtualMachine vm) throws TypeMismatch {
      vm.result = truth(eq(vm.popArg(), vm.popArg()));
      vm.popFrame();
    }
  };

  public static boolean ne(Datum x, Datum y) throws TypeMismatch
  {
    return !eq(x, y);
  }
  
  public static final PrimFun Ne = new PrimFun(2, false) {
    public void doApply(VirtualMachine vm) throws TypeMismatch {
      vm.result = truth(ne(vm.popArg(), vm.popArg()));
      vm.popFrame();
    }
  };

  public static boolean lt(Datum x, Datum y) throws TypeMismatch
  {
    if (x.type() == y.type()) {
      switch (x.type()) {
        case Int: return ((Int)x).compare((Int)y) < 0;
        case Real: return ((Real)x).compare((Real)y) < 0;
        case Text: return ((Text)x).compare((Text)y) < 0;
      }
    }
    throw new TypeMismatch();
  }
  
  public static final PrimFun Lt = new PrimFun(2, false) {
    public void doApply(VirtualMachine vm) throws TypeMismatch {
      vm.result = truth(lt(vm.popArg(), vm.popArg()));
      vm.popFrame();
    }
  };

  public static boolean le(Datum x, Datum y) throws TypeMismatch
  {
    if (x.type() == y.type()) {
      switch (x.type()) {
        case Int: return ((Int)x).compare((Int)y) <= 0;
        case Real: return ((Real)x).compare((Real)y) <= 0;
        case Text: return ((Text)x).compare((Text)y) <= 0;
      }
    }
    throw new TypeMismatch();
  }
  
  public static final PrimFun Le = new PrimFun(2, false) {
    public void doApply(VirtualMachine vm) throws TypeMismatch {
      vm.result = truth(le(vm.popArg(), vm.popArg()));
      vm.popFrame();
    }
  };

  public static boolean gt(Datum x, Datum y) throws TypeMismatch
  {
    if (x.type() == y.type()) {
      switch (x.type()) {
        case Int: return ((Int)x).compare((Int)y) > 0;
        case Real: return ((Real)x).compare((Real)y) > 0;
        case Text: return ((Text)x).compare((Text)y) > 0;
      }
    }
    throw new TypeMismatch();
  }
  
  public static final PrimFun Gt = new PrimFun(2, false) {
    public void doApply(VirtualMachine vm) throws TypeMismatch {
      vm.result = truth(gt(vm.popArg(), vm.popArg()));
      vm.popFrame();
    }
  };

  public static boolean ge(Datum x, Datum y) throws TypeMismatch
  {
    if (x.type() == y.type()) {
      switch (x.type()) {
        case Int: return ((Int)x).compare((Int)y) >= 0;
        case Real: return ((Real)x).compare((Real)y) >= 0;
        case Text: return ((Text)x).compare((Text)y) >= 0;
      }
    }
    throw new TypeMismatch();
  }
  
  public static final PrimFun Ge = new PrimFun(2, false) {
    public void doApply(VirtualMachine vm) throws TypeMismatch {
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
  
  public static final PrimFun Car = new PrimFun(1, false) {
    public void doApply(VirtualMachine vm) throws TypeMismatch {
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
  
  public static final PrimFun Cdr = new PrimFun(1, false) {
    public void doApply(VirtualMachine vm) throws TypeMismatch {
      vm.result = cdr(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static final PrimFun Cadr = new PrimFun(1, false) {
    public void doApply(VirtualMachine vm) throws TypeMismatch {
      vm.result = car(cdr(vm.popArg()));
      vm.popFrame();
    }
  };
  
  public static List cons(Datum first, Datum rest) throws TypeMismatch
  {
    if (rest.type() != Type.List)
      throw new TypeMismatch();
    return new List(first, (List)rest);
  }
  
  public static final PrimFun Cons = new PrimFun(2, false) {
    public void doApply(VirtualMachine vm) throws TypeMismatch {
      vm.result = cons(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };
  
  public static final PrimFun MakeList = new PrimFun(0, true) {
    public void doApply(VirtualMachine vm) {
      vm.result = vm.popArgs();
      vm.popFrame();
    }
  };
  
  public static final PrimFun MakeVector = new PrimFun(0, true) {
    public void doApply(VirtualMachine vm) {
      vm.result = Vector.fromList(vm.popArgs());
      vm.popFrame();
    }
  };
  
  public static final PrimFun MakeRecord = new PrimFun(0, true) {
    public void doApply(VirtualMachine vm) throws TypeMismatch {
      vm.result = Record.fromList(vm.popArgs());
      vm.popFrame();
    }
  };
  
  public static final PrimFun Apply = new PrimFun(2, false) {
    public void doApply(VirtualMachine vm) throws Error {
      vm.result = vm.popArg();
      if (vm.peekArg().type() != Type.List)
        throw new TypeMismatch();
      vm.args = (List)vm.popArg();
      vm.apply(vm.args.length());
    }
  };
  
  public static final PrimFun Print = new PrimFun(1, false) {
    public void doApply(VirtualMachine vm) {
      System.out.println(vm.popArg().repr());
      vm.result = Nil.Instance;
      vm.popFrame();
    }
  };
}
