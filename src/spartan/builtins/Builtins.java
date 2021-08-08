package spartan.builtins;

import spartan.data.*;
import spartan.errors.Error;
import spartan.runtime.VirtualMachine;
import spartan.errors.TypeMismatch;
import spartan.errors.NoSuchElement;

public final class Builtins
{
  public static Bool truth(boolean x)
  {
    return x ? Bool.True : Bool.False;
  }
  
  public static boolean truth(Datum x)
  {
    return !(x == Bool.False || x == Nil.Instance);
  }
  
  public static Datum add(Datum x, Datum y) throws TypeMismatch
  {
    if (x.type() == y.type()) {
      switch (x.type()) {
        case Int: return ((Int)x).add((Int)y);
        case Real: return ((Real)x).add((Real)y);
      }
    }
    throw new TypeMismatch();
  }
    
  public static final PrimFun Add = new PrimFun(2, true) {
    public void doApply(VirtualMachine vm) throws TypeMismatch {
      vm.result = add(vm.popArg(), vm.popArg());
      while (vm.args != List.Empty)
        vm.result = add(vm.result, vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum sub(Datum x, Datum y) throws TypeMismatch
  {
    if (x.type() == y.type()) {
      switch (x.type()) {
        case Int: return ((Int)x).sub((Int)y);
        case Real: return ((Real)x).sub((Real)y);
      }
    }
    throw new TypeMismatch();
  }
  
  public static final PrimFun Sub = new PrimFun(2, false) {
    public void doApply(VirtualMachine vm) throws TypeMismatch {
      vm.result = sub(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };

  public static Datum mul(Datum x, Datum y) throws TypeMismatch
  {
    if (x.type() == y.type()) {
      switch (x.type()) {
        case Int: return ((Int)x).mul((Int)y);
        case Real: return ((Real)x).mul((Real)y);
      }
    }
    throw new TypeMismatch();
  }
    
  public static final PrimFun Mul = new PrimFun(2, true) {
    public void doApply(VirtualMachine vm) throws TypeMismatch {
      vm.result = mul(vm.popArg(), vm.popArg());
      while (vm.args != List.Empty)
        vm.result = mul(vm.result, vm.popArg());
      vm.popFrame();
    }
  };

  public static Datum div(Datum x, Datum y) throws TypeMismatch
  {
    if (x.type() == y.type()) {
      switch (x.type()) {
        case Int: return ((Int)x).div((Int)y);
        case Real: return ((Real)x).div((Real)y);
      }
    }
    throw new TypeMismatch();
  }
  
  public static final PrimFun Div = new PrimFun(2, false) {
    public void doApply(VirtualMachine vm) throws TypeMismatch {
      vm.result = div(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };

  public static Datum mod(Datum x, Datum y) throws TypeMismatch
  {
    if (x.type() == Type.Int && y.type() == Type.Int)
      return ((Int)x).mod((Int)y);
    throw new TypeMismatch();
  }
  
  public static final PrimFun Mod = new PrimFun(2, false) {
    public void doApply(VirtualMachine vm) throws TypeMismatch {
      vm.result = mod(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };

  public static Datum neg(Datum x) throws TypeMismatch
  {
    switch (x.type()) {
      case Int: return ((Int)x).neg();
      case Real: return ((Real)x).neg();
    }
    throw new TypeMismatch();
  }
  
  public static final PrimFun Neg = new PrimFun(1, false) {
    public void doApply(VirtualMachine vm) throws TypeMismatch {
      vm.result = neg(vm.popArg());
      vm.popFrame();
    }
  };

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
    if (x.type() != Type.List)
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
    if (x.type() != Type.List)
      throw new TypeMismatch();
    return ((List)x).cdr();
  }
  
  public static final PrimFun Cdr = new PrimFun(1, false) {
    public void doApply(VirtualMachine vm) throws TypeMismatch {
      vm.result = cdr(vm.popArg());
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
}
