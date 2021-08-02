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
    return !(x == Bool.False ||
             x == Nil.Instance);
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
  
  public static final PrimFun Add = new PrimFun(2, false) {
    public Datum apply(VirtualMachine vm) throws TypeMismatch {
      Datum x = vm.popArg();
      Datum y = vm.popArg();
      return add(x, y);
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
    public Datum apply(VirtualMachine vm) throws TypeMismatch {
      Datum x = vm.popArg();
      Datum y = vm.popArg();
      return sub(x, y);
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
    
  public static final PrimFun Mul = new PrimFun(2, false) {
    public Datum apply(VirtualMachine vm) throws TypeMismatch {
      Datum x = vm.popArg();
      Datum y = vm.popArg();
      return mul(x, y);
    }
  };

  public static Datum div(Datum x, Datum y) throws TypeMismatch
  {
    if (x.type() == y.type()) {
      switch (x.type()) {
        case Int: return ((Int)x).div((Int)y);
        case Real: return ((Real)x).div((Real)x);
      }
    }
    throw new TypeMismatch();
  }
  
  public static final PrimFun Div = new PrimFun(2, false) {
    public Datum apply(VirtualMachine vm) throws TypeMismatch {
      Datum x = vm.popArg();
      Datum y = vm.popArg();
      return div(x, y);
    }
  };

  public static Datum mod(Datum x, Datum y) throws TypeMismatch
  {
    if (x.type() == Type.Int && y.type() == Type.Int)
      return ((Int)x).mod((Int)y);
    throw new TypeMismatch();
  }
  
  public static final PrimFun Mod = new PrimFun(2, false) {
    public Datum apply(VirtualMachine vm) throws TypeMismatch {
      Datum x = vm.popArg();
      Datum y = vm.popArg();
      return mod(x, y);
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
    public Datum apply(VirtualMachine vm) throws TypeMismatch {
      Datum x = vm.popArg();
      return neg(x);
    }
  };

  public static Bool not(Datum x) throws TypeMismatch
  {
    if (x.type() == Type.Bool)
      return ((Bool)x).not();
    throw new TypeMismatch();
  }
  
  public static final PrimFun Not = new PrimFun(1, false) {
    public Datum apply(VirtualMachine vm) throws TypeMismatch {
      Datum x = vm.popArg();
      return not(x);
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
    public Datum apply(VirtualMachine vm) throws TypeMismatch {
      Datum x = vm.popArg();
      Datum y = vm.popArg();
      return truth(eq(x, y));
    }
  };

  public static boolean ne(Datum x, Datum y) throws TypeMismatch
  {
    return !eq(x, y);
  }
  
  public static final PrimFun Ne = new PrimFun(2, false) {
    public Datum apply(VirtualMachine vm) throws TypeMismatch {
      Datum x = vm.popArg();
      Datum y = vm.popArg();
      return truth(ne(x, y));
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
    public Datum apply(VirtualMachine vm) throws TypeMismatch {
      Datum x = vm.popArg();
      Datum y = vm.popArg();
      return truth(lt(x, y));
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
    public Datum apply(VirtualMachine vm) throws TypeMismatch {
      Datum x = vm.popArg();
      Datum y = vm.popArg();
      return truth(le(x, y));
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
    public Datum apply(VirtualMachine vm) throws TypeMismatch {
      Datum x = vm.popArg();
      Datum y = vm.popArg();
      return truth(gt(x, y));
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
    public Datum apply(VirtualMachine vm) throws TypeMismatch {
      Datum x = vm.popArg();
      Datum y = vm.popArg();
      return truth(ge(x, y));
    }
  };
  
  public static final PrimFun First = new PrimFun(1, false) {
    public Datum apply(VirtualMachine vm) throws TypeMismatch {
      Datum x = vm.popArg();
      if (x.type() != Type.List)
        throw new TypeMismatch();
      return ((List)x).first;
    }
  };
  
  public static final PrimFun Rest = new PrimFun(1, false) {
    public Datum apply(VirtualMachine vm) throws TypeMismatch {
      Datum x = vm.popArg();
      if (x.type() != Type.List)
        throw new TypeMismatch();
      return ((List)x).rest;
    }
  };
  
  public static final PrimFun MakeVector = new PrimFun(0, true) {
    public Datum apply(VirtualMachine vm) throws TypeMismatch {
      return Vector.fromList(vm.popArgs());
    }
  };
}
