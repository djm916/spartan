package spartan.builtins;

import spartan.data.*;
import spartan.errors.Error;
import spartan.runtime.VirtualMachine;
import spartan.errors.TypeMismatch;
import spartan.errors.NoSuchElement;

public final class Builtins
{
  static Bool truth(boolean x)
  {
    return x ? Bool.True : Bool.False;
  }
  
  
  public static Value add(Value x, Value y) throws TypeMismatch
  {
    if (x.type() == y.type()) {
      switch (x.type()) {
        case Int: return ((Int)x).add((Int)y);
        case Real: return ((Real)x).add((Real)y);
      }
    }
    throw new TypeMismatch();
  }
  
  public static final PrimFun Add = new PrimFun(2) {
    public Value apply(VirtualMachine vm) throws TypeMismatch {
      Value x = vm.args.pop();
      Value y = vm.args.pop();
      return add(x, y);
    }
  };

  public static Value sub(Value x, Value y) throws TypeMismatch
  {
    if (x.type() == y.type()) {
      switch (x.type()) {
        case Int: return ((Int)x).sub((Int)y);
        case Real: return ((Real)x).sub((Real)y);
      }
    }
    throw new TypeMismatch();
  }
  
  public static final PrimFun Sub = new PrimFun(2) {
    public Value apply(VirtualMachine vm) throws TypeMismatch {
      Value x = vm.args.pop();
      Value y = vm.args.pop();
      return sub(x, y);
    }
  };

  public static Value mul(Value x, Value y) throws TypeMismatch
  {
    if (x.type() == y.type()) {
      switch (x.type()) {
        case Int: return ((Int)x).mul((Int)y);
        case Real: return ((Real)x).mul((Real)y);
      }
    }
    throw new TypeMismatch();
  }
    
  public static final PrimFun Mul = new PrimFun(2) {
    public Value apply(VirtualMachine vm) throws TypeMismatch {
      Value x = vm.args.pop();
      Value y = vm.args.pop();
      return mul(x, y);
    }
  };

  public static Value div(Value x, Value y) throws TypeMismatch
  {
    if (x.type() == y.type()) {
      switch (x.type()) {
        case Int: return ((Int)x).div((Int)y);
        case Real: return ((Real)x).div((Real)x);
      }
    }
    throw new TypeMismatch();
  }
  
  public static final PrimFun Div = new PrimFun(2) {
    public Value apply(VirtualMachine vm) throws TypeMismatch {
      Value x = vm.args.pop();
      Value y = vm.args.pop();
      return div(x, y);
    }
  };

  public static Value mod(Value x, Value y) throws TypeMismatch
  {
    if (x.type() == Type.Int && y.type() == Type.Int)
      return ((Int)x).mod((Int)y);
    throw new TypeMismatch();
  }
  
  public static final PrimFun Mod = new PrimFun(2) {
    public Value apply(VirtualMachine vm) throws TypeMismatch {
      Value x = vm.args.pop();
      Value y = vm.args.pop();
      return mod(x, y);
    }
  };

  public static Value neg(Value x) throws TypeMismatch
  {
    switch (x.type()) {
      case Int: return ((Int)x).neg();
      case Real: return ((Real)x).neg();
    }
    throw new TypeMismatch();
  }
  
  public static final PrimFun Neg = new PrimFun(1) {
    public Value apply(VirtualMachine vm) throws TypeMismatch {
      Value x = vm.args.pop();
      return neg(x);
    }
  };

  public static Bool not(Value x) throws TypeMismatch
  {
    if (x.type() == Type.Bool)
      return ((Bool)x).not();
    throw new TypeMismatch();
  }
  
  public static final PrimFun Not = new PrimFun(1) {
    public Value apply(VirtualMachine vm) throws TypeMismatch {
      Value x = vm.args.pop();
      return not(x);
    }
  };

  public static boolean eq(Value x, Value y) throws TypeMismatch
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
  
  public static final PrimFun Eq = new PrimFun(2) {
    public Value apply(VirtualMachine vm) throws TypeMismatch {
      Value x = vm.args.pop();
      Value y = vm.args.pop();
      return truth(eq(x, y));
    }
  };

  public static boolean ne(Value x, Value y) throws TypeMismatch
  {
    return !eq(x, y);
  }
  
  public static final PrimFun Ne = new PrimFun(2) {
    public Value apply(VirtualMachine vm) throws TypeMismatch {
      Value x = vm.args.pop();
      Value y = vm.args.pop();
      return truth(ne(x, y));
    }
  };

  public static boolean lt(Value x, Value y) throws TypeMismatch
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
  
  public static final PrimFun Lt = new PrimFun(2) {
    public Value apply(VirtualMachine vm) throws TypeMismatch {
      Value x = vm.args.pop();
      Value y = vm.args.pop();
      return truth(lt(x, y));
    }
  };

  public static boolean le(Value x, Value y) throws TypeMismatch
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
  
  public static final PrimFun Le = new PrimFun(2) {
    public Value apply(VirtualMachine vm) throws TypeMismatch {
      Value x = vm.args.pop();
      Value y = vm.args.pop();
      return truth(le(x, y));
    }
  };

  public static boolean gt(Value x, Value y) throws TypeMismatch
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
  
  public static final PrimFun Gt = new PrimFun(2) {
    public Value apply(VirtualMachine vm) throws TypeMismatch {
      Value x = vm.args.pop();
      Value y = vm.args.pop();
      return truth(gt(x, y));
    }
  };

  public static boolean ge(Value x, Value y) throws TypeMismatch
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
  
  public static final PrimFun Ge = new PrimFun(2) {
    public Value apply(VirtualMachine vm) throws TypeMismatch {
      Value x = vm.args.pop();
      Value y = vm.args.pop();
      return truth(ge(x, y));
    }
  };
}
