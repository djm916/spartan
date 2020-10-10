package spartan.builtins;

import spartan.data.*;
import spartan.errors.Error;
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
        case Int: return Int.add((Int)x, (Int)y);
        case Real: return Real.add((Real)x, (Real)y);
      }
    }
    throw new TypeMismatch();
  }
    
  public static final PrimFun Add = new PrimFun() {
    public Value apply(final Value x) {
      return new PrimFun() {
        public Value apply(final Value y) throws TypeMismatch {
          return add(x, y);
        }
      };
    }
  };

  public static Value sub(Value x, Value y) throws TypeMismatch
  {
    if (x.type() == y.type()) {
      switch (x.type()) {
        case Int: return Int.sub((Int)x, (Int)y);
        case Real: return Real.sub((Real)x, (Real)y);
      }
    }
    throw new TypeMismatch();
  }
  
  public static final PrimFun Sub = new PrimFun() {
    public Value apply(final Value x) {
      return new PrimFun() {
        public Value apply(final Value y) throws TypeMismatch {
          return sub(y, x);
        }
      };
    }
  };

  public static Value mul(Value x, Value y) throws TypeMismatch
  {
    if (x.type() == y.type()) {
      switch (x.type()) {
        case Int: return Int.mul((Int)x, (Int)y);
        case Real: return Real.mul((Real)x, (Real)y);
      }
    }
    throw new TypeMismatch();
  }
    
  public static final PrimFun Mul = new PrimFun() {
    public Value apply(final Value x) {
      return new PrimFun() {
        public Value apply(final Value y) throws TypeMismatch {
          return mul(x, y);
        }
      };
    }
  };

  public static Value div(Value x, Value y) throws TypeMismatch
  {
    if (x.type() == y.type()) {
      switch (x.type()) {
        case Int: return Int.div((Int)x, (Int)y);
        case Real: return Real.div((Real)x, (Real)y);
      }
    }
    throw new TypeMismatch();
  }
  
  public static final PrimFun Div = new PrimFun() {
    public Value apply(final Value x) {
      return new PrimFun() {
        public Value apply(final Value y) throws TypeMismatch {
          return div(y, x);
        }
      };
    }
  };

  public static final PrimFun Mod = new PrimFun() {
    public Value apply(final Value x) {
      return new PrimFun() {
        public Value apply(final Value y) throws TypeMismatch {
          return null;
        }
      };
    }
  };

  public static final PrimFun Neg = new PrimFun() {
    public Value apply(final Value x) {
      return new PrimFun() {
        public Value apply(final Value y) throws TypeMismatch {
          return null;
        }
      };
    }
  };

  public static final PrimFun Not = new PrimFun() {
    public Value apply(final Value x) {
      return new PrimFun() {
        public Value apply(final Value y) throws TypeMismatch {
          return null;
        }
      };
    }
  };

  public static final PrimFun And = new PrimFun() {
    public Value apply(final Value x) {
      return new PrimFun() {
        public Value apply(final Value y) throws TypeMismatch {
          return null;
        }
      };
    }
  };

  public static final PrimFun Or = new PrimFun() {
    public Value apply(final Value x) {
      return new PrimFun() {
        public Value apply(final Value y) throws TypeMismatch {
          return null;
        }
      };
    }
  };

  public static boolean eq(Value x, Value y) throws TypeMismatch
  {
    if (x.type() == y.type()) {
      switch (x.type()) {
        case Int: return Int.eq((Int)x, (Int)y);
        case Real: return Real.eq((Real)x, (Real)y);
        case Tuple: return Tuple.eq((Tuple)x, (Tuple)y);
        case List: return List.eq((List)x, (List)y);
        case Record: return Record.eq((Record)x, (Record)y);
        case Bool:
        case Unit: return x == y;
      }
    }
    throw new TypeMismatch();
  }
  
  public static final PrimFun Eq = new PrimFun() {
    public Value apply(final Value x) {
      return new PrimFun() {
        public Value apply(final Value y) throws TypeMismatch {
          return truth(eq(x, y));
        }
      };
    }
  };

  public static final PrimFun Ne = new PrimFun() {
    public Value apply(final Value x) {
      return new PrimFun() {
        public Value apply(final Value y) throws TypeMismatch {
          return null;
        }
      };
    }
  };

  public static final PrimFun Lt = new PrimFun() {
    public Value apply(final Value x) {
      return new PrimFun() {
        public Value apply(final Value y) throws TypeMismatch {
          return null;
        }
      };
    }
  };

  public static final PrimFun Le = new PrimFun() {
    public Value apply(final Value x) {
      return new PrimFun() {
        public Value apply(final Value y) throws TypeMismatch {
          return null;
        }
      };
    }
  };

  public static final PrimFun Gt = new PrimFun() {
    public Value apply(final Value x) {
      return new PrimFun() {
        public Value apply(final Value y) throws TypeMismatch {
          return null;
        }
      };
    }
  };

  public static final PrimFun Ge = new PrimFun() {
    public Value apply(final Value x) {
      return new PrimFun() {
        public Value apply(final Value y) throws TypeMismatch {
          return null;
        }
      };
    }
  };
}
