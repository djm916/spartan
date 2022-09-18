package spartan.builtins;

import spartan.data.*;
import spartan.errors.TypeMismatch;
import spartan.runtime.VirtualMachine;
import spartan.Config;

public final class MathLib
{
  public static Datum add(Datum x, Datum y)
  {
    switch (x.type()) {
      case INT: {
        switch (y.type()) {
          case INT:     return ((Int)x).add((Int)y);
          case BIGINT:  return ((Int)x).toBigInt().add((BigInt)y);
          case RATIO:   return ((Int)x).toRatio().add((Ratio)y);
          case REAL:    return ((Int)x).toReal().add((Real)y);
          case COMPLEX: return ((Int)x).toComplex().add((Complex)y);
        }
        break;
      }
      case BIGINT: {
        switch (y.type()) {
          case INT:     return ((BigInt)x).add(((Int)y).toBigInt());
          case BIGINT:  return ((BigInt)x).add((BigInt)y);
          case RATIO:   return ((BigInt)x).toRatio().add((Ratio)y);
          case REAL:    return ((BigInt)x).toReal().add((Real)y);
          case COMPLEX: return ((BigInt)x).toComplex().add((Complex)y);
        }
        break;
      }
      case RATIO: {
        switch (y.type()) {
          case INT:     return ((Ratio)x).add(((Int)y).toRatio());
          case BIGINT:  return ((Ratio)x).add(((BigInt)y).toRatio());
          case RATIO:   return ((Ratio)x).add((Ratio)y);
          case REAL:    return ((Ratio)x).toReal().add((Real)y);
          case COMPLEX: return ((Ratio)x).toComplex().add((Complex)y);
        }
        break;
      }
      case REAL: {
        switch (y.type()) {
          case INT:     return ((Real)x).add(((Int)y).toReal());
          case BIGINT:  return ((Real)x).add(((BigInt)y).toReal());
          case RATIO:   return ((Real)x).add(((Ratio)y).toReal());
          case REAL:    return ((Real)x).add((Real)y);
          case COMPLEX: return ((Real)x).toComplex().add((Complex)y);
        }
        break;
      }
      case COMPLEX: {
        switch (y.type()) {
          case INT:     return ((Complex)x).add(((Int)y).toComplex());
          case BIGINT:  return ((Complex)x).add(((BigInt)y).toComplex());
          case RATIO:   return ((Complex)x).add(((Ratio)y).toComplex());
          case REAL:    return ((Complex)x).add(((Real)y).toComplex());
          case COMPLEX: return ((Complex)x).add((Complex)y);
        }
        break;
      }
    }
    throw new TypeMismatch();
  }
  
  public static final Primitive ADD = new Primitive(2, true) {
    public void apply(VirtualMachine vm) {
      vm.result = add(vm.popArg(), vm.popArg());
      while (vm.args != List.EMPTY)
        vm.result = add(vm.result, vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum sub(Datum x, Datum y)
  {
    if (x.type() != y.type())
      throw new TypeMismatch();
    
    return switch (x.type()) {
      case INT     -> ((Int)x).sub((Int)y);
      case REAL    -> ((Real)x).sub((Real)y);
      case COMPLEX -> ((Complex)x).sub((Complex)y);
      default      -> throw new TypeMismatch();
    };
  }
  
  public static final Primitive SUB = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = sub(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };

  public static Datum mul(Datum x, Datum y)
  {
    if (x.type() != y.type())
      throw new TypeMismatch();
    
    return switch (x.type()) {
      case INT     -> ((Int)x).mul((Int)y);
      case REAL    -> ((Real)x).mul((Real)y);
      case COMPLEX -> ((Complex)x).mul((Complex)y);
      default      -> throw new TypeMismatch();
    };
  }
  
  public static final Primitive MUL = new Primitive(2, true) {
    public void apply(VirtualMachine vm) {
      vm.result = mul(vm.popArg(), vm.popArg());
      while (vm.args != List.EMPTY)
        vm.result = mul(vm.result, vm.popArg());
      vm.popFrame();
    }
  };

  public static Datum div(Datum x, Datum y)
  {
    if (x.type() != y.type())
      throw new TypeMismatch();
    
    return switch (x.type()) {
      case INT     -> ((Int)x).div((Int)y);
      case REAL    -> ((Real)x).div((Real)y);
      case COMPLEX -> ((Complex)x).div((Complex)y);
      default      -> throw new TypeMismatch();
    };
  }
    
  public static final Primitive DIV = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = div(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum quotient(Datum x, Datum y)
  {
    if (x.type() != Type.INT || y.type() != Type.INT)
      throw new TypeMismatch();
    
    return ((Int)x).quotient((Int)y);
  }
  
  public static final Primitive QUOTIENT = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = quotient(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };

  public static Datum remainder(Datum x, Datum y)
  {
    if (x.type() != Type.INT || y.type() != Type.INT)
      throw new TypeMismatch();
    
    return ((Int)x).remainder((Int)y);
  }
  
  public static final Primitive REMAINDER = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = remainder(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum neg(Datum x)
  {
    return switch (x.type()) {
      case INT     -> ((Int)x).neg();
      case REAL    -> ((Real)x).neg();
      case COMPLEX -> ((Complex)x).neg();
      default      -> throw new TypeMismatch();
    };
  }
  
  public static final Primitive NEG = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = neg(vm.popArg());
      vm.popFrame();
    }
  };

  public static Datum abs(Datum x)
  {
    return switch (x.type()) {
      case INT     -> ((Int)x).abs();
      case REAL    -> ((Real)x).abs();
      case COMPLEX -> ((Complex)x).abs();
      default      -> throw new TypeMismatch();
    };
  }
  
  public static final Primitive ABS = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = abs(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum floor(Datum x)
  {
    if (x.type() != Type.REAL)
      throw new TypeMismatch();
    
    return ((Real)x).floor();
  }
  
  public static final Primitive FLOOR = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = floor(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum ceiling(Datum x)
  {
    if (x.type() != Type.REAL)
      throw new TypeMismatch();
    
    return ((Real)x).ceiling();
  }
  
  public static final Primitive CEILING = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = ceiling(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum round(Datum x)
  {
    if (x.type() != Type.REAL)
      throw new TypeMismatch();
    
    return ((Real)x).round();
  }
  
  public static final Primitive ROUND = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = round(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum truncate(Datum x)
  {
    if (x.type() != Type.REAL)
      throw new TypeMismatch();
    
    return ((Real)x).truncate();
  }
  
  public static final Primitive TRUNCATE = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truncate(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum exp(Datum x, Datum y)
  {
    if (x.type() != y.type())
      throw new TypeMismatch();
    
    return switch (x.type()) {
      case REAL    -> ((Real)x).exp((Real)y);
      case COMPLEX -> ((Complex)x).exp((Complex)y);
      default      -> throw new TypeMismatch();
    };
  }
  
  public static final Primitive EXP = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = exp(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum log(Datum x, Datum y)
  {
    if (x.type() != y.type())
      throw new TypeMismatch();
    
    return switch (x.type()) {
      case REAL    -> ((Real)x).log((Real)y);
      case COMPLEX -> ((Complex)x).log((Complex)y);
      default      -> throw new TypeMismatch();
    };
  }
  
  public static final Primitive LOG = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = log(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum sin(Datum x)
  {
    return switch (x.type()) {
      case REAL    -> ((Real)x).sin();
      case COMPLEX -> ((Complex)x).sin();
      default      -> throw new TypeMismatch();
    };
  }
  
  public static final Primitive SIN = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = sin(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum cos(Datum x)
  {
    return switch (x.type()) {
      case REAL    -> ((Real)x).cos();
      case COMPLEX -> ((Complex)x).cos();
      default      -> throw new TypeMismatch();
    };
  }
  
  public static final Primitive COS = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = cos(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum tan(Datum x)
  {
    return switch (x.type()) {
      case REAL    -> ((Real)x).tan();
      case COMPLEX -> ((Complex)x).tan();
      default      -> throw new TypeMismatch();
    };
  }
  
  public static final Primitive TAN = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = tan(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum asin(Datum x)
  {
    return switch (x.type()) {
      case REAL    -> ((Real)x).asin();
      case COMPLEX -> ((Complex)x).asin();
      default      -> throw new TypeMismatch();
    };
  }
  
  public static final Primitive ASIN = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = asin(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum acos(Datum x)
  {
    return switch (x.type()) {
      case REAL    -> ((Real)x).acos();
      case COMPLEX -> ((Complex)x).acos();
      default      -> throw new TypeMismatch();
    };
  }
  
  public static final Primitive ACOS = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = acos(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum atan(Datum x)
  {
    return switch (x.type()) {
      case REAL    -> ((Real)x).atan();
      case COMPLEX -> ((Complex)x).atan();
      default      -> throw new TypeMismatch();
    };
  }
  
  public static final Primitive ATAN = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = atan(vm.popArg());
      vm.popFrame();
    }
  };
    
  public static final Primitive RAND = new Primitive(0, false) {
    public void apply(VirtualMachine vm) {
      vm.result = new Real(Config.DEFAULT_RNG.nextDouble());
      vm.popFrame();
    }
  };
  
  public static final Complex makeComplex(Datum x, Datum y)
  {
    if (x.type() != Type.REAL || y.type() != Type.REAL)
      throw new TypeMismatch();
    return new Complex((Real)x, (Real)y);
  }
  
  public static final Primitive MAKE_COMPLEX = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = makeComplex(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };
    
  public static final Primitive REAL_PART = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.COMPLEX)
        throw new TypeMismatch();
      vm.result = ((Complex)vm.popArg()).real();
      vm.popFrame();
    }
  };
  
  public static final Primitive IMAG_PART = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.COMPLEX)
        throw new TypeMismatch();
      vm.result = ((Complex)vm.popArg()).imag();
      vm.popFrame();
    }
  };
  
  public static final Primitive RECT_TO_POLAR = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.COMPLEX)
        throw new TypeMismatch();
      vm.result = ((Complex)vm.popArg()).toPolar();
      vm.popFrame();
    }
  };
  
  public static final Primitive POLAR_TO_RECT = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.COMPLEX)
        throw new TypeMismatch();
      vm.result = ((Complex)vm.popArg()).toRect();
      vm.popFrame();
    }
  };
}
