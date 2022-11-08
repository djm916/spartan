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
    switch (x.type()) {
      case INT: {
        switch (y.type()) {
          case INT:     return ((Int)x).sub((Int)y);
          case BIGINT:  return ((Int)x).toBigInt().sub((BigInt)y);
          case RATIO:   return ((Int)x).toRatio().sub((Ratio)y);
          case REAL:    return ((Int)x).toReal().sub((Real)y);
          case COMPLEX: return ((Int)x).toComplex().sub((Complex)y);
        }
        break;
      }
      case BIGINT: {
        switch (y.type()) {
          case INT:     return ((BigInt)x).sub(((Int)y).toBigInt());
          case BIGINT:  return ((BigInt)x).sub((BigInt)y);
          case RATIO:   return ((BigInt)x).toRatio().sub((Ratio)y);
          case REAL:    return ((BigInt)x).toReal().sub((Real)y);
          case COMPLEX: return ((BigInt)x).toComplex().sub((Complex)y);
        }
        break;
      }
      case RATIO: {
        switch (y.type()) {
          case INT:     return ((Ratio)x).sub(((Int)y).toRatio());
          case BIGINT:  return ((Ratio)x).sub(((BigInt)y).toRatio());
          case RATIO:   return ((Ratio)x).sub((Ratio)y);
          case REAL:    return ((Ratio)x).toReal().sub((Real)y);
          case COMPLEX: return ((Ratio)x).toComplex().sub((Complex)y);
        }
        break;
      }
      case REAL: {
        switch (y.type()) {
          case INT:     return ((Real)x).sub(((Int)y).toReal());
          case BIGINT:  return ((Real)x).sub(((BigInt)y).toReal());
          case RATIO:   return ((Real)x).sub(((Ratio)y).toReal());
          case REAL:    return ((Real)x).sub((Real)y);
          case COMPLEX: return ((Real)x).toComplex().sub((Complex)y);
        }
        break;
      }
      case COMPLEX: {
        switch (y.type()) {
          case INT:     return ((Complex)x).sub(((Int)y).toComplex());
          case BIGINT:  return ((Complex)x).sub(((BigInt)y).toComplex());
          case RATIO:   return ((Complex)x).sub(((Ratio)y).toComplex());
          case REAL:    return ((Complex)x).sub(((Real)y).toComplex());
          case COMPLEX: return ((Complex)x).sub((Complex)y);
        }
        break;
      }
    }
    throw new TypeMismatch();
  }
  
  public static final Primitive SUB = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = sub(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };

  public static Datum mul(Datum x, Datum y)
  {
    switch (x.type()) {
      case INT: {
        switch (y.type()) {
          case INT:     return ((Int)x).mul((Int)y);
          case BIGINT:  return ((Int)x).toBigInt().mul((BigInt)y);
          case RATIO:   return ((Int)x).toRatio().mul((Ratio)y);
          case REAL:    return ((Int)x).toReal().mul((Real)y);
          case COMPLEX: return ((Int)x).toComplex().mul((Complex)y);
        }
        break;
      }
      case BIGINT: {
        switch (y.type()) {
          case INT:     return ((BigInt)x).mul(((Int)y).toBigInt());
          case BIGINT:  return ((BigInt)x).mul((BigInt)y);
          case RATIO:   return ((BigInt)x).toRatio().mul((Ratio)y);
          case REAL:    return ((BigInt)x).toReal().mul((Real)y);
          case COMPLEX: return ((BigInt)x).toComplex().mul((Complex)y);
        }
        break;
      }
      case RATIO: {
        switch (y.type()) {
          case INT:     return ((Ratio)x).mul(((Int)y).toRatio());
          case BIGINT:  return ((Ratio)x).mul(((BigInt)y).toRatio());
          case RATIO:   return ((Ratio)x).mul((Ratio)y);
          case REAL:    return ((Ratio)x).toReal().mul((Real)y);
          case COMPLEX: return ((Ratio)x).toComplex().mul((Complex)y);
        }
        break;
      }
      case REAL: {
        switch (y.type()) {
          case INT:     return ((Real)x).mul(((Int)y).toReal());
          case BIGINT:  return ((Real)x).mul(((BigInt)y).toReal());
          case RATIO:   return ((Real)x).mul(((Ratio)y).toReal());
          case REAL:    return ((Real)x).mul((Real)y);
          case COMPLEX: return ((Real)x).toComplex().mul((Complex)y);
        }
        break;
      }
      case COMPLEX: {
        switch (y.type()) {
          case INT:     return ((Complex)x).mul(((Int)y).toComplex());
          case BIGINT:  return ((Complex)x).mul(((BigInt)y).toComplex());
          case RATIO:   return ((Complex)x).mul(((Ratio)y).toComplex());
          case REAL:    return ((Complex)x).mul(((Real)y).toComplex());
          case COMPLEX: return ((Complex)x).mul((Complex)y);
        }
        break;
      }
    }
    throw new TypeMismatch();
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
    switch (x.type()) {
      case INT: {
        switch (y.type()) {
          case INT:     return ((Int)x).div((Int)y);
          case BIGINT:  return ((Int)x).toBigInt().div((BigInt)y);
          case RATIO:   return ((Int)x).toRatio().div((Ratio)y);
          case REAL:    return ((Int)x).toReal().div((Real)y);
          case COMPLEX: return ((Int)x).toComplex().div((Complex)y);
        }
        break;
      }
      case BIGINT: {
        switch (y.type()) {
          case INT:     return ((BigInt)x).div(((Int)y).toBigInt());
          case BIGINT:  return ((BigInt)x).div((BigInt)y);
          case RATIO:   return ((BigInt)x).toRatio().div((Ratio)y);
          case REAL:    return ((BigInt)x).toReal().div((Real)y);
          case COMPLEX: return ((BigInt)x).toComplex().div((Complex)y);
        }
        break;
      }
      case RATIO: {
        switch (y.type()) {
          case INT:     return ((Ratio)x).div(((Int)y).toRatio());
          case BIGINT:  return ((Ratio)x).div(((BigInt)y).toRatio());
          case RATIO:   return ((Ratio)x).div((Ratio)y);
          case REAL:    return ((Ratio)x).toReal().div((Real)y);
          case COMPLEX: return ((Ratio)x).toComplex().div((Complex)y);
        }
        break;
      }
      case REAL: {
        switch (y.type()) {
          case INT:     return ((Real)x).div(((Int)y).toReal());
          case BIGINT:  return ((Real)x).div(((BigInt)y).toReal());
          case RATIO:   return ((Real)x).div(((Ratio)y).toReal());
          case REAL:    return ((Real)x).div((Real)y);
          case COMPLEX: return ((Real)x).toComplex().div((Complex)y);
        }
        break;
      }
      case COMPLEX: {
        switch (y.type()) {
          case INT:     return ((Complex)x).div(((Int)y).toComplex());
          case BIGINT:  return ((Complex)x).div(((BigInt)y).toComplex());
          case RATIO:   return ((Complex)x).div(((Ratio)y).toComplex());
          case REAL:    return ((Complex)x).div(((Real)y).toComplex());
          case COMPLEX: return ((Complex)x).div((Complex)y);
        }
        break;
      }
    }
    throw new TypeMismatch();
  }
    
  public static final Primitive DIV = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = div(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum quotient(Datum x, Datum y)
  {
    switch (x.type()) {
      case INT: {
        switch (y.type()) {
          case INT:     return ((Int)x).quotient((Int)y);
          case BIGINT:  return ((Int)x).toBigInt().quotient((BigInt)y);
        }
        break;
      }
      case BIGINT: {
        switch (y.type()) {
          case INT:     return ((BigInt)x).quotient(((Int)y).toBigInt());
          case BIGINT:  return ((BigInt)x).quotient((BigInt)y);
        }
        break;
      }
    }
    throw new TypeMismatch();    
  }
  
  public static final Primitive QUOTIENT = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = quotient(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };

  public static Datum remainder(Datum x, Datum y)
  {
    switch (x.type()) {
      case INT: {
        switch (y.type()) {
          case INT:     return ((Int)x).remainder((Int)y);
          case BIGINT:  return ((Int)x).toBigInt().remainder((BigInt)y);
        }
        break;
      }
      case BIGINT: {
        switch (y.type()) {
          case INT:     return ((BigInt)x).remainder(((Int)y).toBigInt());
          case BIGINT:  return ((BigInt)x).remainder((BigInt)y);
        }
        break;
      }
    }
    throw new TypeMismatch();    
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
      case BIGINT  -> ((BigInt)x).neg();
      case RATIO   -> ((Ratio)x).neg();
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
      case BIGINT  -> ((BigInt)x).abs();
      case RATIO   -> ((Ratio)x).abs();
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
    return switch (x.type()) {
      case RATIO   -> ((Ratio)x).floor();
      case REAL    -> ((Real)x).floor();
      default      -> throw new TypeMismatch();
    };
  }
  
  public static final Primitive FLOOR = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = floor(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum ceiling(Datum x)
  {
    return switch (x.type()) {
      case RATIO   -> ((Ratio)x).ceiling();
      case REAL    -> ((Real)x).ceiling();
      default      -> throw new TypeMismatch();
    };
  }
  
  public static final Primitive CEILING = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = ceiling(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum round(Datum x)
  {
    return switch (x.type()) {
      case RATIO   -> ((Ratio)x).round();
      case REAL    -> ((Real)x).round();
      default      -> throw new TypeMismatch();
    };
  }
  
  public static final Primitive ROUND = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = round(vm.popArg());
      vm.popFrame();
    }
  };
    
  public static Datum exp(Datum x, Datum y)
  {
    switch (x.type()) {
      case INT: {
        switch (y.type()) {
          case INT:     return ((Int)x).toReal().exp(((Int)y).toReal());
          case BIGINT:  return ((Int)x).toReal().exp(((BigInt)y).toReal());
          case RATIO:   return ((Int)x).toReal().exp(((Ratio)y).toReal());
          case REAL:    return ((Int)x).toReal().exp((Real)y);
          case COMPLEX: return ((Int)x).toComplex().add((Complex)y);
        }
        break;
      }
      case BIGINT: {
        switch (y.type()) {
          case INT:     return ((BigInt)x).toReal().exp(((Int)y).toReal());
          case BIGINT:  return ((BigInt)x).toReal().exp(((BigInt)y).toReal());
          case RATIO:   return ((BigInt)x).toReal().exp(((Ratio)y).toReal());
          case REAL:    return ((BigInt)x).toReal().exp((Real)y);
          case COMPLEX: return ((BigInt)x).toComplex().add((Complex)y);
        }
        break;
      }
      case RATIO: {
        switch (y.type()) {
          case INT:     return ((Ratio)x).toReal().exp(((Int)y).toReal());
          case BIGINT:  return ((Ratio)x).toReal().exp(((BigInt)y).toReal());
          case RATIO:   return ((Ratio)x).toReal().exp(((Ratio)y).toReal());
          case REAL:    return ((Ratio)x).toReal().exp((Real)y);
          case COMPLEX: return ((Ratio)x).toComplex().exp((Complex)y);
        }
        break;
      }
      case REAL: {
        switch (y.type()) {
          case INT:     return ((Real)x).exp(((Int)y).toReal());
          case BIGINT:  return ((Real)x).exp(((BigInt)y).toReal());
          case RATIO:   return ((Real)x).exp(((Ratio)y).toReal());
          case REAL:    return ((Real)x).exp((Real)y);
          case COMPLEX: return ((Real)x).toComplex().exp((Complex)y);
        }
        break;
      }
      case COMPLEX: {
        switch (y.type()) {
          case INT:     return ((Complex)x).exp(((Int)y).toComplex());
          case BIGINT:  return ((Complex)x).exp(((BigInt)y).toComplex());
          case RATIO:   return ((Complex)x).exp(((Ratio)y).toComplex());
          case REAL:    return ((Complex)x).exp(((Real)y).toComplex());
          case COMPLEX: return ((Complex)x).exp((Complex)y);
        }
        break;
      }
    }
    throw new TypeMismatch();
  }
  
  public static final Primitive EXP = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = exp(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum log(Datum x, Datum y)
  {
    switch (x.type()) {
      case INT: {
        switch (y.type()) {
          case INT:     return ((Int)x).toReal().log(((Int)y).toReal());
          case BIGINT:  return ((Int)x).toReal().log(((BigInt)y).toReal());
          case RATIO:   return ((Int)x).toReal().log(((Ratio)y).toReal());
          case REAL:    return ((Int)x).toReal().log((Real)y);
          case COMPLEX: return ((Int)x).toComplex().add((Complex)y);
        }
        break;
      }
      case BIGINT: {
        switch (y.type()) {
          case INT:     return ((BigInt)x).toReal().log(((Int)y).toReal());
          case BIGINT:  return ((BigInt)x).toReal().log(((BigInt)y).toReal());
          case RATIO:   return ((BigInt)x).toReal().log(((Ratio)y).toReal());
          case REAL:    return ((BigInt)x).toReal().log((Real)y);
          case COMPLEX: return ((BigInt)x).toComplex().add((Complex)y);
        }
        break;
      }
      case RATIO: {
        switch (y.type()) {
          case INT:     return ((Ratio)x).toReal().log(((Int)y).toReal());
          case BIGINT:  return ((Ratio)x).toReal().log(((BigInt)y).toReal());
          case RATIO:   return ((Ratio)x).toReal().log(((Ratio)y).toReal());
          case REAL:    return ((Ratio)x).toReal().log((Real)y);
          case COMPLEX: return ((Ratio)x).toComplex().log((Complex)y);
        }
        break;
      }
      case REAL: {
        switch (y.type()) {
          case INT:     return ((Real)x).log(((Int)y).toReal());
          case BIGINT:  return ((Real)x).log(((BigInt)y).toReal());
          case RATIO:   return ((Real)x).log(((Ratio)y).toReal());
          case REAL:    return ((Real)x).log((Real)y);
          case COMPLEX: return ((Real)x).toComplex().log((Complex)y);
        }
        break;
      }
      case COMPLEX: {
        switch (y.type()) {
          case INT:     return ((Complex)x).log(((Int)y).toComplex());
          case BIGINT:  return ((Complex)x).log(((BigInt)y).toComplex());
          case RATIO:   return ((Complex)x).log(((Ratio)y).toComplex());
          case REAL:    return ((Complex)x).log(((Real)y).toComplex());
          case COMPLEX: return ((Complex)x).log((Complex)y);
        }
        break;
      }
    }
    throw new TypeMismatch();
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
      case INT     -> ((Int)x).toReal().sin();
      case BIGINT  -> ((BigInt)x).toReal().sin();
      case RATIO   -> ((Ratio)x).toReal().sin();
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
      case INT     -> ((Int)x).toReal().cos();
      case BIGINT  -> ((BigInt)x).toReal().cos();
      case RATIO   -> ((Ratio)x).toReal().cos();
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
      case INT     -> ((Int)x).toReal().tan();
      case BIGINT  -> ((BigInt)x).toReal().tan();
      case RATIO   -> ((Ratio)x).toReal().tan();
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
      case INT     -> ((Int)x).toReal().asin();
      case BIGINT  -> ((BigInt)x).toReal().asin();
      case RATIO   -> ((Ratio)x).toReal().asin();
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
      case INT     -> ((Int)x).toReal().acos();
      case BIGINT  -> ((BigInt)x).toReal().acos();
      case RATIO   -> ((Ratio)x).toReal().acos();
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
      case INT     -> ((Int)x).toReal().atan();
      case BIGINT  -> ((BigInt)x).toReal().atan();
      case RATIO   -> ((Ratio)x).toReal().atan();
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
    switch (x.type()) {
      case INT: {
        switch (y.type()) {
          case INT:     return new Complex(((Int)x).toReal(), ((Int)y).toReal());
          case BIGINT:  return new Complex(((Int)x).toReal(), ((BigInt)y).toReal());
          case RATIO:   return new Complex(((Int)x).toReal(), ((Ratio)y).toReal());
          case REAL:    return new Complex(((Int)x).toReal(), (Real)y);
        }
        break;
      }
      case BIGINT: {
        switch (y.type()) {
          case INT:     return new Complex(((BigInt)x).toReal(), ((Int)y).toReal());
          case BIGINT:  return new Complex(((BigInt)x).toReal(), ((BigInt)y).toReal());
          case RATIO:   return new Complex(((BigInt)x).toReal(), ((Ratio)y).toReal());
          case REAL:    return new Complex(((BigInt)x).toReal(), (Real)y);
        }
        break;
      }
      case RATIO: {
        switch (y.type()) {
          case INT:     return new Complex(((Ratio)x).toReal(), ((Int)y).toReal());
          case BIGINT:  return new Complex(((Ratio)x).toReal(), ((BigInt)y).toReal());
          case RATIO:   return new Complex(((Ratio)x).toReal(), ((Ratio)y).toReal());
          case REAL:    return new Complex(((Ratio)x).toReal(), (Real)y);
        }
        break;
      }
      case REAL: {
        switch (y.type()) {
          case INT:     return new Complex((Real)x, ((Int)y).toReal());
          case BIGINT:  return new Complex((Real)x, ((BigInt)y).toReal());
          case RATIO:   return new Complex((Real)x, ((Ratio)y).toReal());
          case REAL:    return new Complex((Real)x, (Real)y);
        }
        break;
      }
    }
    throw new TypeMismatch();
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
  
  public static final Ratio makeRatio(Datum x, Datum y)
  {
    switch (x.type()) {
      case INT: {
        switch (y.type()) {
          case INT:     return new Ratio(((Int)x).toBigInt(), ((Int)y).toBigInt());
          case BIGINT:  return new Ratio(((Int)x).toBigInt(), (BigInt)y);
        }
        break;
      }
      case BIGINT: {
        switch (y.type()) {
          case INT:     return new Ratio((BigInt)x, ((Int)y).toBigInt());
          case BIGINT:  return new Ratio((BigInt)x, (BigInt)y);
        }
        break;
      }
    }
    throw new TypeMismatch();
  }
  
  public static final Primitive MAKE_RATIO = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = makeRatio(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };
  
  public static final Primitive NUMERATOR = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.RATIO)
        throw new TypeMismatch();
      vm.result = ((Ratio)vm.popArg()).numerator();
      vm.popFrame();
    }
  };
  
  public static final Primitive DENOMINATOR = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.RATIO)
        throw new TypeMismatch();
      vm.result = ((Ratio)vm.popArg()).denominator();
      vm.popFrame();
    }
  };
  
  public static final Primitive INC = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!vm.peekArg().type().isInt())
        throw new TypeMismatch();
      vm.result = add(vm.popArg(), new Int(1));
      vm.popFrame();
    }
  };
  
  public static final Primitive DEC = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!vm.peekArg().type().isInt())
        throw new TypeMismatch();
      vm.result = add(vm.popArg(), new Int(-1));
      vm.popFrame();
    }
  };
}
