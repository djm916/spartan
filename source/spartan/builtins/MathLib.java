package spartan.builtins;

import spartan.data.*;
import spartan.errors.TypeMismatch;
import spartan.runtime.VirtualMachine;
import spartan.Config;

public final class MathLib
{
  public static INum add(Datum x, Datum y)
  {
    if (x instanceof INum lhs && y instanceof INum rhs)
      return lhs.add(rhs);
    throw new TypeMismatch();
  }
    
  public static INum sub(Datum x, Datum y)
  {
    if (x instanceof INum lhs && y instanceof INum rhs)
      return lhs.sub(rhs);
    throw new TypeMismatch();
  }
  
  public static INum mul(Datum x, Datum y)
  {
    if (x instanceof INum lhs && y instanceof INum rhs)
      return lhs.mul(rhs);
    throw new TypeMismatch();
  }
  
  public static INum div(Datum x, Datum y)
  {
    if (x instanceof INum lhs && y instanceof INum rhs)
      return lhs.div(rhs);
    throw new TypeMismatch();
  }
    
  public static IInt quotient(Datum x, Datum y)
  {
    if (x instanceof IInt lhs && y instanceof IInt rhs)
      return lhs.quotient(rhs);
    throw new TypeMismatch();
  }
  
  public static IInt remainder(Datum x, Datum y)
  {
    if (x instanceof IInt lhs && y instanceof IInt rhs)
      return lhs.remainder(rhs);
    throw new TypeMismatch();    
  }
  
  public static INum neg(Datum x)
  {
    if (x instanceof INum arg)
      return arg.neg();
    throw new TypeMismatch();
  }
  
  public static INum abs(Datum x)
  {
    if (x instanceof INum arg)
      return arg.abs();
    throw new TypeMismatch();
  }
  
  public static IInt floor(Datum x)
  {
    if (x instanceof IReal arg)
      return arg.floor();
    throw new TypeMismatch();
  }
  
  public static IInt ceiling(Datum x)
  {
    if (x instanceof IReal arg)
      return arg.ceiling();
    throw new TypeMismatch();
  }
  
  public static IInt round(Datum x)
  {
    if (x instanceof IReal arg)
      return arg.round();
    throw new TypeMismatch();
  }
  
  public static ITrans exp(Datum x, Datum y)
  {
    if (x instanceof ITrans base && y instanceof ITrans power)
      return base.exp(power);
    throw new TypeMismatch();
  }
  
  public static ITrans log(Datum x, Datum y)
  {
    if (x instanceof ITrans arg && y instanceof ITrans base)
      return arg.log(base);
    throw new TypeMismatch();
  }
  
  public static ITrans sin(Datum x)
  {
    if (x instanceof ITrans arg)
      return arg.sin();
    throw new TypeMismatch();
  }
  
  public static ITrans cos(Datum x)
  {
    if (x instanceof ITrans arg)
      return arg.cos();
    throw new TypeMismatch();
  }
  
  public static ITrans tan(Datum x)
  {
    if (x instanceof ITrans arg)
      return arg.tan();
    throw new TypeMismatch();
  }
  
  public static ITrans asin(Datum x)
  {
    if (x instanceof ITrans arg)
      return arg.asin();
    throw new TypeMismatch();
  }
  
  public static ITrans acos(Datum x)
  {
    if (x instanceof ITrans arg)
      return arg.acos();
    throw new TypeMismatch();
  }
  
  public static ITrans atan(Datum x)
  {
    if (x instanceof ITrans arg)
      return arg.atan();
    throw new TypeMismatch();
  }
  
  public static final IComplex makeComplex(Datum x, Datum y)
  {
    if (x instanceof IReal real && y instanceof IReal imag)
      return new Complex(real, imag);
    throw new TypeMismatch();
  }
  
  public static final IRatio makeRatio(Datum x, Datum y)
  {
    if (x instanceof IInt numer && y instanceof IInt denom)
      return numer.over(denom);
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
  
  public static final Primitive SUB = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = sub(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };
  
  public static final Primitive MUL = new Primitive(2, true) {
    public void apply(VirtualMachine vm) {
      vm.result = mul(vm.popArg(), vm.popArg());
      while (vm.args != List.EMPTY)
        vm.result = mul(vm.result, vm.popArg());
      vm.popFrame();
    }
  };
  
  public static final Primitive DIV = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = div(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };
  
  public static final Primitive QUOTIENT = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = quotient(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };
  
  public static final Primitive REMAINDER = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = remainder(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };
  
  public static final Primitive NEG = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = neg(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static final Primitive ABS = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = abs(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static final Primitive FLOOR = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = floor(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static final Primitive CEILING = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = ceiling(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static final Primitive ROUND = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = round(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static final Primitive EXP = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = exp(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };
  
  public static final Primitive LOG = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = log(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };
  
  public static final Primitive SIN = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = sin(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static final Primitive COS = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = cos(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static final Primitive TAN = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = tan(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static final Primitive ASIN = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = asin(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static final Primitive ACOS = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = acos(vm.popArg());
      vm.popFrame();
    }
  };
  
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
  
  public static final Primitive MAKE_COMPLEX = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = makeComplex(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };
    
  public static final Primitive REAL = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof IComplex c))
        throw new TypeMismatch();
      vm.result = c.real();
      vm.popFrame();
    }
  };
  
  public static final Primitive IMAG = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof IComplex c))
        throw new TypeMismatch();
      vm.result = c.imag();
      vm.popFrame();
    }
  };
  
  public static final Primitive ANGLE = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof IComplex c))
        throw new TypeMismatch();
      vm.result = c.angle();
      vm.popFrame();
    }
  };
  
  public static final Primitive MAGNITUDE = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof IComplex c))
        throw new TypeMismatch();
      vm.result = c.magnitude();
      vm.popFrame();
    }
  };
  
  public static final Primitive MAKE_RATIO = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = makeRatio(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };
  
  public static final Primitive NUMERATOR = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof IRatio q))
        throw new TypeMismatch();
      vm.result = q.numerator();
      vm.popFrame();
    }
  };
  
  public static final Primitive DENOMINATOR = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof IRatio q))
        throw new TypeMismatch();
      vm.result = q.denominator();
      vm.popFrame();
    }
  };
}
