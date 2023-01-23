package spartan.builtins;

import spartan.data.*;
import spartan.errors.TypeMismatch;
import spartan.runtime.VirtualMachine;
import spartan.Config;
import static spartan.builtins.Math.*;

public final class MathLib
{
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
    
  public static final Primitive REAL_PART = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof INum c))
        throw new TypeMismatch();
      vm.result = c.realPart();
      vm.popFrame();
    }
  };
  
  public static final Primitive IMAG_PART = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof INum c))
        throw new TypeMismatch();
      vm.result = c.imagPart();
      vm.popFrame();
    }
  };
  
  public static final Primitive ANGLE = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof INum c))
        throw new TypeMismatch();
      vm.result = c.angle();
      vm.popFrame();
    }
  };
  
  public static final Primitive MAGNITUDE = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof INum c))
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
