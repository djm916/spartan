package spartan.builtins;

import spartan.data.*;
import spartan.errors.TypeMismatch;
import spartan.runtime.VirtualMachine;

public final class MathLib
{
  public static Datum add(Datum x, Datum y)
  {
    if (x.type() != y.type())
      throw new TypeMismatch();
    
    return switch (x.type()) {
      case Int     -> ((Int)x).add((Int)y);
      case Real    -> ((Real)x).add((Real)y);
      case Complex -> ((Complex)x).add((Complex)y);
      default      -> throw new TypeMismatch();
    };
  }
  
  public static final Primitive Add = new Primitive(2, true) {
    public void apply(VirtualMachine vm) {
      vm.result = add(vm.popArg(), vm.popArg());
      while (vm.args != List.Empty)
        vm.result = add(vm.result, vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum sub(Datum x, Datum y)
  {
    if (x.type() != y.type())
      throw new TypeMismatch();
    
    return switch (x.type()) {
      case Int     -> ((Int)x).sub((Int)y);
      case Real    -> ((Real)x).sub((Real)y);
      case Complex -> ((Complex)x).sub((Complex)y);
      default      -> throw new TypeMismatch();
    };
  }
  
  public static final Primitive Sub = new Primitive(2, false) {
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
      case Int     -> ((Int)x).mul((Int)y);
      case Real    -> ((Real)x).mul((Real)y);
      case Complex -> ((Complex)x).mul((Complex)y);
      default      -> throw new TypeMismatch();
    };
  }
  
  public static final Primitive Mul = new Primitive(2, true) {
    public void apply(VirtualMachine vm) {
      vm.result = mul(vm.popArg(), vm.popArg());
      while (vm.args != List.Empty)
        vm.result = mul(vm.result, vm.popArg());
      vm.popFrame();
    }
  };

  public static Datum div(Datum x, Datum y)
  {
    if (x.type() != y.type())
      throw new TypeMismatch();
    
    return switch (x.type()) {
      case Int     -> ((Int)x).div((Int)y);
      case Real    -> ((Real)x).div((Real)y);
      case Complex -> ((Complex)x).div((Complex)y);
      default      -> throw new TypeMismatch();
    };
  }
    
  public static final Primitive Div = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = div(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum quotient(Datum x, Datum y)
  {
    if (x.type() != Type.Int || y.type() != Type.Int)
      throw new TypeMismatch();
    
    return ((Int)x).quotient((Int)y);
  }
  
  public static final Primitive Quotient = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = quotient(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };

  public static Datum remainder(Datum x, Datum y)
  {
    if (x.type() != Type.Int || y.type() != Type.Int)
      throw new TypeMismatch();
    
    return ((Int)x).remainder((Int)y);
  }
  
  public static final Primitive Remainder = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = remainder(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum neg(Datum x)
  {
    return switch (x.type()) {
      case Int     -> ((Int)x).neg();
      case Real    -> ((Real)x).neg();
      case Complex -> ((Complex)x).neg();
      default      -> throw new TypeMismatch();
    };
  }
  
  public static final Primitive Neg = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = neg(vm.popArg());
      vm.popFrame();
    }
  };

  public static Datum abs(Datum x)
  {
    return switch (x.type()) {
      case Int     -> ((Int)x).abs();
      case Real    -> ((Real)x).abs();
      case Complex -> ((Complex)x).abs();
      default      -> throw new TypeMismatch();
    };
  }
  
  public static final Primitive Abs = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = abs(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum floor(Datum x)
  {
    if (x.type() != Type.Real)
      throw new TypeMismatch();
    
    return ((Real)x).floor();
  }
  
  public static final Primitive Floor = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = floor(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum ceiling(Datum x)
  {
    if (x.type() != Type.Real)
      throw new TypeMismatch();
    
    return ((Real)x).ceiling();
  }
  
  public static final Primitive Ceiling = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = ceiling(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum round(Datum x)
  {
    if (x.type() != Type.Real)
      throw new TypeMismatch();
    
    return ((Real)x).round();
  }
  
  public static final Primitive Round = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = round(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum truncate(Datum x)
  {
    if (x.type() != Type.Real)
      throw new TypeMismatch();
    
    return ((Real)x).truncate();
  }
  
  public static final Primitive Truncate = new Primitive(1, false) {
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
      case Real    -> ((Real)x).exp((Real)y);
      case Complex -> ((Complex)x).exp((Complex)y);
      default      -> throw new TypeMismatch();
    };
  }
  
  public static final Primitive Exp = new Primitive(2, false) {
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
      case Real    -> ((Real)x).log((Real)y);
      case Complex -> ((Complex)x).log((Complex)y);
      default      -> throw new TypeMismatch();
    };
  }
  
  public static final Primitive Log = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = log(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum sin(Datum x)
  {
    return switch (x.type()) {
      case Real    -> ((Real)x).sin();
      case Complex -> ((Complex)x).sin();
      default      -> throw new TypeMismatch();
    };
  }
  
  public static final Primitive Sin = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = sin(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum cos(Datum x)
  {
    return switch (x.type()) {
      case Real    -> ((Real)x).cos();
      case Complex -> ((Complex)x).cos();
      default      -> throw new TypeMismatch();
    };
  }
  
  public static final Primitive Cos = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = cos(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum tan(Datum x)
  {
    return switch (x.type()) {
      case Real    -> ((Real)x).tan();
      case Complex -> ((Complex)x).tan();
      default      -> throw new TypeMismatch();
    };
  }
  
  public static final Primitive Tan = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = tan(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum asin(Datum x)
  {
    return switch (x.type()) {
      case Real    -> ((Real)x).asin();
      case Complex -> ((Complex)x).asin();
      default      -> throw new TypeMismatch();
    };
  }
  
  public static final Primitive Asin = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = asin(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum acos(Datum x)
  {
    return switch (x.type()) {
      case Real    -> ((Real)x).acos();
      case Complex -> ((Complex)x).acos();
      default      -> throw new TypeMismatch();
    };
  }
  
  public static final Primitive Acos = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = acos(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum atan(Datum x)
  {
    return switch (x.type()) {
      case Real    -> ((Real)x).atan();
      case Complex -> ((Complex)x).atan();
      default      -> throw new TypeMismatch();
    };
  }
  
  public static final Primitive Atan = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = atan(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static final Primitive Rand = new Primitive(0, false) {
    public void apply(VirtualMachine vm) {
      vm.result = new Real(Math.random());
      vm.popFrame();
    }
  };
  
  public static final Complex makeComplex(Datum x, Datum y)
  {
    if (x.type() != Type.Real || y.type() != Type.Real)
      throw new TypeMismatch();
    return new Complex((Real)x, (Real)y);
  }
  
  public static final Primitive MakeComplex = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = makeComplex(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };
    
  public static final Primitive RealPart = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.Complex)
        throw new TypeMismatch();
      vm.result = ((Complex)vm.popArg()).real();
      vm.popFrame();
    }
  };
  
  public static final Primitive ImagPart = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.Complex)
        throw new TypeMismatch();
      vm.result = ((Complex)vm.popArg()).imag();
      vm.popFrame();
    }
  };
  
  public static final Primitive RectToPolar = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.Complex)
        throw new TypeMismatch();
      vm.result = ((Complex)vm.popArg()).toPolar();
      vm.popFrame();
    }
  };
  
  public static final Primitive PolarToRect = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.Complex)
        throw new TypeMismatch();
      vm.result = ((Complex)vm.popArg()).toRect();
      vm.popFrame();
    }
  };
}
