package spartan.builtins;

import spartan.data.*;
import spartan.errors.TypeMismatch;
import spartan.errors.DivisionByZero;
import spartan.runtime.VirtualMachine;

public final class MathLib
{
  public static Datum add(Datum x, Datum y)
  {
    switch (x.type()) {
      case Int: {
        switch (y.type()) {
          case Int:     return ((Int)x).add((Int)y);
          case Ratio:   return ((Int)x).toRatio().add((Ratio)y);
          case Real:    return ((Int)x).toReal().add((Real)y);
          case Complex: return ((Int)x).toComplex().add((Complex)y);
        }
        break;
      }
      case Ratio: {
        switch (y.type()) {
          case Int:     return ((Ratio)x).add(((Int)y).toRatio());
          case Ratio:   return ((Ratio)x).add((Ratio)y);
          case Real:    return ((Ratio)x).toReal().add((Real)y);
          case Complex: return ((Ratio)x).toComplex().add((Complex)y);
        }
        break;
      }
      case Real: {
        switch (y.type()) {
          case Int:     return ((Real)x).add(((Int)y).toReal());
          case Ratio:   return ((Real)x).add(((Ratio)y).toReal());
          case Real:    return ((Real)x).add((Real)y);
          case Complex: return ((Real)x).toComplex().add((Complex)y);
        }
        break;
      }
      case Complex: {
        switch (y.type()) {
          case Int:     return ((Complex)x).add(((Int)y).toComplex());
          case Ratio:   return ((Complex)x).add(((Ratio)y).toComplex());
          case Real:    return ((Complex)x).add(((Real)y).toComplex());
          case Complex: return ((Complex)x).add((Complex)y);
        }
        break;
      }
    }
    throw new TypeMismatch();
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
    switch (x.type()) {
      case Int: {
        switch (y.type()) {
          case Int:     return ((Int)x).sub((Int)y);
          case Ratio:   return ((Int)x).toRatio().sub((Ratio)y);
          case Real:    return ((Int)x).toReal().sub((Real)y);
          case Complex: return ((Int)x).toComplex().sub((Complex)y);
        }
        break;
      }
      case Ratio: {
        switch (y.type()) {
          case Int:     return ((Ratio)x).sub(((Int)y).toRatio());
          case Ratio:   return ((Ratio)x).sub((Ratio)y);
          case Real:    return ((Ratio)x).toReal().sub((Real)y);
          case Complex: return ((Ratio)x).toComplex().sub((Complex)y);
        }
        break;
      }
      case Real: {
        switch (y.type()) {
          case Int:     return ((Real)x).sub(((Int)y).toReal());
          case Ratio:   return ((Real)x).sub(((Ratio)y).toReal());
          case Real:    return ((Real)x).sub((Real)y);
          case Complex: return ((Real)x).toComplex().sub((Complex)y);
        }
        break;
      }
      case Complex: {
        switch (y.type()) {
          case Int:     return ((Complex)x).sub(((Int)y).toComplex());
          case Ratio:   return ((Complex)x).sub(((Ratio)y).toComplex());
          case Real:    return ((Complex)x).sub(((Real)y).toComplex());
          case Complex: return ((Complex)x).sub((Complex)y);
        }
        break;
      }
    }
    throw new TypeMismatch();
  }
  
  public static final Primitive Sub = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = sub(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };

  public static Datum mul(Datum x, Datum y)
  {
    switch (x.type()) {
      case Int: {
        switch (y.type()) {
          case Int:     return ((Int)x).mul((Int)y);
          case Ratio:   return ((Int)x).toRatio().mul((Ratio)y);
          case Real:    return ((Int)x).toReal().mul((Real)y);
          case Complex: return ((Int)x).toComplex().mul((Complex)y);
        }
        break;
      }
      case Ratio: {
        switch (y.type()) {
          case Int:     return ((Ratio)x).mul(((Int)y).toRatio());
          case Ratio:   return ((Ratio)x).mul((Ratio)y);
          case Real:    return ((Ratio)x).toReal().mul((Real)y);
          case Complex: return ((Ratio)x).toComplex().mul((Complex)y);
        }
        break;
      }
      case Real: {
        switch (y.type()) {
          case Int:     return ((Real)x).mul(((Int)y).toReal());
          case Ratio:   return ((Real)x).mul(((Ratio)y).toReal());
          case Real:    return ((Real)x).mul((Real)y);
          case Complex: return ((Real)x).toComplex().mul((Complex)y);
        }
        break;
      }
      case Complex: {
        switch (y.type()) {
          case Int:     return ((Complex)x).mul(((Int)y).toComplex());
          case Ratio:   return ((Complex)x).mul(((Ratio)y).toComplex());
          case Real:    return ((Complex)x).mul(((Real)y).toComplex());
          case Complex: return ((Complex)x).mul((Complex)y);
        }
        break;
      }
    }
    throw new TypeMismatch();
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
    switch (x.type()) {
      case Int: {
        switch (y.type()) {
          case Int:     return ((Int)x).div((Int)y);
          case Ratio:   return ((Int)x).toRatio().div((Ratio)y);
          case Real:    return ((Int)x).toReal().div((Real)y);
          case Complex: return ((Int)x).toComplex().div((Complex)y);
        }
        break;
      }
      case Ratio: {
        switch (y.type()) {
          case Int:     return ((Ratio)x).div(((Int)y).toRatio());
          case Ratio:   return ((Ratio)x).div((Ratio)y);
          case Real:    return ((Ratio)x).toReal().div((Real)y);
          case Complex: return ((Ratio)x).toComplex().div((Complex)y);
        }
        break;
      }
      case Real: {
        switch (y.type()) {
          case Int:     return ((Real)x).div(((Int)y).toReal());
          case Ratio:   return ((Real)x).div(((Ratio)y).toReal());
          case Real:    return ((Real)x).div((Real)y);
          case Complex: return ((Real)x).toComplex().div((Complex)y);
        }
        break;
      }
      case Complex: {
        switch (y.type()) {
          case Int:     return ((Complex)x).div(((Int)y).toComplex());
          case Ratio:   return ((Complex)x).div(((Ratio)y).toComplex());
          case Real:    return ((Complex)x).div(((Real)y).toComplex());
          case Complex: return ((Complex)x).div((Complex)y);
        }
        break;
      }
    }
    throw new TypeMismatch();
  }
    
  public static final Primitive Div = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = div(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };

  public static Datum mod(Datum x, Datum y)
  {
    if (x.type() == Type.Int && y.type() == Type.Int)
      return ((Int)x).mod((Int)y);
    throw new TypeMismatch();
  }
  
  public static final Primitive Mod = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = mod(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };

  public static Datum neg(Datum x)
  {
    switch (x.type()) {
      case Int: return ((Int)x).neg();
      case Ratio: return ((Ratio)x).neg();
      case Real: return ((Real)x).neg();
      case Complex: return ((Complex)x).neg();
    }
    throw new TypeMismatch();
  }
  
  public static final Primitive Neg = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = neg(vm.popArg());
      vm.popFrame();
    }
  };

  public static Datum abs(Datum x)
  {
    switch (x.type()) {
      case Int: return ((Int)x).abs();
      case Ratio: return ((Ratio)x).abs();
      case Real: return ((Real)x).abs();
      case Complex: return ((Complex)x).abs();
    }
    throw new TypeMismatch();
  }
  
  public static final Primitive Abs = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = abs(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Real floor(Datum x)
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
  
  public static Real ceiling(Datum x)
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
  
  public static Int round(Datum x)
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
  
  public static Datum exp(Datum x, Datum y)
  {
    if (x.type() == y.type()) {
      switch (x.type()) {
        case Real: return ((Real)x).exp((Real)y);
        case Complex: return ((Complex)x).exp((Complex)y);
      }
    }
    throw new TypeMismatch();
  }
  
  public static final Primitive Exp = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = exp(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum log(Datum x, Datum y)
  {
    if (x.type() == y.type()) {
      switch (x.type()) {
        case Real: return ((Real)x).log((Real)y);
        case Complex: return ((Complex)x).log((Complex)y);
      }
    }
    throw new TypeMismatch();
  }
  
  public static final Primitive Log = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = log(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum sin(Datum x)
  {
    switch (x.type()) {
      case Real: return ((Real)x).sin();
      case Complex: return ((Complex)x).sin();
    }
    throw new TypeMismatch();
  }
  
  public static final Primitive Sin = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = sin(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum cos(Datum x)
  {
    switch (x.type()) {
      case Real: return ((Real)x).cos();
      case Complex: return ((Complex)x).cos();
    }
    throw new TypeMismatch();
  }
  
  public static final Primitive Cos = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = cos(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum tan(Datum x)
  {
    switch (x.type()) {
      case Real: return ((Real)x).tan();
      case Complex: return ((Complex)x).tan();
    }
    throw new TypeMismatch();
  }
  
  public static final Primitive Tan = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = tan(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum asin(Datum x)
  {
    switch (x.type()) {
      case Real: return ((Real)x).asin();
      case Complex: return ((Complex)x).asin();
    }
    throw new TypeMismatch();
  }
  
  public static final Primitive Asin = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = asin(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum acos(Datum x)
  {
    switch (x.type()) {
      case Real: return ((Real)x).acos();
      case Complex: return ((Complex)x).acos();
    }
    throw new TypeMismatch();
  }
  
  public static final Primitive Acos = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = acos(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum atan(Datum x)
  {
    switch (x.type()) {
      case Real: return ((Real)x).atan();
      case Complex: return ((Complex)x).atan();
    }
    throw new TypeMismatch();
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
  
  public static final Ratio makeRatio(Datum x, Datum y)
  {
    if (x.type() != Type.Int || y.type() != Type.Int)
      throw new TypeMismatch();
    return new Ratio((Int)x, (Int)y);
  }
  
  public static final Primitive MakeRatio = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = makeRatio(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };
  
  public static final Primitive Numerator = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.Ratio)
        throw new TypeMismatch();
      vm.result = ((Ratio)vm.popArg()).numerator();
      vm.popFrame();
    }
  };
  
  public static final Primitive Denominator = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.Ratio)
        throw new TypeMismatch();
      vm.result = ((Ratio)vm.popArg()).denominator();
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
}
