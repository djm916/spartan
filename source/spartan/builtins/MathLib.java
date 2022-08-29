package spartan.builtins;

import spartan.data.*;
import spartan.errors.TypeMismatch;
import spartan.runtime.VirtualMachine;

public final class MathLib
{
  public static Datum add(Datum x, Datum y)
  {
    switch (x.type()) {
      case Int: {
        switch (y.type()) {
          case Int:     return ((Int)x).add((Int)y);
          case BigInt:  return ((Int)x).toBigInt().add((BigInt)y);
          case Ratio:   return ((Int)x).toRatio().add((Ratio)y);
          case Real:    return ((Int)x).toReal().add((Real)y);
          case Complex: return ((Int)x).toComplex().add((Complex)y);
        }
        break;
      }
      case BigInt: {
        switch (y.type()) {
          case Int:     return ((BigInt)x).add(((Int)y).toBigInt());
          case BigInt:  return ((BigInt)x).add((BigInt)y);
          case Ratio:   return ((BigInt)x).toRatio().add((Ratio)y);
          case Real:    return ((BigInt)x).toReal().add((Real)y);
          case Complex: return ((BigInt)x).toComplex().add((Complex)y);
        }
        break;
      }
      case Ratio: {
        switch (y.type()) {
          case Int:     return ((Ratio)x).add(((Int)y).toRatio());
          case BigInt:  return ((Ratio)x).add(((BigInt)y).toRatio());
          case Ratio:   return ((Ratio)x).add((Ratio)y);
          case Real:    return ((Ratio)x).toReal().add((Real)y);
          case Complex: return ((Ratio)x).toComplex().add((Complex)y);
        }
        break;
      }
      case Real: {
        switch (y.type()) {
          case Int:     return ((Real)x).add(((Int)y).toReal());
          case BigInt:  return ((Real)x).add(((BigInt)y).toReal());
          case Ratio:   return ((Real)x).add(((Ratio)y).toReal());
          case Real:    return ((Real)x).add((Real)y);
          case Complex: return ((Real)x).toComplex().add((Complex)y);
        }
        break;
      }
      case Complex: {
        switch (y.type()) {
          case Int:     return ((Complex)x).add(((Int)y).toComplex());
          case BigInt:  return ((Complex)x).add(((BigInt)y).toComplex());
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
          case BigInt:  return ((Int)x).toBigInt().sub((BigInt)y);
          case Ratio:   return ((Int)x).toRatio().sub((Ratio)y);
          case Real:    return ((Int)x).toReal().sub((Real)y);
          case Complex: return ((Int)x).toComplex().sub((Complex)y);
        }
        break;
      }
      case BigInt: {
        switch (y.type()) {
          case Int:     return ((BigInt)x).sub(((Int)y).toBigInt());
          case BigInt:  return ((BigInt)x).sub((BigInt)y);
          case Ratio:   return ((BigInt)x).toRatio().sub((Ratio)y);
          case Real:    return ((BigInt)x).toReal().sub((Real)y);
          case Complex: return ((BigInt)x).toComplex().sub((Complex)y);
        }
        break;
      }
      case Ratio: {
        switch (y.type()) {
          case Int:     return ((Ratio)x).sub(((Int)y).toRatio());
          case BigInt:  return ((Ratio)x).sub(((BigInt)y).toRatio());
          case Ratio:   return ((Ratio)x).sub((Ratio)y);
          case Real:    return ((Ratio)x).toReal().sub((Real)y);
          case Complex: return ((Ratio)x).toComplex().sub((Complex)y);
        }
        break;
      }
      case Real: {
        switch (y.type()) {
          case Int:     return ((Real)x).sub(((Int)y).toReal());
          case BigInt:  return ((Real)x).sub(((BigInt)y).toReal());
          case Ratio:   return ((Real)x).sub(((Ratio)y).toReal());
          case Real:    return ((Real)x).sub((Real)y);
          case Complex: return ((Real)x).toComplex().sub((Complex)y);
        }
        break;
      }
      case Complex: {
        switch (y.type()) {
          case Int:     return ((Complex)x).sub(((Int)y).toComplex());
          case BigInt:  return ((Complex)x).sub(((BigInt)y).toComplex());
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
          case BigInt:  return ((Int)x).toBigInt().mul((BigInt)y);
          case Ratio:   return ((Int)x).toRatio().mul((Ratio)y);
          case Real:    return ((Int)x).toReal().mul((Real)y);
          case Complex: return ((Int)x).toComplex().mul((Complex)y);
        }
        break;
      }
      case BigInt: {
        switch (y.type()) {
          case Int:     return ((BigInt)x).mul(((Int)y).toBigInt());
          case BigInt:  return ((BigInt)x).mul((BigInt)y);
          case Ratio:   return ((BigInt)x).toRatio().mul((Ratio)y);
          case Real:    return ((BigInt)x).toReal().mul((Real)y);
          case Complex: return ((BigInt)x).toComplex().mul((Complex)y);
        }
        break;
      }
      case Ratio: {
        switch (y.type()) {
          case Int:     return ((Ratio)x).mul(((Int)y).toRatio());
          case BigInt:  return ((Ratio)x).mul(((BigInt)y).toRatio());
          case Ratio:   return ((Ratio)x).mul((Ratio)y);
          case Real:    return ((Ratio)x).toReal().mul((Real)y);
          case Complex: return ((Ratio)x).toComplex().mul((Complex)y);
        }
        break;
      }
      case Real: {
        switch (y.type()) {
          case Int:     return ((Real)x).mul(((Int)y).toReal());
          case BigInt:  return ((Real)x).mul(((BigInt)y).toReal());
          case Ratio:   return ((Real)x).mul(((Ratio)y).toReal());
          case Real:    return ((Real)x).mul((Real)y);
          case Complex: return ((Real)x).toComplex().mul((Complex)y);
        }
        break;
      }
      case Complex: {
        switch (y.type()) {
          case Int:     return ((Complex)x).mul(((Int)y).toComplex());
          case BigInt:  return ((Complex)x).mul(((BigInt)y).toComplex());
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
          case BigInt:  return ((Int)x).toBigInt().div((BigInt)y);
          case Ratio:   return ((Int)x).toRatio().div((Ratio)y);
          case Real:    return ((Int)x).toReal().div((Real)y);
          case Complex: return ((Int)x).toComplex().div((Complex)y);
        }
        break;
      }
      case BigInt: {
        switch (y.type()) {
          case Int:     return ((BigInt)x).div(((Int)y).toBigInt());
          case BigInt:  return ((BigInt)x).div((BigInt)y);
          case Ratio:   return ((BigInt)x).toRatio().div((Ratio)y);
          case Real:    return ((BigInt)x).toReal().div((Real)y);
          case Complex: return ((BigInt)x).toComplex().div((Complex)y);
        }
        break;
      }
      case Ratio: {
        switch (y.type()) {
          case Int:     return ((Ratio)x).div(((Int)y).toRatio());
          case BigInt:  return ((Ratio)x).div(((BigInt)y).toRatio());
          case Ratio:   return ((Ratio)x).div((Ratio)y);
          case Real:    return ((Ratio)x).toReal().div((Real)y);
          case Complex: return ((Ratio)x).toComplex().div((Complex)y);
        }
        break;
      }
      case Real: {
        switch (y.type()) {
          case Int:     return ((Real)x).div(((Int)y).toReal());
          case BigInt:  return ((Real)x).div(((BigInt)y).toReal());
          case Ratio:   return ((Real)x).div(((Ratio)y).toReal());
          case Real:    return ((Real)x).div((Real)y);
          case Complex: return ((Real)x).toComplex().div((Complex)y);
        }
        break;
      }
      case Complex: {
        switch (y.type()) {
          case Int:     return ((Complex)x).div(((Int)y).toComplex());
          case BigInt:  return ((Complex)x).div(((BigInt)y).toComplex());
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
  
  public static Datum quotient(Datum x, Datum y)
  {
    switch (x.type()) {
      case Int: {
        switch (y.type()) {
          case Int:    return ((Int)x).quotient((Int)y);
          case BigInt: return ((Int)x).toBigInt().quotient((BigInt)y);
        }
        break;
      }
      case BigInt: {
        switch (y.type()) {
          case Int:    return ((BigInt)x).quotient(((Int)y).toBigInt());
          case BigInt: return ((BigInt)x).quotient((BigInt)y);
        }
        break;
      }
    }
    throw new TypeMismatch();
  }
  
  public static final Primitive Quotient = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = quotient(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };

  public static Datum remainder(Datum x, Datum y)
  {
    switch (x.type()) {
      case Int: {
        switch (y.type()) {
          case Int:    return ((Int)x).remainder((Int)y);
          case BigInt: return ((Int)x).toBigInt().remainder((BigInt)y);
        }
        break;
      }
      case BigInt: {
        switch (y.type()) {
          case Int:    return ((BigInt)x).remainder(((Int)y).toBigInt());
          case BigInt: return ((BigInt)x).remainder((BigInt)y);
        }
        break;
      }
    }
    throw new TypeMismatch();
  }
  
  public static final Primitive Remainder = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = remainder(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum neg(Datum x)
  {
    switch (x.type()) {
      case Int:     return ((Int)x).neg();
      case BigInt:  return ((BigInt)x).neg();
      case Ratio:   return ((Ratio)x).neg();
      case Real:    return ((Real)x).neg();
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
      case Int:     return ((Int)x).abs();
      case BigInt:  return ((BigInt)x).abs();
      case Ratio:   return ((Ratio)x).abs();
      case Real:    return ((Real)x).abs();
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
  
  public static Datum floor(Datum x)
  {
    switch (x.type()) {
      case Int:    
      case BigInt: return x;
      case Ratio:  return ((Ratio)x).toReal().floor();
      case Real:   return ((Real)x).floor();
    }
     
    throw new TypeMismatch();
  }
  
  public static final Primitive Floor = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = floor(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum ceiling(Datum x)
  {
    switch (x.type()) {
      case Int:    
      case BigInt: return x;
      case Ratio:  return ((Ratio)x).toReal().ceiling();
      case Real:   return ((Real)x).ceiling();
    }
    
    throw new TypeMismatch();
  }
  
  public static final Primitive Ceiling = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = ceiling(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum round(Datum x)
  {
    switch (x.type()) {
      case Int:    
      case BigInt: return x;
      case Ratio:  return ((Ratio)x).toReal().round();
      case Real:   return ((Real)x).round();
    }
     
    throw new TypeMismatch();
  }
  
  public static final Primitive Round = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = round(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum exp(Datum x, Datum y)
  {
    switch (x.type()) {
      case Int: {
        switch (y.type()) {
          case Int:     return ((Int)x).toReal().exp(((Int)y).toReal());
          case BigInt:  return ((Int)x).toReal().exp(((BigInt)y).toReal());
          case Ratio:   return ((Int)x).toReal().exp(((Ratio)y).toReal());
          case Real:    return ((Int)x).toReal().exp((Real)y);
          case Complex: return ((Int)x).toComplex().exp((Complex)y);
        }
        break;
      }
      case BigInt: {
        switch (y.type()) {
          case Int:     return ((BigInt)x).toReal().exp(((Int)y).toReal());
          case BigInt:  return ((BigInt)x).toReal().exp(((BigInt)y).toReal());
          case Ratio:   return ((BigInt)x).toReal().exp(((Ratio)y).toReal());
          case Real:    return ((BigInt)x).toReal().exp((Real)y);
          case Complex: return ((BigInt)x).toComplex().exp((Complex)y);
        }
        break;
      }
      case Ratio: {
        switch (y.type()) {
          case Int:     return ((Ratio)x).toReal().exp(((Int)y).toReal());
          case BigInt:  return ((BigInt)x).toReal().exp(((BigInt)y).toReal());
          case Ratio:   return ((Ratio)x).toReal().exp(((Ratio)y).toReal());
          case Real:    return ((Ratio)x).toReal().exp((Real)y);
          case Complex: return ((Ratio)x).toComplex().exp((Complex)y);
        }
        break;
      }
      case Real: {
        switch (y.type()) {
          case Int:     return ((Real)x).exp(((Int)y).toReal());
          case BigInt:  return ((Real)x).exp(((BigInt)y).toReal());
          case Ratio:   return ((Real)x).exp(((Ratio)y).toReal());
          case Real:    return ((Real)x).exp((Real)y);
          case Complex: return ((Real)x).toComplex().exp((Complex)y);
        }
        break;
      }
      case Complex: {
        switch (y.type()) {
          case Int:     return ((Complex)x).exp(((Int)y).toComplex());
          case BigInt:  return ((Complex)x).exp(((BigInt)y).toComplex());
          case Ratio:   return ((Complex)x).exp(((Ratio)y).toComplex());
          case Real:    return ((Complex)x).exp(((Real)y).toComplex());
          case Complex: return ((Complex)x).exp((Complex)y);
        }
        break;
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
    switch (x.type()) {
      case Int: {
        switch (y.type()) {
          case Int:     return ((Int)x).toReal().log(((Int)y).toReal());
          case BigInt:  return ((Int)x).toReal().log(((BigInt)y).toReal());
          case Ratio:   return ((Int)x).toReal().log(((Ratio)y).toReal());
          case Real:    return ((Int)x).toReal().log((Real)y);
          case Complex: return ((Int)x).toComplex().log((Complex)y);
        }
        break;
      }
      case BigInt: {
        switch (y.type()) {
          case Int:     return ((BigInt)x).toReal().log(((Int)y).toReal());
          case BigInt:  return ((BigInt)x).toReal().log(((BigInt)y).toReal());
          case Ratio:   return ((BigInt)x).toReal().log(((Ratio)y).toReal());
          case Real:    return ((BigInt)x).toReal().log((Real)y);
          case Complex: return ((BigInt)x).toComplex().log((Complex)y);
        }
        break;
      }
      case Ratio: {
        switch (y.type()) {
          case Int:     return ((Ratio)x).toReal().log(((Int)y).toReal());
          case BigInt:  return ((BigInt)x).toReal().log(((BigInt)y).toReal());
          case Ratio:   return ((Ratio)x).toReal().log(((Ratio)y).toReal());
          case Real:    return ((Ratio)x).toReal().log((Real)y);
          case Complex: return ((Ratio)x).toComplex().log((Complex)y);
        }
        break;
      }
      case Real: {
        switch (y.type()) {
          case Int:     return ((Real)x).log(((Int)y).toReal());
          case BigInt:  return ((Real)x).log(((BigInt)y).toReal());
          case Ratio:   return ((Real)x).log(((Ratio)y).toReal());
          case Real:    return ((Real)x).log((Real)y);
          case Complex: return ((Real)x).toComplex().log((Complex)y);
        }
        break;
      }
      case Complex: {
        switch (y.type()) {
          case Int:     return ((Complex)x).log(((Int)y).toComplex());
          case BigInt:  return ((Complex)x).log(((BigInt)y).toComplex());
          case Ratio:   return ((Complex)x).log(((Ratio)y).toComplex());
          case Real:    return ((Complex)x).log(((Real)y).toComplex());
          case Complex: return ((Complex)x).log((Complex)y);
        }
        break;
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
      case Int:     return ((Int)x).toReal().sin();
      case BigInt:  return ((BigInt)x).toReal().sin();
      case Real:    return ((Real)x).sin();
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
      case Int:     return ((Int)x).toReal().cos();
      case BigInt:  return ((BigInt)x).toReal().cos();
      case Real:    return ((Real)x).cos();
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
      case Int:     return ((Int)x).toReal().tan();
      case BigInt:  return ((BigInt)x).toReal().tan();
      case Real:    return ((Real)x).tan();
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
      case Int:     return ((Int)x).toReal().asin();
      case BigInt:  return ((BigInt)x).toReal().asin();
      case Real:    return ((Real)x).asin();
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
      case Int:     return ((Int)x).toReal().acos();
      case BigInt:  return ((BigInt)x).toReal().acos();
      case Real:    return ((Real)x).acos();
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
      case Int:     return ((Int)x).toReal().atan();
      case BigInt:  return ((BigInt)x).toReal().atan();
      case Real:    return ((Real)x).atan();
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
    switch (x.type()) {
      case Int: {
        switch (y.type()) {
          case Int:     return new Ratio(((Int)x).toBigInt(), ((Int)y).toBigInt());
          case BigInt:  return new Ratio(((Int)x).toBigInt(), (BigInt)y);
        }
        break;
      }
      case BigInt: {
        switch (y.type()) {
          case Int:     return new Ratio((BigInt)x, ((Int)y).toBigInt());
          case BigInt:  return new Ratio((BigInt)x, (BigInt)y);
        }
        break;
      }
    }
    throw new TypeMismatch();
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
