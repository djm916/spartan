package spartan.builtins;

import spartan.data.*;
import spartan.errors.TypeMismatch;
import spartan.runtime.VirtualMachine;

public final class MathLib
{
  public static Datum add(Datum x, Datum y) throws TypeMismatch
  {
    if (x.type() == y.type()) {
      switch (x.type()) {
        case Int: return Int.add((Int)x, (Int)y);
        case Real: return ((Real)x).add((Real)y);
        case Complex: return Complex.add((Complex)x, (Complex)y);
      }
    }
    throw new TypeMismatch();
  }
    
  public static final Primitive Add = new Primitive(2, true) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
      vm.result = add(vm.popArg(), vm.popArg());
      while (vm.args != List.Empty)
        vm.result = add(vm.result, vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum sub(Datum x, Datum y) throws TypeMismatch
  {
    if (x.type() == y.type()) {
      switch (x.type()) {
        case Int: return Int.sub((Int)x, (Int)y);
        case Real: return ((Real)x).sub((Real)y);
        case Complex: return Complex.sub((Complex)x, (Complex)y);
      }
    }
    throw new TypeMismatch();
  }
  
  public static final Primitive Sub = new Primitive(2, false) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
      vm.result = sub(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };

  public static Datum mul(Datum x, Datum y) throws TypeMismatch
  {
    if (x.type() == y.type()) {
      switch (x.type()) {
        case Int: return Int.mul((Int)x, (Int)y);
        case Real: return ((Real)x).mul((Real)y);
        case Complex: return Complex.mul((Complex)x, (Complex)y);
      }
    }
    throw new TypeMismatch();
  }
    
  public static final Primitive Mul = new Primitive(2, true) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
      vm.result = mul(vm.popArg(), vm.popArg());
      while (vm.args != List.Empty)
        vm.result = mul(vm.result, vm.popArg());
      vm.popFrame();
    }
  };

  public static Datum div(Datum x, Datum y) throws TypeMismatch
  {
    if (x.type() == y.type()) {
      switch (x.type()) {
        case Int: return Int.div((Int)x, (Int)y);
        case Real: return ((Real)x).div((Real)y);
        case Complex: return Complex.div((Complex)x, (Complex)y);
      }
    }
    throw new TypeMismatch();
  }
  
  public static final Primitive Div = new Primitive(2, false) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
      vm.result = div(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };

  public static Datum mod(Datum x, Datum y) throws TypeMismatch
  {
    if (x.type() == Type.Int && y.type() == Type.Int)
      return Int.mod((Int)x, (Int)y);
    throw new TypeMismatch();
  }
  
  public static final Primitive Mod = new Primitive(2, false) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
      vm.result = mod(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };

  public static Datum neg(Datum x) throws TypeMismatch
  {
    switch (x.type()) {
      case Int: return ((Int)x).neg();
      case Real: return ((Real)x).neg();
      case Complex: return ((Complex)x).neg();
    }
    throw new TypeMismatch();
  }
  
  public static final Primitive Neg = new Primitive(1, false) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
      vm.result = neg(vm.popArg());
      vm.popFrame();
    }
  };

  public static Datum abs(Datum x) throws TypeMismatch
  {
    switch (x.type()) {
      case Int: return ((Int)x).abs();
      case Real: return ((Real)x).abs();
      case Complex: return ((Complex)x).abs();
    }
    throw new TypeMismatch();
  }
  
  public static final Primitive Abs = new Primitive(1, false) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
      vm.result = abs(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum floor(Datum x) throws TypeMismatch
  {
    switch (x.type()) {
      case Real: return ((Real)x).floor();
    }
    throw new TypeMismatch();
  }
  
  public static final Primitive Floor = new Primitive(1, false) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
      vm.result = floor(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum ceil(Datum x) throws TypeMismatch
  {
    switch (x.type()) {
      case Real: return ((Real)x).ceil();
    }
    throw new TypeMismatch();
  }
  
  public static final Primitive Ceil = new Primitive(1, false) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
      vm.result = ceil(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum exp(Datum x, Datum y) throws TypeMismatch
  {
    if (x.type() == y.type()) {
      switch (x.type()) {
        case Real: return ((Real)x).exp((Real)y);
        case Complex: return Complex.exp((Complex)x, (Complex)y);
      }
    }
    throw new TypeMismatch();
  }
  
  public static final Primitive Exp = new Primitive(2, false) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
      vm.result = exp(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum log(Datum x, Datum y) throws TypeMismatch
  {
    if (x.type() == y.type()) {
      switch (x.type()) {
        case Real: return ((Real)x).log((Real)y);
        case Complex: return Complex.log((Complex)x, (Complex)y);
      }
    }
    throw new TypeMismatch();
  }
  
  public static final Primitive Log = new Primitive(2, false) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
      vm.result = log(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum sin(Datum x) throws TypeMismatch
  {
    switch (x.type()) {
      case Real: return ((Real)x).sin();
      case Complex: return ((Complex)x).sin();
    }
    throw new TypeMismatch();
  }
  
  public static final Primitive Sin = new Primitive(1, false) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
      vm.result = sin(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum cos(Datum x) throws TypeMismatch
  {
    switch (x.type()) {
      case Real: return ((Real)x).cos();
      case Complex: return ((Complex)x).cos();
    }
    throw new TypeMismatch();
  }
  
  public static final Primitive Cos = new Primitive(1, false) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
      vm.result = cos(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum tan(Datum x) throws TypeMismatch
  {
    switch (x.type()) {
      case Real: return ((Real)x).tan();
      case Complex: return ((Complex)x).tan();
    }
    throw new TypeMismatch();
  }
  
  public static final Primitive Tan = new Primitive(1, false) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
      vm.result = tan(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum asin(Datum x) throws TypeMismatch
  {
    switch (x.type()) {
      case Real: return ((Real)x).asin();
      case Complex: return ((Complex)x).asin();
    }
    throw new TypeMismatch();
  }
  
  public static final Primitive Asin = new Primitive(1, false) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
      vm.result = asin(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum acos(Datum x) throws TypeMismatch
  {
    switch (x.type()) {
      case Real: return ((Real)x).acos();
      case Complex: return ((Complex)x).acos();
    }
    throw new TypeMismatch();
  }
  
  public static final Primitive Acos = new Primitive(1, false) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
      vm.result = acos(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum atan(Datum x) throws TypeMismatch
  {
    switch (x.type()) {
      case Real: return ((Real)x).atan();
      case Complex: return ((Complex)x).atan();
    }
    throw new TypeMismatch();
  }
  
  public static final Primitive Atan = new Primitive(1, false) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
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
}
