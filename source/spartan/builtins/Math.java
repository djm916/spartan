package spartan.builtins;

import spartan.data.*;
import spartan.errors.TypeMismatch;
import spartan.runtime.VirtualMachine;

public final class Math
{
  public static Datum add(Datum x, Datum y) throws TypeMismatch
  {
    if (x.type() == y.type()) {
      switch (x.type()) {
        case Int: return ((Int)x).add((Int)y);
        case Real: return ((Real)x).add((Real)y);
      }
    }
    throw new TypeMismatch();
  }
    
  public static final PrimFun Add = new PrimFun(2, true) {
    public void doApply(VirtualMachine vm) throws TypeMismatch {
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
        case Int: return ((Int)x).sub((Int)y);
        case Real: return ((Real)x).sub((Real)y);
      }
    }
    throw new TypeMismatch();
  }
  
  public static final PrimFun Sub = new PrimFun(2, false) {
    public void doApply(VirtualMachine vm) throws TypeMismatch {
      vm.result = sub(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };

  public static Datum mul(Datum x, Datum y) throws TypeMismatch
  {
    if (x.type() == y.type()) {
      switch (x.type()) {
        case Int: return ((Int)x).mul((Int)y);
        case Real: return ((Real)x).mul((Real)y);
      }
    }
    throw new TypeMismatch();
  }
    
  public static final PrimFun Mul = new PrimFun(2, true) {
    public void doApply(VirtualMachine vm) throws TypeMismatch {
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
        case Int: return ((Int)x).div((Int)y);
        case Real: return ((Real)x).div((Real)y);
      }
    }
    throw new TypeMismatch();
  }
  
  public static final PrimFun Div = new PrimFun(2, false) {
    public void doApply(VirtualMachine vm) throws TypeMismatch {
      vm.result = div(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };

  public static Datum mod(Datum x, Datum y) throws TypeMismatch
  {
    if (x.type() == Type.Int && y.type() == Type.Int)
      return ((Int)x).mod((Int)y);
    throw new TypeMismatch();
  }
  
  public static final PrimFun Mod = new PrimFun(2, false) {
    public void doApply(VirtualMachine vm) throws TypeMismatch {
      vm.result = mod(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };

  public static Datum neg(Datum x) throws TypeMismatch
  {
    switch (x.type()) {
      case Int: return ((Int)x).neg();
      case Real: return ((Real)x).neg();
    }
    throw new TypeMismatch();
  }
  
  public static final PrimFun Neg = new PrimFun(1, false) {
    public void doApply(VirtualMachine vm) throws TypeMismatch {
      vm.result = neg(vm.popArg());
      vm.popFrame();
    }
  };

  public static Datum abs(Datum x) throws TypeMismatch
  {
    switch (x.type()) {
      case Int: return ((Int)x).abs();
      case Real: return ((Real)x).abs();
    }
    throw new TypeMismatch();
  }
  
  public static final PrimFun Abs = new PrimFun(1, false) {
    public void doApply(VirtualMachine vm) throws TypeMismatch {
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
  
  public static final PrimFun Floor = new PrimFun(1, false) {
    public void doApply(VirtualMachine vm) throws TypeMismatch {
      vm.result = floor(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum ceiling(Datum x) throws TypeMismatch
  {
    switch (x.type()) {
      case Real: return ((Real)x).ceiling();
    }
    throw new TypeMismatch();
  }
  
  public static final PrimFun Ceiling = new PrimFun(1, false) {
    public void doApply(VirtualMachine vm) throws TypeMismatch {
      vm.result = ceiling(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum exp(Datum x, Datum y) throws TypeMismatch
  {
    if (x.type() == y.type()) {
      switch (x.type()) {
        case Real: return ((Real)x).exp((Real)y);
      }
    }
    throw new TypeMismatch();
  }
  
  public static final PrimFun Exp = new PrimFun(2, false) {
    public void doApply(VirtualMachine vm) throws TypeMismatch {
      vm.result = exp(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum log(Datum x, Datum y) throws TypeMismatch
  {
    if (x.type() == y.type()) {
      switch (x.type()) {
        case Real: return ((Real)x).log((Real)y);
      }
    }
    throw new TypeMismatch();
  }
  
  public static final PrimFun Log = new PrimFun(2, false) {
    public void doApply(VirtualMachine vm) throws TypeMismatch {
      vm.result = log(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum sin(Datum x) throws TypeMismatch
  {
    switch (x.type()) {
      case Real: return ((Real)x).sin();
    }
    throw new TypeMismatch();
  }
  
  public static final PrimFun Sin = new PrimFun(1, false) {
    public void doApply(VirtualMachine vm) throws TypeMismatch {
      vm.result = sin(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum cos(Datum x) throws TypeMismatch
  {
    switch (x.type()) {
      case Real: return ((Real)x).cos();
    }
    throw new TypeMismatch();
  }
  
  public static final PrimFun Cos = new PrimFun(1, false) {
    public void doApply(VirtualMachine vm) throws TypeMismatch {
      vm.result = cos(vm.popArg());
      vm.popFrame();
    }
  };
  
  public static Datum tan(Datum x) throws TypeMismatch
  {
    switch (x.type()) {
      case Real: return ((Real)x).tan();
    }
    throw new TypeMismatch();
  }
  
  public static final PrimFun Tan = new PrimFun(1, false) {
    public void doApply(VirtualMachine vm) throws TypeMismatch {
      vm.result = tan(vm.popArg());
      vm.popFrame();
    }
  };
}
