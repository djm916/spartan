package spartan.builtins;

import spartan.data.Datum;
import spartan.data.IInt;
import spartan.data.Bool;
import spartan.data.Signature;
import spartan.data.Primitive;
import spartan.runtime.VirtualMachine;
import spartan.errors.TypeMismatch;

public final class BitOpsLib
{
  public static Datum bitAnd(Datum lhs, Datum rhs)
  {
    if (lhs instanceof IInt z1 && rhs instanceof IInt z2)
      return z1.bitAnd(z2);
    throw new TypeMismatch();
  }
  
  public static Datum bitOr(Datum lhs, Datum rhs)
  {
    if (lhs instanceof IInt z1 && rhs instanceof IInt z2)
      return z1.bitOr(z2);
    throw new TypeMismatch();
  }
  
  public static Datum bitXor(Datum lhs, Datum rhs)
  {
    if (lhs instanceof IInt z1 && rhs instanceof IInt z2)
      return z1.bitXor(z2);
    throw new TypeMismatch();
  }
  
  public static final Primitive NOT = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof IInt arg))
        throw new TypeMismatch();
      vm.result = arg.bitNot();
      vm.popFrame();
    }
  };
  
  public static final Primitive AND = new Primitive(Signature.variadic(2)) {
    public void apply(VirtualMachine vm) {
      vm.result = bitAnd(vm.popArg(), vm.popArg());
      while (!vm.args.isEmpty())
        vm.result = bitAnd(vm.result, vm.popArg());
      vm.popFrame();
    }
  };
  
  public static final Primitive OR = new Primitive(Signature.variadic(2)) {
    public void apply(VirtualMachine vm) {
      vm.result = bitOr(vm.popArg(), vm.popArg());
      while (!vm.args.isEmpty())
        vm.result = bitOr(vm.result, vm.popArg());
      vm.popFrame();
    }
  };
  
  public static final Primitive XOR = new Primitive(Signature.variadic(2)) {
    public void apply(VirtualMachine vm) {
      vm.result = bitXor(vm.popArg(), vm.popArg());
      while (!vm.args.isEmpty())
        vm.result = bitXor(vm.result, vm.popArg());
      vm.popFrame();
    }
  };
  
  // (bit-set? n index)
  
  public static final Primitive IS_BIT_SET = new Primitive(Signature.fixed(2)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof IInt num))
        throw new TypeMismatch();
      if (!(vm.popArg() instanceof IInt idx))
        throw new TypeMismatch();
      vm.result = Bool.valueOf(num.isBitSet(idx.intValue()));
      vm.popFrame();
    }
  };
  
  // (set-bit n index)
  
  public static final Primitive SET_BIT = new Primitive(Signature.fixed(2)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof IInt num))
        throw new TypeMismatch();
      if (!(vm.popArg() instanceof IInt idx))
        throw new TypeMismatch();
      vm.result = num.setBit(idx.intValue());
      vm.popFrame();
    }
  };
  
  // (clear-bit n index)
  
  public static final Primitive CLEAR_BIT = new Primitive(Signature.fixed(2)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof IInt num))
        throw new TypeMismatch();
      if (!(vm.popArg() instanceof IInt idx))
        throw new TypeMismatch();
      vm.result = num.clearBit(idx.intValue());
      vm.popFrame();
    }
  };
  
  // (flip-bit n index)
  
  public static final Primitive FLIP_BIT = new Primitive(Signature.fixed(2)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof IInt num))
        throw new TypeMismatch();
      if (!(vm.popArg() instanceof IInt idx))
        throw new TypeMismatch();
      vm.result = num.flipBit(idx.intValue());
      vm.popFrame();
    }
  };
}
