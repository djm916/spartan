package spartan.builtins;

import spartan.errors.TypeMismatch;
import spartan.data.Type;
import spartan.data.Primitive;
import spartan.data.Bytes;
import spartan.data.Int;
import spartan.data.Nil;
import spartan.data.Bool;
import spartan.data.List;
import spartan.runtime.VirtualMachine;

public final class BytesLib
{
  public static final Primitive FROM_LIST = new Primitive(0, true) {
    public void apply(VirtualMachine vm) {      
      vm.result = Bytes.fromList(vm.popRestArgs());
      vm.popFrame();
    }
  };
  
  public static final Primitive MAKE_BYTES = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.INT)
        throw new TypeMismatch();
      var capacity = ((Int) vm.popArg()).value;
      vm.result = new Bytes(capacity);
      vm.popFrame();
    }
  };
  
  public static final Primitive REF = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.BYTES)
        throw new TypeMismatch();
      var bytes = (Bytes) vm.popArg();
      if (vm.peekArg().type() != Type.INT)
        throw new TypeMismatch();
      var index = ((Int) vm.popArg()).value;
      vm.result = new Int(bytes.get(index));
      vm.popFrame();
    }
  };
  
  public static final Primitive SET = new Primitive(3, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.BYTES)
        throw new TypeMismatch();
      var bytes = (Bytes) vm.popArg();
      if (vm.peekArg().type() != Type.INT)
        throw new TypeMismatch();
      var index = ((Int) vm.popArg()).value;
      if (vm.peekArg().type() != Type.INT)
        throw new TypeMismatch();
      var value = (byte) ((Int) vm.popArg()).value;
      bytes.set(index, value);
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
  
  public static final Primitive PUSH = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.BYTES)
        throw new TypeMismatch();
      var bytes = (Bytes) vm.popArg();
      if (vm.peekArg().type() != Type.INT)
        throw new TypeMismatch();
      var value = (byte) ((Int) vm.popArg()).value;
      bytes.push(value);
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
  
  public static final Primitive POP = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.BYTES)
        throw new TypeMismatch();
      var bytes = (Bytes) vm.popArg();      
      vm.result = new Int(bytes.pop());
      vm.popFrame();
    }
  };
  
  public static final Primitive FLIP = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.BYTES)
        throw new TypeMismatch();
      var bytes = (Bytes) vm.popArg();
      bytes.flip();
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
  
  public static final Primitive CLEAR = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.BYTES)
        throw new TypeMismatch();
      var bytes = (Bytes) vm.popArg();
      bytes.clear();
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
  
  public static final Primitive REMAINING = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.BYTES)
        throw new TypeMismatch();
      var bytes = (Bytes) vm.popArg();
      vm.result = new Int(bytes.remaining());
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_EMPTY = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.BYTES)
        throw new TypeMismatch();
      var bytes = (Bytes) vm.popArg();
      vm.result = bytes.remaining() == 0 ? Bool.TRUE : Bool.FALSE;
      vm.popFrame();
    }
  };
  
  private BytesLib() {}
}
