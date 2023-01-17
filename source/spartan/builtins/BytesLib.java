package spartan.builtins;

import spartan.errors.TypeMismatch;
import spartan.data.Primitive;
import spartan.data.Bytes;
import spartan.data.Int;
import spartan.data.IInt;
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
      if (!(vm.popArg() instanceof IInt capacity))
        throw new TypeMismatch();
      vm.result = new Bytes(capacity.intValue());
      vm.popFrame();
    }
  };
  
  public static final Primitive REF = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Bytes bytes && vm.popArg() instanceof IInt index))
        throw new TypeMismatch();
      vm.result = new Int(bytes.get(index.intValue()));
      vm.popFrame();
    }
  };
  
  public static final Primitive SET = new Primitive(3, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Bytes bytes && vm.popArg() instanceof IInt index && vm.popArg() instanceof IInt elem))
        throw new TypeMismatch();
      bytes.set(index.intValue(), (byte) elem.intValue());
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
  
  public static final Primitive PUSH = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Bytes bytes && vm.popArg() instanceof IInt elem))
        throw new TypeMismatch();
      bytes.push((byte) elem.intValue());
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
  
  public static final Primitive POP = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Bytes bytes))
        throw new TypeMismatch();
      vm.result = new Int(bytes.pop());
      vm.popFrame();
    }
  };
  
  public static final Primitive FLIP = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Bytes bytes))
        throw new TypeMismatch();
      bytes.flip();
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
  
  public static final Primitive CLEAR = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Bytes bytes))
        throw new TypeMismatch();
      bytes.clear();
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
  
  public static final Primitive REMAINING = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Bytes bytes))
        throw new TypeMismatch();
      vm.result = new Int(bytes.remaining());
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_EMPTY = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Bytes bytes))
        throw new TypeMismatch();
      vm.result = Bool.of(bytes.isEmpty());
      vm.popFrame();
    }
  };
  
  private BytesLib() {}
}
