package spartan.builtins;

import spartan.data.*;
import spartan.errors.IOError;
import spartan.errors.TypeMismatch;
import spartan.runtime.VirtualMachine;

public final class PortLib
{
  public static final Primitive OPEN = new Primitive(2, false) {    
    public void apply(VirtualMachine vm) {      
      if (vm.peekArg().type() != Type.TEXT)
        throw new TypeMismatch();
      var fileName = ((Text) vm.popArg()).str();
      
      if (vm.peekArg().type() != Type.TEXT)
        throw new TypeMismatch();
      var flags = ((Text) vm.popArg()).str();
      
      vm.result = Port.open(fileName, flags);
      vm.popFrame();
    }
  };
  
  public static final Primitive CLOSE = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.PORT)
        throw new TypeMismatch();
      var port = (Port) vm.popArg();      
      port.close();
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
  
  public static final Primitive READ = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.PORT)
        throw new TypeMismatch();
      var port = (Port) vm.popArg();
      
      if (vm.peekArg().type() != Type.INT)
        throw new TypeMismatch();
      var numBytes = ((Int) vm.popArg()).value;
      
      vm.result = port.read(numBytes);
      vm.popFrame();
    }
  };
  
  public static final Primitive WRITE = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.PORT)
        throw new TypeMismatch();
      var port = (Port) vm.popArg();
      
      if (vm.peekArg().type() != Type.BYTES)
        throw new TypeMismatch();
      var bytes = (Bytes) vm.popArg();
      
      port.write(bytes);
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
}
