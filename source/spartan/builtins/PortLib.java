package spartan.builtins;

import spartan.data.*;
import spartan.errors.IOError;
import spartan.errors.TypeMismatch;
import spartan.runtime.VirtualMachine;

public final class PortLib
{
  public static final Primitive Open = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {      
      if (vm.peekArg().type() != Type.Text)
        throw new TypeMismatch();
      var fileName = ((Text) vm.popArg()).str();
      
      if (vm.peekArg().type() != Type.Text)
        throw new TypeMismatch();
      var flags = ((Text) vm.popArg()).str();
      
      vm.result = Port.open(fileName, flags);
      vm.popFrame();
    }
  };
  
  public static final Primitive Close = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.Port)
        throw new TypeMismatch();
      var port = (Port) vm.popArg();      
      port.close();
      vm.result = Nil.Value;
      vm.popFrame();
    }
  };
  
  public static final Primitive Read = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.Port)
        throw new TypeMismatch();
      var port = (Port) vm.popArg();
      
      if (vm.peekArg().type() != Type.Int)
        throw new TypeMismatch();
      var numBytes = ((Int) vm.popArg()).intValue();
      
      vm.result = port.read(numBytes);
      vm.popFrame();
    }
  };
  
  public static final Primitive Write = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.Port)
        throw new TypeMismatch();
      var port = (Port) vm.popArg();
      
      if (vm.peekArg().type() != Type.Bytes)
        throw new TypeMismatch();
      var bytes = (Bytes) vm.popArg();
      
      port.write(bytes);
      vm.result = Nil.Value;
      vm.popFrame();
    }
  };
}
