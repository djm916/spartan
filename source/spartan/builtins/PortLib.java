package spartan.builtins;

import spartan.data.*;
import spartan.errors.IOError;
import spartan.errors.TypeMismatch;
import spartan.runtime.VirtualMachine;

public final class PortLib
{
  public static final Primitive OPEN = new Primitive(2, false) {    
    public void apply(VirtualMachine vm) {      
      if (!(vm.popArg() instanceof Text file && vm.popArg() instanceof Text flags))
        throw new TypeMismatch();
      vm.result = Port.open(file.str(), flags.str());
      vm.popFrame();
    }
  };
  
  public static final Primitive CLOSE = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Port port))
        throw new TypeMismatch();
      port.close();
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
  
  public static final Primitive READ = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Port port && vm.popArg() instanceof Bytes bytes))
        throw new TypeMismatch();
      vm.result = new Int(port.read(bytes));
      vm.popFrame();
    }
  };
  
  public static final Primitive WRITE = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Port port && vm.popArg() instanceof Bytes bytes))
        throw new TypeMismatch();
      vm.result = new Int(port.write(bytes));
      vm.popFrame();
    }
  };
}
