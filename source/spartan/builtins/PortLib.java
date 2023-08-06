package spartan.builtins;

import spartan.data.*;
import spartan.errors.TypeMismatch;
import spartan.runtime.VirtualMachine;
import static spartan.builtins.Core.truth;

public final class PortLib
{
  // (port/open path flags)
  
  public static final Primitive OPEN = new Primitive(2, false) {    
    public void apply(VirtualMachine vm) {      
      if (!(vm.popArg() instanceof Text file && vm.popArg() instanceof Text flags))
        throw new TypeMismatch();
      vm.result = Port.open(file.str(), flags.str());
      vm.popFrame();
    }
  };
  
  // (port/close port)
  
  public static final Primitive CLOSE = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Port port))
        throw new TypeMismatch();
      port.close();
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
  
  // (port/open? port)
  
  public static final Primitive IS_OPEN = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Port port))
        throw new TypeMismatch();
      vm.result = truth(port.isOpen());
      vm.popFrame();
    }
  };
  
  // (port/read port buffer start count)
  
  public static final Primitive READ = new Primitive(4, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Port port && vm.popArg() instanceof Bytes bytes && vm.popArg() instanceof IInt start && vm.popArg() instanceof IInt count))
        throw new TypeMismatch();
      vm.result = new Int(port.read(bytes.buffer(), start.toInt32(), count.toInt32()));
      vm.popFrame();
    }
  };
  
  // (port/write port buffer start count)
  
  public static final Primitive WRITE = new Primitive(4, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Port port && vm.popArg() instanceof Bytes bytes && vm.popArg() instanceof IInt start && vm.popArg() instanceof IInt count))
        throw new TypeMismatch();
      vm.result = new Int(port.write(bytes.buffer(), start.toInt32(), count.toInt32()));
      vm.popFrame();
    }
  };
  
  // (port/position port)
  
  public static final Primitive POSITION = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Port port))
        throw new TypeMismatch();
      vm.result = new Int(port.position()); // TODO: use Int if it fits
      vm.popFrame();
    }
  };
  
  // (port/seek port position)
  
  public static final Primitive SEEK = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Port port && vm.popArg() instanceof IInt position))
        throw new TypeMismatch();
      port.seek(position.toInt64());
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
  
  // (port/length port)
  
  public static final Primitive LENGTH = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Port port))
        throw new TypeMismatch();
      vm.result = new Int(port.length()); // TODO: use Int if it fits
      vm.popFrame();
    }
  };
}
