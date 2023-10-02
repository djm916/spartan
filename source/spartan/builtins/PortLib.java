package spartan.builtins;

import spartan.data.*;
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
  
  public static final Primitive IS_OPEN = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Port port))
        throw new TypeMismatch();
      vm.result = Bool.valueOf(port.isOpen());
      vm.popFrame();
    }
  };
  
  public static final Primitive READ = new Primitive(4, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Port port && vm.popArg() instanceof Bytes bytes && vm.popArg() instanceof IInt start && vm.popArg() instanceof IInt count))
        throw new TypeMismatch();
      vm.result = Int.valueOf(port.read(bytes.buffer(), start.intValue(), count.intValue()));
      vm.popFrame();
    }
  };
  
  public static final Primitive WRITE = new Primitive(4, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Port port && vm.popArg() instanceof Bytes bytes && vm.popArg() instanceof IInt start && vm.popArg() instanceof IInt count))
        throw new TypeMismatch();
      vm.result = Int.valueOf(port.write(bytes.buffer(), start.intValue(), count.intValue()));
      vm.popFrame();
    }
  };
  
  public static final Primitive POSITION = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Port port))
        throw new TypeMismatch();
      vm.result = Int.valueOf(port.position());
      vm.popFrame();
    }
  };
  
  public static final Primitive SEEK = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Port port && vm.popArg() instanceof IInt position))
        throw new TypeMismatch();
      port.seek(position.longValue());
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
  
  public static final Primitive SIZE = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Port port))
        throw new TypeMismatch();
      vm.result = Int.valueOf(port.size());
      vm.popFrame();
    }
  };
}
