package spartan.builtins;

import spartan.data.*;
import spartan.data.Void; // shadows java.lang.Void
import spartan.errors.TypeMismatch;
import spartan.runtime.VirtualMachine;

public final class PortLib
{
  // (open-file file-path open-mode)
  
  public static final Primitive OPEN = new Primitive(Signature.fixed(2)) {    
    public void apply(VirtualMachine vm) {      
      if (!(vm.popArg() instanceof Text file && vm.popArg() instanceof Text flags))
        throw new TypeMismatch();
      vm.result = Port.open(file.str(), flags.str());
      vm.popFrame();
    }
  };
  
  // (port-close port)
  
  public static final Primitive CLOSE = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Port port))
        throw new TypeMismatch();
      port.close();
      vm.result = Void.VALUE;
      vm.popFrame();
    }
  };
  
  // (port-open? port)
  
  public static final Primitive IS_OPEN = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Port port))
        throw new TypeMismatch();
      vm.result = Bool.valueOf(port.isOpen());
      vm.popFrame();
    }
  };
  
  // (port-read port buffer start end)
  
  public static final Primitive READ = new Primitive(Signature.fixed(4)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Port port && vm.popArg() instanceof Bytes bytes && vm.popArg() instanceof IInt start && vm.popArg() instanceof IInt count))
        throw new TypeMismatch();
      vm.result = Int.valueOf(port.read(bytes.buffer(), start.intValue(), count.intValue()));
      vm.popFrame();
    }
  };
  
  // (port-write port buffer start end)
  
  public static final Primitive WRITE = new Primitive(Signature.fixed(4)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Port port && vm.popArg() instanceof Bytes bytes && vm.popArg() instanceof IInt start && vm.popArg() instanceof IInt count))
        throw new TypeMismatch();
      vm.result = Int.valueOf(port.write(bytes.buffer(), start.intValue(), count.intValue()));
      vm.popFrame();
    }
  };
  
  // (port-position port)
  
  public static final Primitive POSITION = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Port port))
        throw new TypeMismatch();
      vm.result = Int.valueOf(port.position());
      vm.popFrame();
    }
  };
  
  // (port-seek port position)
  
  public static final Primitive SEEK = new Primitive(Signature.fixed(2)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Port port && vm.popArg() instanceof IInt position))
        throw new TypeMismatch();
      port.seek(position.longValue());
      vm.result = Void.VALUE;
      vm.popFrame();
    }
  };
  
  // (port-size port)
  
  public static final Primitive SIZE = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Port port))
        throw new TypeMismatch();
      vm.result = Int.valueOf(port.size());
      vm.popFrame();
    }
  };
}
