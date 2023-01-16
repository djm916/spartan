package spartan.builtins;

import spartan.data.*;
import spartan.errors.TypeMismatch;
import spartan.runtime.VirtualMachine;

public final class VectorLib
{  
  public static final Primitive FROM_LIST = new Primitive(0, true) {
    public void apply(VirtualMachine vm) {
      vm.result = Vector.fromList(vm.popRestArgs());
      vm.popFrame();
    }
  };
    
  public static final Primitive FILL = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      if (vm.popArg() instanceof IInt length) {
        vm.result = new Vector(length.intValue(), vm.popArg());
        vm.popFrame();
      }
      else throw new TypeMismatch();
    }
  };
  
  public static final Primitive COPY = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.popArg() instanceof Vector vector) {
        vm.result = new Vector(vector);
        vm.popFrame();
      }
      else throw new TypeMismatch();      
    }
  };
  
  public static final Primitive REF = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      if (vm.popArg() instanceof Vector vector && vm.popArg() instanceof IInt index) {
        vm.result = vector.at(index.intValue());
        vm.popFrame();
      }
      else throw new TypeMismatch();
    }
  };
  
  public static final Primitive SET = new Primitive(3, false) {
    public void apply(VirtualMachine vm) {
      if (vm.popArg() instanceof Vector vector && vm.popArg() instanceof IInt index) {
        vector.set(index.intValue(), vm.popArg());
        vm.result = Nil.VALUE;
        vm.popFrame();
      }
      else throw new TypeMismatch();
    }
  };
  
  public static final Primitive APPEND = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      if (vm.popArg() instanceof Vector vector) {
        vector.append(vm.popArg());
        vm.result = Nil.VALUE;
        vm.popFrame();
      }
      else throw new TypeMismatch();
    }
  };
}
