package spartan.builtins;

import spartan.data.Type;
import spartan.data.Primitive;
import spartan.data.Text;
import spartan.data.List;
import spartan.data.Int;
import spartan.runtime.VirtualMachine;
import spartan.errors.TypeMismatch;

public final class TextLib
{  
  public static Text concat(List args) throws TypeMismatch
  {
    var buffer = new StringBuilder();
    for (; !args.empty(); args = args.cdr()) {
      if (args.car().type() != Type.Text)
        throw new TypeMismatch();
      buffer.append(((Text)args.car()).value());
    }
    return new Text(buffer.toString());
  }
  
  public static final Primitive Concat = new Primitive(0, true) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
      vm.result = concat(vm.popRestArgs());
      vm.popFrame();
    }
  };
  
  public static final Primitive Hash = new Primitive(1, true) {
    public void apply(VirtualMachine vm) throws TypeMismatch {
      vm.result = new Int(((Text)vm.popArg()).hash());
      vm.popFrame();
    }
  };
}
