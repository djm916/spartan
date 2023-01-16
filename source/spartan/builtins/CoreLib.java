package spartan.builtins;

import spartan.data.*;
import spartan.errors.Error;
import spartan.errors.TypeMismatch;
import spartan.errors.InvalidArgument;
import spartan.runtime.VirtualMachine;
import spartan.Config;
import spartan.parsing.Reader;
import static spartan.builtins.Core.*;

public final class CoreLib
{
  public static final Primitive NOT = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = not(vm.popArg());
      vm.popFrame();
    }
  };
 
  public static final Primitive EQ = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(isEqual(vm.popArg(), vm.popArg()));
      vm.popFrame();
    }
  };

  public static final Primitive NE = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(!isEqual(vm.popArg(), vm.popArg()));
      vm.popFrame();
    }
  };
  
  public static final Primitive LT = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(compareTo(vm.popArg(), vm.popArg()) < 0);
      vm.popFrame();
    }
  };
  
  public static final Primitive LE = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(compareTo(vm.popArg(), vm.popArg()) <= 0);
      vm.popFrame();
    }
  };
  
  public static final Primitive GT = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(compareTo(vm.popArg(), vm.popArg()) > 0);
      vm.popFrame();
    }
  };
  
  public static final Primitive GE = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(compareTo(vm.popArg(), vm.popArg()) >= 0);
      vm.popFrame();
    }
  };
    
  public static final Primitive APPLY = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = vm.popArg();
      if (vm.peekArg().type() != Type.LIST)
        throw new TypeMismatch();
      vm.args = (List)vm.popArg();
      vm.apply(vm.args.length());
    }
  };
    
  public static final Primitive PRINT = new Primitive(1, true) {
    public void apply(VirtualMachine vm) {
      while (!vm.args.empty())
        System.out.print(vm.popArg().str());
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
  
  public static final Primitive PRINT_LINE = new Primitive(1, true) {
    public void apply(VirtualMachine vm) {
      while (!vm.args.empty())
        System.out.print(vm.popArg().str());
      System.out.println();
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
    
  public static final Primitive LENGTH = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = new Int(length(vm.popArg()));
      vm.popFrame();
    }
  };
  
  public static final Primitive TYPE = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      var value = vm.popArg();
      var type = value.type();
      if (type == Type.WRAPPED)
        vm.result = ((Wrapped)value).tag();
      else
        vm.result = type.toSymbol();
      vm.popFrame();
    }
  };
  
  public static final Primitive LOAD = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.TEXT)
        throw new TypeMismatch();
      var file = ((Text) vm.popArg()).str();
      
      //TODO: Better handling of errors bubbling up from loaded file
      
      try {
        spartan.Loader.load(file);
      }
      finally {
        vm.result = Nil.VALUE;
        vm.popFrame();
      }
    }
  };
  
  public static final Primitive SYMBOL_TO_TEXT = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.SYMBOL)
        throw new TypeMismatch();
      vm.result = new Text(((Symbol)vm.popArg()).str());
      vm.popFrame();
    }
  };
    
  public static final Primitive TEXT_TO_SYMBOL = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.TEXT)
        throw new TypeMismatch();
      vm.result = new Symbol(((Text)vm.popArg()).str()).intern();
      vm.popFrame();
    }
  };
  
  public static final Primitive GENSYM = new Primitive(0, false) {
    public void apply(VirtualMachine vm) {
      vm.result = Symbol.generateUnique();
      vm.popFrame();
    }
  };
    
  public static final Primitive IDENTITY_HASH = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = new Int(System.identityHashCode(vm.popArg()));
      vm.popFrame();
    }
  };

  public static final Primitive TEXT_TO_BYTES = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.TEXT)
        throw new TypeMismatch();
      var text = (Text) vm.popArg();
      vm.result = text.encode(Config.DEFAULT_ENCODING);
      vm.popFrame();
    }
  };
  
  public static final Primitive TEXT_TO_INT = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.TEXT)
        throw new TypeMismatch();
      var text = (Text) vm.popArg();
      try {
        vm.result = new Int(text.str());
      }
      catch (NumberFormatException ex) {
        throw new InvalidArgument();
      }
      vm.popFrame();
    }
  };
  
  public static final Primitive BYTES_TO_TEXT = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.BYTES)
        throw new TypeMismatch();
      var bytes = (Bytes) vm.popArg();
      vm.result = bytes.decode(Config.DEFAULT_ENCODING);
      vm.popFrame();
    }
  };
  
  /*
  public static final Primitive TEXT_TO_NUMBER = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.TEXT)
        throw new TypeMismatch();
      var text = (Text) vm.popArg();
      try {
        vm.result = Reader.forString(text.str()).readNumber();
      }
      catch (java.io.IOException ex) {
        // ignore exception
      }
      finally {
        vm.popFrame();
      }
    }
  };
  */
  
  public static final Primitive ERROR = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.TEXT)
        throw new TypeMismatch();
      var errMsg = (Text) vm.popArg();
      throw new Error(errMsg.str());
    }
  };
  
  public static final Primitive FORMAT_DECIMAL = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (! (vm.peekArg() instanceof IReal))
        throw new TypeMismatch();
      var number = (IReal) vm.popArg();
      vm.result = new Text(Config.NUMERIC_FORMATTER.format(number.doubleValue()));
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_NIL = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(vm.popArg() == Nil.VALUE);
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_BOOL = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(vm.popArg().type().isBool());
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_INT = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(vm.popArg().type().isInt());
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_REAL = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(vm.popArg().type().isReal());
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_COMPLEX = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(vm.popArg().type().isComplex());
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_NUMBER = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(vm.popArg().type().isNumber());
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_SYMBOL = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(vm.popArg().type().isSymbol());
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_TEXT = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(vm.popArg().type().isText());
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_CALLABLE = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(vm.popArg().type().isCallable());
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_VECTOR = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(vm.popArg().type().isVector());
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_LIST = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(vm.popArg().type().isList());
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_EMPTY_LIST = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(vm.popArg() == List.EMPTY);
      vm.popFrame();
    }
  };
  
  public static final Primitive MAX = new Primitive(1, true) {
    public void apply(VirtualMachine vm) {
      vm.result = max(vm.popRestArgs());
      vm.popFrame();
    }
  };
  
  public static final Primitive MIN = new Primitive(1, true) {
    public void apply(VirtualMachine vm) {
      vm.result = min(vm.popRestArgs());
      vm.popFrame();
    }
  };
  
  public static final Primitive WRAP_TYPE = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      if (!vm.peekArg().type().isSymbol())
        throw new TypeMismatch();
      var type = (Symbol) vm.popArg();
      var value = vm.popArg();
      vm.result = new Wrapped(type, value);
      vm.popFrame();
    }
  };
  
  public static final Primitive UNWRAP_TYPE = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.WRAPPED)
        throw new TypeMismatch();      
      vm.result = ((Wrapped) vm.popArg()).value();
      vm.popFrame();
    }
  };
}
