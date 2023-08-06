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
      if (! (vm.popArg() instanceof List args))
        throw new TypeMismatch();
      vm.args = args;
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
  
  public static final Primitive TYPE = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = Symbol.of(vm.popArg().type());
      vm.popFrame();
    }
  };
  
  public static final Primitive LOAD = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Text file))
        throw new TypeMismatch();
      try {
        spartan.Loader.load(file.str());
      }
      finally {
        vm.result = Nil.VALUE;
        vm.popFrame();
      }
    }
  };
  
  public static final Primitive SYMBOL_TO_TEXT = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Symbol x))
        throw new TypeMismatch();
      vm.result = new Text(x.str());
      vm.popFrame();
    }
  };
   
  public static final Primitive TEXT_TO_SYMBOL = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Text x))
        throw new TypeMismatch();
      vm.result = Symbol.of(x.str());
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
      if (!(vm.popArg() instanceof Text text))
        throw new TypeMismatch();
      vm.result = text.encode(Config.DEFAULT_ENCODING);
      vm.popFrame();
    }
  };
  
  public static final Primitive TEXT_TO_INT = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Text text))
        throw new TypeMismatch();
      try {
        vm.result = new Int(text.str());
        vm.popFrame();
      }
      catch (NumberFormatException ex) {
        throw new InvalidArgument();
      }      
    }
  };
  
  public static final Primitive BYTES_TO_TEXT = new Primitive(3, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Bytes bytes && vm.popArg() instanceof IInt start && vm.popArg() instanceof IInt count))
        throw new TypeMismatch();
      vm.result = new Text(bytes.decode(start.intValue(), count.intValue(), Config.DEFAULT_ENCODING));
      vm.popFrame();
    }
  };
    
  public static final Primitive ERROR = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Text errMsg))
        throw new TypeMismatch();
      throw new Error(errMsg.str());
    }
  };
  
  // (format-decimal num [precision])
  
  public static final Primitive FORMAT_DECIMAL = new Primitive(1, true) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof IReal num))
        throw new TypeMismatch();
      var args = vm.popRestArgs();
      var precision = 2;
      if (!args.empty() && args.car() instanceof IInt arg)
        precision = arg.intValue();
      Config.NUMERIC_FORMATTER.setMinimumFractionDigits(precision);
      Config.NUMERIC_FORMATTER.setMaximumFractionDigits(precision);
      vm.result = new Text(Config.NUMERIC_FORMATTER.format(num.doubleValue()));
      Config.NUMERIC_FORMATTER.setMinimumFractionDigits(Config.DEFAULT_DECIMAL_PRECISION);
      Config.NUMERIC_FORMATTER.setMaximumFractionDigits(Config.DEFAULT_DECIMAL_PRECISION);
      vm.popFrame();
    }
  };
  
  // (format-int num [base])
  
  public static final Primitive FORMAT_INT = new Primitive(1, true) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof IInt num))
        throw new TypeMismatch();
      var args = vm.popRestArgs();
      int base = 10;
      if (!args.empty() && args.car() instanceof IInt arg)
        base = arg.intValue();
      vm.result = new Text(num.format(base));
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_NIL = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(vm.popArg() instanceof Nil);
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_BOOL = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(vm.popArg() instanceof Bool);
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_INT = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(vm.popArg() instanceof IInt);
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_REAL = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(vm.popArg() instanceof IReal);
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_COMPLEX = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(vm.popArg() instanceof Complex);
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_NUMBER = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(vm.popArg() instanceof INum);
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_SYMBOL = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(vm.popArg() instanceof Symbol);
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_TEXT = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(vm.popArg() instanceof Text);
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_CALLABLE = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(vm.popArg() instanceof IFun);
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_VECTOR = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(vm.popArg() instanceof Vector);
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_LIST = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(vm.popArg() instanceof List);
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_PORT = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(vm.popArg() instanceof Port);
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_BYTES = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(vm.popArg() instanceof Bytes);
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
}
