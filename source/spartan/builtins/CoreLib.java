package spartan.builtins;

import spartan.data.*;
import spartan.errors.Error;
import spartan.errors.TypeMismatch;
import spartan.errors.InvalidArgument;
import spartan.runtime.VirtualMachine;
import spartan.Config;
import spartan.parsing.Reader;

public final class CoreLib
{
  public static Bool not(Datum x)
  {
    return Bool.valueOf(!x.boolValue());
  }
  
  public static boolean isEqual(Datum x, Datum y)
  {
    if (x instanceof IEq lhs && y instanceof IEq rhs)
      return lhs.isEqual(rhs);
    throw new TypeMismatch();
  }
  
  public static int compareTo(Datum x, Datum y)
  {
    if (x instanceof IOrd lhs && y instanceof IOrd rhs)
      return lhs.compareTo(rhs);
    throw new TypeMismatch();
  }
  
  public static final Primitive NOT = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = not(vm.popArg());
      vm.popFrame();
    }
  };
 
  public static final Primitive EQ = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = Bool.valueOf(isEqual(vm.popArg(), vm.popArg()));
      vm.popFrame();
    }
  };

  public static final Primitive NE = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = Bool.valueOf(!isEqual(vm.popArg(), vm.popArg()));
      vm.popFrame();
    }
  };
  
  public static final Primitive LT = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = Bool.valueOf(compareTo(vm.popArg(), vm.popArg()) < 0);
      vm.popFrame();
    }
  };
  
  public static final Primitive LE = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = Bool.valueOf(compareTo(vm.popArg(), vm.popArg()) <= 0);
      vm.popFrame();
    }
  };
  
  public static final Primitive GT = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = Bool.valueOf(compareTo(vm.popArg(), vm.popArg()) > 0);
      vm.popFrame();
    }
  };
  
  public static final Primitive GE = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = Bool.valueOf(compareTo(vm.popArg(), vm.popArg()) >= 0);
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
      while (!vm.args.isEmpty())
        System.out.print(vm.popArg().str());
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
  
  public static final Primitive PRINT_LINE = new Primitive(1, true) {
    public void apply(VirtualMachine vm) {
      while (!vm.args.isEmpty())
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
      vm.result = Int.valueOf(System.identityHashCode(vm.popArg()));
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
        vm.result = Int.valueOf(text.str());
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
      if (!args.isEmpty() && args.car() instanceof IInt arg)
        precision = arg.intValue();
      vm.result = new Text(num.formatDec(precision));
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
      if (!args.isEmpty() && args.car() instanceof IInt arg)
        base = arg.intValue();
      vm.result = new Text(num.formatInt(base));
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_NIL = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = Bool.valueOf(vm.popArg() instanceof Nil);
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_BOOL = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = Bool.valueOf(vm.popArg() instanceof Bool);
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_INT = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = Bool.valueOf(vm.popArg() instanceof IInt);
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_REAL = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = Bool.valueOf(vm.popArg() instanceof IReal);
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_COMPLEX = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = Bool.valueOf(vm.popArg() instanceof Complex);
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_NUMBER = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = Bool.valueOf(vm.popArg() instanceof INum);
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_SYMBOL = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = Bool.valueOf(vm.popArg() instanceof Symbol);
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_TEXT = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = Bool.valueOf(vm.popArg() instanceof Text);
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_CALLABLE = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = Bool.valueOf(vm.popArg() instanceof IFun);
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_VECTOR = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = Bool.valueOf(vm.popArg() instanceof Vector);
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_LIST = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = Bool.valueOf(vm.popArg() instanceof List);
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_PORT = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = Bool.valueOf(vm.popArg() instanceof Port);
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_BYTES = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = Bool.valueOf(vm.popArg() instanceof Bytes);
      vm.popFrame();
    }
  };
  
  public static final Primitive AT = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof IAssoc c))
        throw new TypeMismatch();
      var k = vm.popArg();
      vm.result = c.get(k);
      vm.popFrame();
    }
  };
  
  public static final Primitive SET_AT = new Primitive(3, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof IAssoc c))
        throw new TypeMismatch();
      var k = vm.popArg();
      var v = vm.popArg();
      c.set(k, v);
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
  
  public static final Primitive LENGTH = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof ILen c))
        throw new TypeMismatch();
      vm.result = Int.valueOf(c.length());
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_EMPTY = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof ILen c))
        throw new TypeMismatch();
      vm.result = Bool.valueOf(c.isEmpty());
      vm.popFrame();
    }
  };
  
  public static final Primitive SYMBOL_INTERN = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Symbol s))
        throw new TypeMismatch();
      vm.result = s.intern();
      vm.popFrame();
    }
  };
  
  public static final Primitive NAMESPACE = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Symbol pkg))
        throw new TypeMismatch();
      spartan.Runtime.enterNS(pkg);
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
  
  public static final Primitive IMPORT = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Symbol pkg && vm.popArg() instanceof List vars))
        throw new TypeMismatch();
      //spartan.Runtime.currentPackage().importChecked(spartan.Runtime.getPackage(pkg), vars);
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
}
