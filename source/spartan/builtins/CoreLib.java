package spartan.builtins;

import spartan.data.*;
import spartan.errors.Error;
import spartan.errors.TypeMismatch;
import spartan.runtime.VirtualMachine;
import spartan.Config;
import java.util.Map;
import java.util.EnumMap;

public final class CoreLib
{
  public static Bool truth(boolean x)
  {
    return x ? Bool.TRUE : Bool.FALSE;
  }
  
  public static boolean truth(Datum x)
  {
    return !(x == Bool.FALSE || x == Nil.VALUE);
  }
  
  public static Bool not(Datum x)
  {
    return truth(!truth(x));
  }
  
  public static final Primitive NOT = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = not(vm.popArg());
      vm.popFrame();
    }
  };

  public static boolean eq(Datum x, Datum y)
  {
    if (x.type() != y.type())
      return false;
    
    return switch (x.type()) {
      case INT     -> ((Int)x).eq((Int)y);
      case REAL    -> ((Real)x).eq((Real)y);
      case COMPLEX -> ((Complex)x).eq((Complex)y);
      case TEXT    -> ((Text)x).eq((Text)y);
      case VECTOR  -> ((Vector)x).eq((Vector)y, CoreLib::eq);
      case LIST    -> ((List)x).eq((List)y, CoreLib::eq);
      case SYMBOL  -> ((Symbol)x).eq((Symbol)y);
      case BOOL,
           NIL     -> x == y;
      default      -> false;
    };
  }
  
  public static final Primitive EQ = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(eq(vm.popArg(), vm.popArg()));
      vm.popFrame();
    }
  };

  public static final Primitive NE = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(!eq(vm.popArg(), vm.popArg()));
      vm.popFrame();
    }
  };

  public static boolean lt(Datum x, Datum y)
  {
    if (x.type() != y.type())
      throw new TypeMismatch();
    
    return switch (x.type()) {
      case INT  -> ((Int)x).compare((Int)y) < 0;
      case REAL -> ((Real)x).compare((Real)y) < 0;
      case TEXT -> ((Text)x).compare((Text)y) < 0;
      default   -> throw new TypeMismatch();
    };
  }
  
  public static final Primitive LT = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(lt(vm.popArg(), vm.popArg()));
      vm.popFrame();
    }
  };

  public static boolean le(Datum x, Datum y)
  {
    if (x.type() != y.type())
      throw new TypeMismatch();
    
    return switch (x.type()) {
      case INT  -> ((Int)x).compare((Int)y) <= 0;
      case REAL -> ((Real)x).compare((Real)y) <= 0;
      case TEXT -> ((Text)x).compare((Text)y) <= 0;
      default   -> throw new TypeMismatch();
    };
  }
  
  public static final Primitive LE = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(le(vm.popArg(), vm.popArg()));
      vm.popFrame();
    }
  };

  public static boolean gt(Datum x, Datum y)
  {
    if (x.type() != y.type())
      throw new TypeMismatch();
    
    return switch (x.type()) {
      case INT  -> ((Int)x).compare((Int)y) > 0;
      case REAL -> ((Real)x).compare((Real)y) > 0;
      case TEXT -> ((Text)x).compare((Text)y) > 0;
      default   -> throw new TypeMismatch();
    };
  }
  
  public static final Primitive GT = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(gt(vm.popArg(), vm.popArg()));
      vm.popFrame();
    }
  };

  public static boolean ge(Datum x, Datum y)
  {
    if (x.type() != y.type())
      throw new TypeMismatch();
    
    return switch (x.type()) {
      case INT  -> ((Int)x).compare((Int)y) >= 0;
      case REAL -> ((Real)x).compare((Real)y) >= 0;
      case TEXT -> ((Text)x).compare((Text)y) >= 0;
      default   -> throw new TypeMismatch();
    };
  }
  
  public static final Primitive GE = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(ge(vm.popArg(), vm.popArg()));
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
  
  public static int length(Datum x)
  {
    return switch (x.type()) {
      case LIST   -> ((List)x).length();
      case VECTOR -> ((Vector)x).length();
      case TEXT   -> ((Text)x).length();
      default     -> throw new TypeMismatch();
    };
  }
  
  public static final Primitive LENGTH = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = new Int(length(vm.popArg()));
      vm.popFrame();
    }
  };
  
  public static final Primitive TYPE = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = typeSymbolMap.get(vm.popArg().type());
      vm.popFrame();
    }
    private Map<Type, Symbol> typeSymbolMap = new EnumMap<>(Type.class);
    {
      for (Type t : Type.values())
        typeSymbolMap.put(t, new Symbol(t.getName()));
    }
  };
  
  public static final Primitive LOAD = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.TEXT)
        throw new TypeMismatch();
      var fileName = ((Text) vm.popArg()).str();
      
      //TODO: Better handling of errors bubbling up from loaded file
      
      try {
        spartan.Loader.load(fileName, vm.globals);
      }
      catch (java.io.IOException ex) {
        throw new Error(ex.getMessage());
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
      vm.result = new Symbol(((Text)vm.popArg()).str());
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
  
  public static final Primitive BYTES_TO_TEXT = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.BYTES)
        throw new TypeMismatch();
      var bytes = (Bytes) vm.popArg();
      vm.result = bytes.decode(Config.DEFAULT_ENCODING);
      vm.popFrame();
    }
  };
  
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
      if (vm.peekArg().type() != Type.REAL)
        throw new TypeMismatch();
      var number = (Real) vm.popArg();
      vm.result = new Text(Config.NUMERIC_FORMATTER.format(number.value));
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_NIL = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(vm.popArg() == Nil.VALUE);
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_EMPTY_LIST = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(vm.popArg() == List.EMPTY);
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
  
  public static Datum max(List xs)
  {
    var max = xs.car();
    xs = xs.cdr();
    while (xs != List.EMPTY) {
      var x = xs.car();
      if (gt(x, max))
        max = x;
      xs = xs.cdr();
    }
    return max;
  }
  
  public static final Primitive MAX = new Primitive(1, true) {
    public void apply(VirtualMachine vm) {
      vm.result = max(vm.popRestArgs());
      vm.popFrame();
    }
  };
  
  public static Datum min(List xs)
  {
    var min = xs.car();
    xs = xs.cdr();
    while (xs != List.EMPTY) {
      var x = xs.car();
      if (lt(x, min))
        min = x;
      xs = xs.cdr();
    }
    return min;
  }
  
  public static final Primitive MIN = new Primitive(1, true) {
    public void apply(VirtualMachine vm) {
      vm.result = min(vm.popRestArgs());
      vm.popFrame();
    }
  };
}
