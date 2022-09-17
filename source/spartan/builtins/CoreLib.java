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
    return x ? Bool.True : Bool.False;
  }
  
  public static boolean truth(Datum x)
  {
    return !(x == Bool.False || x == Nil.Value);
  }
  
  public static Bool not(Datum x)
  {
    return truth(!truth(x));
  }
  
  public static final Primitive Not = new Primitive(1, false) {
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
      case Int     -> ((Int)x).eq((Int)y);
      case Real    -> ((Real)x).eq((Real)y);
      case Complex -> ((Complex)x).eq((Complex)y);
      case Text    -> ((Text)x).eq((Text)y);
      case Vector  -> ((Vector)x).eq((Vector)y, CoreLib::eq);
      case List    -> ((List)x).eq((List)y, CoreLib::eq);
      case Bool,
           Symbol,
           Nil     -> x == y;
      default      -> false;
    };
  }
  
  public static final Primitive Eq = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(eq(vm.popArg(), vm.popArg()));
      vm.popFrame();
    }
  };

  public static final Primitive Ne = new Primitive(2, false) {
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
      case Int  -> ((Int)x).compare((Int)y) < 0;
      case Real -> ((Real)x).compare((Real)y) < 0;
      case Text -> ((Text)x).compare((Text)y) < 0;
      default   -> throw new TypeMismatch();
    };
  }
  
  public static final Primitive Lt = new Primitive(2, false) {
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
      case Int  -> ((Int)x).compare((Int)y) <= 0;
      case Real -> ((Real)x).compare((Real)y) <= 0;
      case Text -> ((Text)x).compare((Text)y) <= 0;
      default   -> throw new TypeMismatch();
    };
  }
  
  public static final Primitive Le = new Primitive(2, false) {
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
      case Int  -> ((Int)x).compare((Int)y) > 0;
      case Real -> ((Real)x).compare((Real)y) > 0;
      case Text -> ((Text)x).compare((Text)y) > 0;
      default   -> throw new TypeMismatch();
    };
  }
  
  public static final Primitive Gt = new Primitive(2, false) {
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
      case Int  -> ((Int)x).compare((Int)y) >= 0;
      case Real -> ((Real)x).compare((Real)y) >= 0;
      case Text -> ((Text)x).compare((Text)y) >= 0;
      default   -> throw new TypeMismatch();
    };
  }
  
  public static final Primitive Ge = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(ge(vm.popArg(), vm.popArg()));
      vm.popFrame();
    }
  };
    
  public static final Primitive Apply = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = vm.popArg();
      if (vm.peekArg().type() != Type.List)
        throw new TypeMismatch();
      vm.args = (List)vm.popArg();
      vm.apply(vm.args.length());
    }
  };
  
  public static final Primitive Print = new Primitive(1, true) {
    public void apply(VirtualMachine vm) {
      while (!vm.args.empty())
        System.out.print(vm.popArg().str());
      vm.result = Nil.Value;
      vm.popFrame();
    }
  };
  
  public static final Primitive PrintLine = new Primitive(1, true) {
    public void apply(VirtualMachine vm) {
      while (!vm.args.empty())
        System.out.print(vm.popArg().str());
      System.out.println();
      vm.result = Nil.Value;
      vm.popFrame();
    }
  };
  
  public static int length(Datum x)
  {
    switch (x.type()) {
      case List: return ((List)x).length();
      case Vector: return ((Vector)x).length();
      case Text: return ((Text)x).length();
    }
    throw new TypeMismatch();
  }
  
  public static final Primitive Length = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = new Int(length(vm.popArg()));
      vm.popFrame();
    }
  };
  
  public static final Primitive TypeOf = new Primitive(1, false) {
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
  
  public static final Primitive Load = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.Text)
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
        vm.result = Nil.Value;
        vm.popFrame();
      }
    }
  };
  
  public static final Primitive SymbolToText = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.Symbol)
        throw new TypeMismatch();
      vm.result = new Text(((Symbol)vm.popArg()).str());
      vm.popFrame();
    }
  };
    
  public static final Primitive TextToSymbol = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.Text)
        throw new TypeMismatch();
      vm.result = new Symbol(((Text)vm.popArg()).str());
      vm.popFrame();
    }
  };
  
  public static final Primitive GenSym = new Primitive(0, false) {
    public void apply(VirtualMachine vm) {
      vm.result = Symbol.generateUnique();
      vm.popFrame();
    }
  };
    
  public static final Primitive IdentityHash = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = new Int(System.identityHashCode(vm.popArg()));
      vm.popFrame();
    }
  };

  public static final Primitive TextToBytes = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.Text)
        throw new TypeMismatch();
      var text = (Text) vm.popArg();
      vm.result = text.encode(Config.DefaultEncoding);
      vm.popFrame();
    }
  };
  
  public static final Primitive BytesToText = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.Bytes)
        throw new TypeMismatch();
      var bytes = (Bytes) vm.popArg();
      vm.result = bytes.decode(Config.DefaultEncoding);
      vm.popFrame();
    }
  };
  
  public static final Primitive Error = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.Text)
        throw new TypeMismatch();
      var errMsg = (Text) vm.popArg();
      throw new Error(errMsg.str());
    }
  };
  
  public static final Primitive FormatDecimal = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.Real)
        throw new TypeMismatch();
      var number = (Real) vm.popArg();
      vm.result = new Text(Config.NumberFormatter.format(number.value));
      vm.popFrame();
    }
  };
  
  public static final Primitive IsNil = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(vm.popArg() == Nil.Value);
      vm.popFrame();
    }
  };
  
  public static final Primitive IsEmptyList = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(vm.popArg() == List.Empty);
      vm.popFrame();
    }
  };
  
  public static final Primitive IsBoolean = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(vm.popArg().type().isBool());
      vm.popFrame();
    }
  };
  
  public static final Primitive IsInteger = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(vm.popArg().type().isInt());
      vm.popFrame();
    }
  };
  
  public static final Primitive IsReal = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(vm.popArg().type().isReal());
      vm.popFrame();
    }
  };
  
  public static final Primitive IsComplex = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(vm.popArg().type().isComplex());
      vm.popFrame();
    }
  };
  
  public static final Primitive IsNumber = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(vm.popArg().type().isNumber());
      vm.popFrame();
    }
  };
  
  public static final Primitive IsSymbol = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(vm.popArg().type().isSymbol());
      vm.popFrame();
    }
  };
  
  public static final Primitive IsText = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(vm.popArg().type().isText());
      vm.popFrame();
    }
  };
  
  public static final Primitive IsCallable = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(vm.popArg().type().isCallable());
      vm.popFrame();
    }
  };
  
  public static Datum max(List xs)
  {
    var max = xs.car();
    xs = xs.cdr();
    while (xs != List.Empty) {
      var x = xs.car();
      if (gt(x, max))
        max = x;
      xs = xs.cdr();
    }
    return max;
  }
  
  public static final Primitive Max = new Primitive(1, true) {
    public void apply(VirtualMachine vm) {
      vm.result = max(vm.popRestArgs());
      vm.popFrame();
    }
  };
  
  public static Datum min(List xs)
  {
    var min = xs.car();
    xs = xs.cdr();
    while (xs != List.Empty) {
      var x = xs.car();
      if (lt(x, min))
        min = x;
      xs = xs.cdr();
    }
    return min;
  }
  
  public static final Primitive Min = new Primitive(1, true) {
    public void apply(VirtualMachine vm) {
      vm.result = min(vm.popRestArgs());
      vm.popFrame();
    }
  };
}
