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
    switch (x.type()) {
      case INT: {
        switch (y.type()) {
          case INT:     return ((Int)x).eq((Int)y);
          case BIGINT:  return ((Int)x).toBigInt().eq((BigInt)y);
          case RATIO:   return ((Int)x).toRatio().eq((Ratio)y);
          case REAL:    return ((Int)x).toReal().eq((Real)y);
          case COMPLEX: return ((Int)x).toComplex().eq((Complex)y);
        }
        break;
      }
      case BIGINT: {
        switch (y.type()) {
          case INT:     return ((BigInt)x).eq(((Int)y).toBigInt());
          case BIGINT:  return ((BigInt)x).eq((BigInt)y);
          case RATIO:   return ((BigInt)x).toRatio().eq((Ratio)y);
          case REAL:    return ((BigInt)x).toReal().eq((Real)y);
          case COMPLEX: return ((BigInt)x).toComplex().eq((Complex)y);
        }
        break;
      }
      case RATIO: {
        switch (y.type()) {
          case INT:     return ((Ratio)x).eq(((Int)y).toRatio());
          case BIGINT:  return ((Ratio)x).eq(((BigInt)y).toRatio());
          case RATIO:   return ((Ratio)x).eq((Ratio)y);
          case REAL:    return ((Ratio)x).toReal().eq((Real)y);
          case COMPLEX: return ((Ratio)x).toComplex().eq((Complex)y);
        }
        break;
      }
      case REAL: {
        switch (y.type()) {
          case INT:     return ((Real)x).eq(((Int)y).toReal());
          case BIGINT:  return ((Real)x).eq(((BigInt)y).toReal());
          case RATIO:   return ((Real)x).eq(((Ratio)y).toReal());
          case REAL:    return ((Real)x).eq((Real)y);
          case COMPLEX: return ((Real)x).toComplex().eq((Complex)y);
        }
        break;
      }
      case COMPLEX: {
        switch (y.type()) {
          case INT:     return ((Complex)x).eq(((Int)y).toComplex());
          case BIGINT:  return ((Complex)x).eq(((BigInt)y).toComplex());
          case RATIO:   return ((Complex)x).eq(((Ratio)y).toComplex());
          case REAL:    return ((Complex)x).eq(((Real)y).toComplex());
          case COMPLEX: return ((Complex)x).eq((Complex)y);
        }
        break;
      }
      case SYMBOL: {
        switch (y.type()) {
          case SYMBOL: return ((Symbol)x).eq((Symbol)y);
        }
        break;
      }
      case TEXT: {
        switch (y.type()) {
          case TEXT: return ((Text)x).eq((Text)y);
        }
        break;
      }
      case VECTOR: {
        switch (y.type()) {
          case VECTOR: return ((Vector)x).eq((Vector)y, CoreLib::eq);
        }
        break;
      }
      case LIST: {
        switch (y.type()) {
          case LIST: return ((List)x).eq((List)y, CoreLib::eq);
        }
        break;
      }
      case BOOL:
      case NIL: return x == y;
    }
    return false;
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
    switch (x.type()) {
      case INT: {
        switch (y.type()) {
          case INT:     return ((Int)x).compare((Int)y) < 0;
          case BIGINT:  return ((Int)x).toBigInt().compare((BigInt)y) < 0;
          case RATIO:   return ((Int)x).toRatio().compare((Ratio)y) < 0;
          case REAL:    return ((Int)x).toReal().compare((Real)y) < 0;
        }
        break;
      }
      case BIGINT: {
        switch (y.type()) {
          case INT:     return ((BigInt)x).compare(((Int)y).toBigInt()) < 0;
          case BIGINT:  return ((BigInt)x).compare((BigInt)y) < 0;
          case RATIO:   return ((BigInt)x).toRatio().compare((Ratio)y) < 0;
          case REAL:    return ((BigInt)x).toReal().compare((Real)y) < 0;
        }
        break;
      }
      case RATIO: {
        switch (y.type()) {
          case INT:     return ((Ratio)x).compare(((Int)y).toRatio()) < 0;
          case BIGINT:  return ((Ratio)x).compare(((BigInt)y).toRatio()) < 0;
          case RATIO:   return ((Ratio)x).compare((Ratio)y) < 0;
          case REAL:    return ((Ratio)x).toReal().compare((Real)y) < 0;
        }
        break;
      }
      case REAL: {
        switch (y.type()) {
          case INT:     return ((Real)x).compare(((Int)y).toReal()) < 0;
          case BIGINT:  return ((Real)x).compare(((BigInt)y).toReal()) < 0;
          case RATIO:   return ((Real)x).compare(((Ratio)y).toReal()) < 0;
          case REAL:    return ((Real)x).compare((Real)y) < 0;
        }
        break;
      }
      case TEXT: {
        switch (y.type()) {
          case TEXT: return ((Text)x).compare((Text)y) < 0;
        }
        break;
      }
    }
    throw new TypeMismatch();
  }
  
  public static final Primitive LT = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(lt(vm.popArg(), vm.popArg()));
      vm.popFrame();
    }
  };

  public static boolean le(Datum x, Datum y)
  {
    switch (x.type()) {
      case INT: {
        switch (y.type()) {
          case INT:     return ((Int)x).compare((Int)y) <= 0;
          case BIGINT:  return ((Int)x).toBigInt().compare((BigInt)y) <= 0;
          case RATIO:   return ((Int)x).toRatio().compare((Ratio)y) <= 0;
          case REAL:    return ((Int)x).toReal().compare((Real)y) <= 0;
        }
        break;
      }
      case BIGINT: {
        switch (y.type()) {
          case INT:     return ((BigInt)x).compare(((Int)y).toBigInt()) <= 0;
          case BIGINT:  return ((BigInt)x).compare((BigInt)y) <= 0;
          case RATIO:   return ((BigInt)x).toRatio().compare((Ratio)y) <= 0;
          case REAL:    return ((BigInt)x).toReal().compare((Real)y) <= 0;
        }
        break;
      }
      case RATIO: {
        switch (y.type()) {
          case INT:     return ((Ratio)x).compare(((Int)y).toRatio()) <= 0;
          case BIGINT:  return ((Ratio)x).compare(((BigInt)y).toRatio()) <= 0;
          case RATIO:   return ((Ratio)x).compare((Ratio)y) <= 0;
          case REAL:    return ((Ratio)x).toReal().compare((Real)y) <= 0;
        }
        break;
      }
      case REAL: {
        switch (y.type()) {
          case INT:     return ((Real)x).compare(((Int)y).toReal()) <= 0;
          case BIGINT:  return ((Real)x).compare(((BigInt)y).toReal()) <= 0;
          case RATIO:   return ((Real)x).compare(((Ratio)y).toReal()) <= 0;
          case REAL:    return ((Real)x).compare((Real)y) <= 0;
        }
        break;
      }
      case TEXT: {
        switch (y.type()) {
          case TEXT: return ((Text)x).compare((Text)y) <= 0;
        }
        break;
      }
    }
    throw new TypeMismatch();
  }
  
  public static final Primitive LE = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(le(vm.popArg(), vm.popArg()));
      vm.popFrame();
    }
  };

  public static boolean gt(Datum x, Datum y)
  {
    switch (x.type()) {
      case INT: {
        switch (y.type()) {
          case INT:     return ((Int)x).compare((Int)y) > 0;
          case BIGINT:  return ((Int)x).toBigInt().compare((BigInt)y) > 0;
          case RATIO:   return ((Int)x).toRatio().compare((Ratio)y) > 0;
          case REAL:    return ((Int)x).toReal().compare((Real)y) > 0;
        }
        break;
      }
      case BIGINT: {
        switch (y.type()) {
          case INT:     return ((BigInt)x).compare(((Int)y).toBigInt()) > 0;
          case BIGINT:  return ((BigInt)x).compare((BigInt)y) > 0;
          case RATIO:   return ((BigInt)x).toRatio().compare((Ratio)y) > 0;
          case REAL:    return ((BigInt)x).toReal().compare((Real)y) > 0;
        }
        break;
      }
      case RATIO: {
        switch (y.type()) {
          case INT:     return ((Ratio)x).compare(((Int)y).toRatio()) > 0;
          case BIGINT:  return ((Ratio)x).compare(((BigInt)y).toRatio()) > 0;
          case RATIO:   return ((Ratio)x).compare((Ratio)y) > 0;
          case REAL:    return ((Ratio)x).toReal().compare((Real)y) > 0;
        }
        break;
      }
      case REAL: {
        switch (y.type()) {
          case INT:     return ((Real)x).compare(((Int)y).toReal()) > 0;
          case BIGINT:  return ((Real)x).compare(((BigInt)y).toReal()) > 0;
          case RATIO:   return ((Real)x).compare(((Ratio)y).toReal()) > 0;
          case REAL:    return ((Real)x).compare((Real)y) > 0;
        }
        break;
      }
      case TEXT: {
        switch (y.type()) {
          case TEXT: return ((Text)x).compare((Text)y) > 0;
        }
        break;
      }
    }
    throw new TypeMismatch();
  }
  
  public static final Primitive GT = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(gt(vm.popArg(), vm.popArg()));
      vm.popFrame();
    }
  };

  public static boolean ge(Datum x, Datum y)
  {
    switch (x.type()) {
      case INT: {
        switch (y.type()) {
          case INT:     return ((Int)x).compare((Int)y) >= 0;
          case BIGINT:  return ((Int)x).toBigInt().compare((BigInt)y) >= 0;
          case RATIO:   return ((Int)x).toRatio().compare((Ratio)y) >= 0;
          case REAL:    return ((Int)x).toReal().compare((Real)y) >= 0;
        }
        break;
      }
      case BIGINT: {
        switch (y.type()) {
          case INT:     return ((BigInt)x).compare(((Int)y).toBigInt()) >= 0;
          case BIGINT:  return ((BigInt)x).compare((BigInt)y) >= 0;
          case RATIO:   return ((BigInt)x).toRatio().compare((Ratio)y) >= 0;
          case REAL:    return ((BigInt)x).toReal().compare((Real)y) >= 0;
        }
        break;
      }
      case RATIO: {
        switch (y.type()) {
          case INT:     return ((Ratio)x).compare(((Int)y).toRatio()) >= 0;
          case BIGINT:  return ((Ratio)x).compare(((BigInt)y).toRatio()) >= 0;
          case RATIO:   return ((Ratio)x).compare((Ratio)y) >= 0;
          case REAL:    return ((Ratio)x).toReal().compare((Real)y) >= 0;
        }
        break;
      }
      case REAL: {
        switch (y.type()) {
          case INT:     return ((Real)x).compare(((Int)y).toReal()) >= 0;
          case BIGINT:  return ((Real)x).compare(((BigInt)y).toReal()) >= 0;
          case RATIO:   return ((Real)x).compare(((Ratio)y).toReal()) >= 0;
          case REAL:    return ((Real)x).compare((Real)y) >= 0;
        }
        break;
      }
      case TEXT: {
        switch (y.type()) {
          case TEXT: return ((Text)x).compare((Text)y) >= 0;
        }
        break;
      }
    }
    throw new TypeMismatch();
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
      case BYTES  -> ((Bytes)x).length();
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
