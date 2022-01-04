package spartan.builtins;

import spartan.data.*;
import spartan.errors.Error;
import spartan.errors.TypeMismatch;
import spartan.runtime.VirtualMachine;

public final class CoreLib
{
  public static Bool truth(boolean x)
  {
    return x ? Bool.True : Bool.False;
  }
  
  public static boolean truth(Datum x)
  {
    return !(x == Bool.False || x == Nil.Instance);
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
    if (x.type() == y.type())
      switch (x.type()) {
        case Int   : return Int.eq((Int)x, (Int)y);
        case Ratio : return Ratio.eq((Ratio)x, (Ratio)y);
        case Real  : return Real.eq((Real)x, (Real)y);
        case Text  : return Text.eq((Text)x, (Text)y);
        case Vector: return Vector.eq((Vector)x, (Vector)y, CoreLib::eq);
        case List  : return List.eq((List)x, (List)y, CoreLib::eq);
        case Bool  :
        case Symbol:
        case Nil   : return x == y;
      }

    return false;
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
    if (x.type() == y.type())
      switch (x.type()) {
        case Int  : return Int.compare((Int)x,  (Int)y)  < 0;
        case Ratio: return Ratio.compare((Ratio)x, (Ratio)y) < 0;
        case Real : return Real.compare((Real)x, (Real)y) < 0;
        case Text : return Text.compare((Text)x, (Text)y) < 0;
      }
    
    throw new TypeMismatch();
  }
    
  public static final Primitive Lt = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(lt(vm.popArg(), vm.popArg()));
      vm.popFrame();
    }
  };

  public static boolean le(Datum x, Datum y)
  {
    if (x.type() == y.type())
      switch (x.type()) {
        case Int  : return Int.compare ((Int)x,  (Int)y)  <= 0;
        case Ratio: return Ratio.compare((Ratio)x, (Ratio)y) <= 0;
        case Real : return Real.compare((Real)x, (Real)y) <= 0;
        case Text : return Text.compare((Text)x, (Text)y) <= 0;
      }
    
    throw new TypeMismatch();
  }
  
  public static final Primitive Le = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(le(vm.popArg(), vm.popArg()));
      vm.popFrame();
    }
  };

  public static boolean gt(Datum x, Datum y)
  {
    if (x.type() == y.type())
      switch (x.type()) {
        case Int  : return Int.compare ((Int)x,  (Int)y)  > 0;
        case Ratio: return Ratio.compare((Ratio)x, (Ratio)y) > 0;
        case Real : return Real.compare((Real)x, (Real)y) > 0;
        case Text : return Text.compare((Text)x, (Text)y) > 0;
      }
    
    throw new TypeMismatch();
  }
  
  public static final Primitive Gt = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(gt(vm.popArg(), vm.popArg()));
      vm.popFrame();
    }
  };

  public static boolean ge(Datum x, Datum y)
  {
    if (x.type() == y.type())
      switch (x.type()) {
        case Int  : return Int.compare ((Int)x,  (Int)y)  >= 0;
        case Ratio: return Ratio.compare((Ratio)x, (Ratio)y) >= 0;
        case Real : return Real.compare((Real)x, (Real)y) >= 0;
        case Text : return Text.compare((Text)x, (Text)y) >= 0;
      }
    
    throw new TypeMismatch();
  }
  
  public static final Primitive Ge = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(ge(vm.popArg(), vm.popArg()));
      vm.popFrame();
    }
  };
  
  public static final Primitive MakeList = new Primitive(0, true) {
    public void apply(VirtualMachine vm) {
      vm.result = vm.popRestArgs();
      vm.popFrame();
    }
  };
  
  public static final Primitive MakeVector = new Primitive(0, true) {
    public void apply(VirtualMachine vm) {
      vm.result = Vector.fromList(vm.popRestArgs());
      vm.popFrame();
    }
  };
  
  public static final Complex makeComplex(Datum x, Datum y)
  {
    if (x.type() != Type.Real || y.type() != Type.Real)
      throw new TypeMismatch();
    return new Complex((Real)x, (Real)y);
  }
  
  public static final Primitive MakeComplex = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = makeComplex(vm.popArg(), vm.popArg());
      vm.popFrame();
    }
  };
  
  public static final Ratio makeRatio(Datum x, Datum y)
  {
    if (x.type() != Type.Int || y.type() != Type.Int)
      throw new TypeMismatch();
    return new Ratio((Int)x, (Int)y);
  }
  
  public static final Primitive MakeRatio = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      vm.result = makeRatio(vm.popArg(), vm.popArg());
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
        System.out.print(vm.popArg().repr());
      vm.result = Nil.Instance;
      vm.popFrame();
    }
  };
  
  public static final Primitive PrintLine = new Primitive(1, true) {
    public void apply(VirtualMachine vm) {
      while (!vm.args.empty())
        System.out.println(vm.popArg().repr());
      vm.result = Nil.Instance;
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
      vm.result = Symbol.get(vm.popArg().type().name);
      vm.popFrame();
    }
  };
  
  public static final Primitive Load = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.Text)
        throw new TypeMismatch();
      var fileName = ((Text) vm.popArg()).value();
      
      //TODO: Better handling of errors bubbling up from loaded file
      
      try {
        spartan.Evaluator.loadFile(fileName, vm.globals);
      }
      catch (java.io.IOException ex) {
        throw new Error(ex.getMessage());
      }
      finally {
        vm.result = Nil.Instance;
        vm.popFrame();
      }
    }
  };
  
  public static final Primitive SymbolToText = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.Symbol)
        throw new TypeMismatch();
      vm.result = new Text(((Symbol)vm.popArg()).value());
      vm.popFrame();
    }
  };
    
  public static final Primitive TextToSymbol = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.Text)
        throw new TypeMismatch();
      vm.result = Symbol.get(((Text)vm.popArg()).value());
      vm.popFrame();
    }
  };
  
  public static final Primitive IntToReal = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.Int)
        throw new TypeMismatch();
      vm.result = new Real(((Int)vm.popArg()).doubleValue());
      vm.popFrame();
    }
  };
  
  public static final Primitive RealToInt = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (vm.peekArg().type() != Type.Real)
        throw new TypeMismatch();
      vm.result = new Int(((Real)vm.popArg()).longValue());
      vm.popFrame();
    }
  };
  
  public static final Primitive GenSym = new Primitive(0, false) {
    public void apply(VirtualMachine vm) {
      vm.result = Symbol.gen();
      vm.popFrame();
    }
  };
  
  public static final Primitive IsEmptyList = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(vm.popArg() == List.Empty);
      vm.popFrame();
    }
  };
  
  public static final Primitive IsNil = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = truth(vm.popArg() == Nil.Instance);
      vm.popFrame();
    }
  };
  
  public static final Primitive IdentityHash = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      vm.result = new Int(System.identityHashCode(vm.popArg()));
      vm.popFrame();
    }
  };  
}
