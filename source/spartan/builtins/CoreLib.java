package spartan.builtins;

import spartan.data.*;
import spartan.data.Void; // shadows java.lang.Void
import spartan.errors.Error;
import spartan.errors.TypeMismatch;
import spartan.errors.InvalidArgument;
import spartan.errors.NoSuchPackage;
import spartan.errors.WrongNumberArgs;
import spartan.errors.UnboundSymbol;
import spartan.runtime.VirtualMachine;
import spartan.Config;
import spartan.parsing.Reader;
import spartan.errors.SourceInfo;

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
  
  public static Datum macroExpand1(Datum form)
  {
    if (!(form instanceof List list && !list.isEmpty() && list.car() instanceof Symbol symbol))
      return Void.VALUE;
    var args = list.cdr();
    return spartan.Runtime.lookupMacro(symbol)
           .map(macro -> macro.expand(new VirtualMachine(), args, new SourceInfo(form, null)))
           .orElse(Void.VALUE);
  }
  
  public static final Primitive NOT = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      vm.result = not(vm.popArg());
      vm.popFrame();
    }
  };
 
  public static final Primitive EQ = new Primitive(Signature.fixed(2)) {
    public void apply(VirtualMachine vm) {
      vm.result = Bool.valueOf(isEqual(vm.popArg(), vm.popArg()));
      vm.popFrame();
    }
  };

  public static final Primitive NE = new Primitive(Signature.fixed(2)) {
    public void apply(VirtualMachine vm) {
      vm.result = Bool.valueOf(!isEqual(vm.popArg(), vm.popArg()));
      vm.popFrame();
    }
  };
  
  public static final Primitive LT = new Primitive(Signature.fixed(2)) {
    public void apply(VirtualMachine vm) {
      vm.result = Bool.valueOf(compareTo(vm.popArg(), vm.popArg()) < 0);
      vm.popFrame();
    }
  };
  
  public static final Primitive LE = new Primitive(Signature.fixed(2)) {
    public void apply(VirtualMachine vm) {
      vm.result = Bool.valueOf(compareTo(vm.popArg(), vm.popArg()) <= 0);
      vm.popFrame();
    }
  };
  
  public static final Primitive GT = new Primitive(Signature.fixed(2)) {
    public void apply(VirtualMachine vm) {
      vm.result = Bool.valueOf(compareTo(vm.popArg(), vm.popArg()) > 0);
      vm.popFrame();
    }
  };
  
  public static final Primitive GE = new Primitive(Signature.fixed(2)) {
    public void apply(VirtualMachine vm) {
      vm.result = Bool.valueOf(compareTo(vm.popArg(), vm.popArg()) >= 0);
      vm.popFrame();
    }
  };
    
  public static final Primitive APPLY = new Primitive(Signature.fixed(2)) {
    public void apply(VirtualMachine vm) {
      vm.result = vm.popArg();
      if (! (vm.popArg() instanceof List args))
        throw new TypeMismatch();
      vm.args = args;
      vm.apply(vm.args.length());
    }
  };
  
  public static final Primitive CALL_CC = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      //if (! (vm.popArg() instanceof Closure proc))
        //throw new TypeMismatch();
      vm.result = vm.popArg();
      vm.args = List.of(new Continuation(vm.frame));
      vm.apply(1);
    }
  };
  
  public static final Primitive PRINT = new Primitive(Signature.variadic(0)) {
    public void apply(VirtualMachine vm) {
      while (!vm.args.isEmpty())
        System.out.print(vm.popArg().str());
      vm.result = Void.VALUE;
      vm.popFrame();
    }
  };
  
  public static final Primitive PRINT_LINE = new Primitive(Signature.variadic(0)) {
    public void apply(VirtualMachine vm) {
      while (!vm.args.isEmpty())
        System.out.print(vm.popArg().str());
      System.out.println();
      vm.result = Void.VALUE;
      vm.popFrame();
    }
  };
  
  public static final Primitive TYPE = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      vm.result = Symbol.of(vm.popArg().type().name());
      vm.popFrame();
    }
  };
  
  public static final Primitive LOAD = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Text file))
        throw new TypeMismatch();
      try {
        spartan.Loader.load(file.str());
      }
      finally {
        vm.result = Void.VALUE;
        vm.popFrame();
      }
    }
  };
  
  public static final Primitive SYMBOL_TO_TEXT = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Symbol x))
        throw new TypeMismatch();
      vm.result = new Text(x.str());
      vm.popFrame();
    }
  };
   
  public static final Primitive TEXT_TO_SYMBOL = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Text x))
        throw new TypeMismatch();
      vm.result = Symbol.of(x.str());
      vm.popFrame();
    }
  };
  
  public static final Primitive GENSYM = new Primitive(Signature.fixed(0)) {
    public void apply(VirtualMachine vm) {
      vm.result = Symbol.generateUnique();
      vm.popFrame();
    }
  };
    
  public static final Primitive IDENTITY_HASH = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      vm.result = Int.valueOf(System.identityHashCode(vm.popArg()));
      vm.popFrame();
    }
  };

  // (string->bytes string)
  
  public static final Primitive STRING_TO_BYTES = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Text text))
        throw new TypeMismatch();
      vm.result = text.encode(Config.DEFAULT_ENCODING);
      vm.popFrame();
    }
  };
  
  public static final Primitive TEXT_TO_INT = new Primitive(Signature.fixed(1)) {
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
  
  // (bytes->string bytes start count)
  
  public static final Primitive BYTES_TO_STRING = new Primitive(Signature.fixed(3)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Bytes bytes && vm.popArg() instanceof IInt start && vm.popArg() instanceof IInt count))
        throw new TypeMismatch();
      vm.result = new Text(bytes.decode(start.intValue(), count.intValue(), Config.DEFAULT_ENCODING));
      vm.popFrame();
    }
  };
    
  public static final Primitive ERROR = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Text errMsg))
        throw new TypeMismatch();
      throw new Error(errMsg.str());
    }
  };
  
  // (apply-primitive/handler handler f args...)
  
  public static final Primitive APPLY_PRIMITIVE_WITH_HANDLER = new Primitive(Signature.variadic(2)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof IFun handler))
        throw new TypeMismatch();
      if (!(vm.popArg() instanceof Primitive f))
        throw new TypeMismatch();
      try {
        int numArgs = vm.args.length();
        if (!f.signature().matches(numArgs))
          throw new WrongNumberArgs();
        f.apply(vm);
      }
      catch (Error err) {
        vm.result = handler;
        vm.args = List.of(new Text(err.getMessage()));
        vm.apply(1);
      }
    }
  };
  
  // (format-decimal num [precision])
  
  public static final Primitive FORMAT_DECIMAL = new Primitive(Signature.variadic(1, 1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof IReal num))
        throw new TypeMismatch();
      if (!((vm.args.isEmpty() ? Int.valueOf(2) : vm.popArg()) instanceof IInt prec))
        throw new TypeMismatch();
      vm.result = new Text(num.formatDec(prec.intValue()));
      vm.popFrame();
    }
  };
  
  // (format-int num [base])
  
  public static final Primitive FORMAT_INT = new Primitive(Signature.variadic(1, 1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof IInt num))
        throw new TypeMismatch();
      if (!((vm.args.isEmpty() ? Int.valueOf(10) : vm.popArg()) instanceof IInt base))
        throw new TypeMismatch();
      vm.result = new Text(num.formatInt(base.intValue()));
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_VOID = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      vm.result = Bool.valueOf(vm.popArg() instanceof Void);
      vm.popFrame();
    }
  };
    
  public static final Primitive IS_BOOL = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      vm.result = Bool.valueOf(vm.popArg() instanceof Bool);
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_INT = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      vm.result = Bool.valueOf(vm.popArg() instanceof IInt);
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_REAL = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      vm.result = Bool.valueOf(vm.popArg() instanceof IReal);
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_COMPLEX = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      vm.result = Bool.valueOf(vm.popArg() instanceof Complex);
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_NUMBER = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      vm.result = Bool.valueOf(vm.popArg() instanceof INum);
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_SYMBOL = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      vm.result = Bool.valueOf(vm.popArg() instanceof Symbol);
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_TEXT = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      vm.result = Bool.valueOf(vm.popArg() instanceof Text);
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_CALLABLE = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      vm.result = Bool.valueOf(vm.popArg() instanceof IFun);
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_VECTOR = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      vm.result = Bool.valueOf(vm.popArg() instanceof Vector);
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_LIST = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      vm.result = Bool.valueOf(vm.popArg() instanceof List);
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_PORT = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      vm.result = Bool.valueOf(vm.popArg() instanceof Port);
      vm.popFrame();
    }
  };
  
  public static final Primitive IS_BYTES = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      vm.result = Bool.valueOf(vm.popArg() instanceof Bytes);
      vm.popFrame();
    }
  };
  
  //
  // Symbol related procedures
  //
  
  // (symbol-intern symbol)
  
  public static final Primitive SYMBOL_INTERN = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Symbol s))
        throw new TypeMismatch();
      vm.result = s.intern();
      vm.popFrame();
    }
  };
  
  // (make-symbol base-name [package-name])
  
  public static final Primitive MAKE_SYMBOL = new Primitive(Signature.variadic(1, 1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Text baseName))
        throw new TypeMismatch();
      if (!((vm.args.isEmpty() ? spartan.Runtime.currentPackage().name().name() : vm.popArg()) instanceof Text pkgName))
        throw new TypeMismatch();
      vm.result = new QualifiedSymbol(pkgName.str(), baseName.str());
      vm.popFrame();
    }
  };
  
  // (symbol-package symbol)
  
  public static final Primitive SYMBOL_PACKAGE = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Symbol s))
        throw new TypeMismatch();
      vm.result = s instanceof QualifiedSymbol qs ? new Text(qs.packageName()) : Void.VALUE;
      vm.popFrame();
    }
  };
  
  // (symbol-basename symbol)
  
  public static final Primitive SYMBOL_BASENAME = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Symbol s))
        throw new TypeMismatch();
      vm.result = s instanceof QualifiedSymbol qs ? new Text(qs.baseName()) : new Text(s.name());
      vm.popFrame();
    }
  };
  
  // (symbol-qualified? symbol)
  
  public static final Primitive SYMBOL_IS_QUALIFIED = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Symbol s))
        throw new TypeMismatch();
      vm.result = Bool.valueOf(s.isQualified());
      vm.popFrame();
    }
  };
  
  //
  // Macro expansion
  //
  
  public static final Primitive MACROEXPAND1 = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      var form = vm.popArg();
      vm.result = macroExpand1(form);
      vm.popFrame();
    }
  };
  
  public static final Primitive PRINT_PACKAGE = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Symbol pkgName))
        throw new TypeMismatch();
      var pkg = spartan.Runtime.getPackage(pkgName).orElseThrow(() -> new NoSuchPackage(pkgName));
      System.out.println(pkg.toString());
      vm.result = Void.VALUE;
      vm.popFrame();
    }
  };
  
  public static final Primitive PRINT_TRACEBACK = new Primitive(Signature.fixed(0)) {
    public void apply(VirtualMachine vm) {
      var backTrace = vm.generateBackTrace();
      if (backTrace != null && !backTrace.isEmpty()) {
        System.out.println("\nbacktrace:");
        for (spartan.parsing.Position position : backTrace)
          System.out.print(String.format("\n\t%s", position));
      }
      vm.result = Void.VALUE;
      vm.popFrame();
    }
  };
  
  // (make-record-type record-name fields)
  
  public static final Primitive MAKE_RECORD_TYPE = new Primitive(Signature.fixed(2)) {
    private void validateFields(List fields) {
      for (var elem : fields) {
        if (!(elem instanceof Symbol field))
          throw new TypeMismatch();
        if (!field.isSimple())
          throw new InvalidArgument();
      }
    }
    private Symbol fullName(Symbol baseName) {
      return new QualifiedSymbol(spartan.Runtime.currentPackage().name().name(), baseName.name()).intern();
    }
    private Symbol[] fieldArray(List fields) {
      return fields.stream().map(f -> (Symbol)f).toArray(Symbol[]::new);
    }
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Symbol name))
        throw new TypeMismatch();
      if (!(vm.popArg() instanceof List fields))
        throw new TypeMismatch();
      if (!name.isSimple())
        throw new InvalidArgument();
      validateFields(fields);
      var rtd = new RecordDescriptor(fullName(name), fieldArray(fields));
      vm.result = rtd;
      vm.popFrame();
    }
  };
  
  // (record-constructor rtd)
  
  public static final Primitive RECORD_CONSTRUCTOR = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof RecordDescriptor rtd))
        throw new TypeMismatch();
      vm.result = rtd.constructor();
      vm.popFrame();
    }
  };
  
  // (record-predicate rtd)
  
  public static final Primitive RECORD_PREDICATE = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof RecordDescriptor rtd))
        throw new TypeMismatch();
      vm.result = rtd.predicate();
      vm.popFrame();
    }
  };

  // (record-accessor rtd field-name)
  
  public static final Primitive RECORD_ACCESSOR = new Primitive(Signature.fixed(2)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof RecordDescriptor rtd))
        throw new TypeMismatch();
      if (!(vm.popArg() instanceof Symbol field))
        throw new TypeMismatch();
      vm.result = rtd.accessor(field);
      vm.popFrame();
    }
  };
  
  // (record-mutator rtd field-name)
  
  public static final Primitive RECORD_MUTATOR = new Primitive(Signature.fixed(2)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof RecordDescriptor rtd))
        throw new TypeMismatch();
      if (!(vm.popArg() instanceof Symbol field))
        throw new TypeMismatch();
      vm.result = rtd.mutator(field);
      vm.popFrame();
    }
  };
  
  // (record-destructor rtd)
  
  public static final Primitive RECORD_DESTRUCTOR = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof RecordDescriptor rtd))
        throw new TypeMismatch();
      vm.result = rtd.destructor();
      vm.popFrame();
    }
  };
}
