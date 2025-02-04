package spartan.builtins;

import spartan.data.*;
import spartan.data.Record; // shadows java.lang.Record
import spartan.errors.Error;
import spartan.errors.TypeMismatch;
import spartan.errors.InvalidArgument;
import spartan.errors.WrongNumberArgs;
import spartan.errors.UnboundSymbol;
import spartan.errors.IOError;
import spartan.errors.NoSuchNamespace;
import java.io.IOException;
import spartan.runtime.VirtualMachine;
import spartan.Config;
import spartan.parsing.Reader;
import spartan.errors.SourceInfo;

/**
 * Contains implementations of Spartan Scheme's core builtin procedures.
 */
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
  
  //   (apply f args)
  
  public static final Primitive APPLY = new Primitive(Signature.fixed(2)) {
    public void apply(VirtualMachine vm) {
      vm.result = vm.popArg();
      if (! (vm.popArg() instanceof List args))
        throw new TypeMismatch();
      vm.args = args;
      vm.apply(vm.args.length());
    }
  };
  
  // (call/cc f)
  
  public static final Primitive CALL_CC = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      //if (! (vm.popArg() instanceof Closure proc))
        //throw new TypeMismatch();
      vm.result = vm.popArg();
      vm.args = List.of(new Kontinue(vm.kon));
      vm.apply(1);
    }
  };
  
  // (print obj...)
  
  public static final Primitive PRINT = new Primitive(Signature.variadic(0)) {
    public void apply(VirtualMachine vm) {
      while (!vm.args.isEmpty())
        System.out.print(vm.popArg().str());
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
  
  // (print-line obj...)
  
  public static final Primitive PRINT_LINE = new Primitive(Signature.variadic(0)) {
    public void apply(VirtualMachine vm) {
      while (!vm.args.isEmpty())
        System.out.print(vm.popArg().str());
      System.out.println();
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
  
  // (type obj)
  
  public static final Primitive TYPE = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      vm.result = Symbol.of(vm.popArg().type().name());
      vm.popFrame();
    }
  };
  
  // (load path)
  
  public static final Primitive LOAD = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Text path))
        throw new TypeMismatch();
      try {
        spartan.Loader.load(path.str());
      }
      finally {
        vm.result = Nil.VALUE;
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

  public static final Primitive IS_IDENTICAL = new Primitive(Signature.fixed(2)) {
    public void apply(VirtualMachine vm) {
      var x = vm.popArg();
      var y = vm.popArg();
      vm.result = Bool.valueOf(x == y);
      vm.popFrame();
    }
  };
  
  // (string->bytes string [encoding])
  
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
  
  //
  // Type predicates
  //
  
  public static final Primitive IS_NIL = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      vm.result = Bool.valueOf(vm.popArg() instanceof Nil);
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
  
  // (make-symbol base-name [ns-name])
  
  public static final Primitive MAKE_SYMBOL = new Primitive(Signature.variadic(1, 1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Text baseName))
        throw new TypeMismatch();
      if (!((vm.args.isEmpty() ? new Text(spartan.Runtime.currentNS().name().name()) : vm.popArg()) instanceof Text nsName))
        throw new TypeMismatch();
      vm.result = new QualifiedSymbol(nsName.str(), baseName.str());
      vm.popFrame();
    }    
  };
    
  // (symbol-ns symbol)
  
  public static final Primitive SYMBOL_NS = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Symbol s))
        throw new TypeMismatch();
      vm.result = s instanceof QualifiedSymbol qs ? new Text(qs.nameSpace()) : Nil.VALUE;
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
  
  public static Datum macroExpand1(Datum form)
  {
    if (!(form instanceof List list && !list.isEmpty() && list.first() instanceof Symbol symbol))
      return Nil.VALUE;
    var args = list.rest();
    return spartan.Runtime.lookupMacro(symbol)
           .map(macro -> macro.expand(new VirtualMachine(), args, new SourceInfo(form, null)))
           .orElse(Nil.VALUE);
  }
  
  public static final Primitive MACROEXPAND1 = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      var form = vm.popArg();
      vm.result = macroExpand1(form);
      vm.popFrame();
    }
  };
    
  //
  // Records
  //
  
  
  // (make-record-type name fields)
  
  public static final Primitive MAKE_RECORD_TYPE = new Primitive(Signature.fixed(2)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Symbol name))
        throw new TypeMismatch();
      if (!name.isSimple())
        throw new InvalidArgument();
      if (!(vm.popArg() instanceof List fields))
        throw new TypeMismatch();
      validateFields(fields);
      var fullName = new QualifiedSymbol(spartan.Runtime.currentNS().name().name(), name.name()).intern();
      var fieldArray = fields.streamOf(Symbol.class).toArray(Symbol[]::new);
      var type = TypeRegistry.register(fullName);
      vm.result = new RecordDescriptor(type, fullName, fieldArray);
      vm.popFrame();
    }
  };
  
  // Ensure all record fields are distinct, simple symbols
  private static void validateFields(List fields)
  {
    var fieldSet = new java.util.HashSet<Symbol>();
    for (var e : fields) {
      if (!(e instanceof Symbol f))
        throw new TypeMismatch();
      if (!f.isSimple())
        throw new InvalidArgument();
      if (fieldSet.contains(f))
        throw new InvalidArgument();
      fieldSet.add(f);
    }
  }
  
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
  
  // (record? obj)
  
  public static final Primitive IS_RECORD = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      vm.result = Bool.valueOf(vm.popArg() instanceof Record);
      vm.popFrame();
    }
  };
  
  // (record-descriptor obj)
  
  public static final Primitive GET_DESCRIPTOR = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Record(var rtd, _)))
        throw new TypeMismatch();
      vm.result = rtd;
      vm.popFrame();
    }
  };
  
  //
  // Namespace related procedures
  //
  
  // (make-ns ns-name)
  
  public static final Primitive MAKE_NS = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Symbol nsName))
        throw new TypeMismatch();
      vm.result = spartan.Runtime.createNS(nsName);
      vm.popFrame();
    }
  };
  
  // (current-ns)
  
  public static final Primitive CURRENT_NS = new Primitive(Signature.fixed(0)) {
    public void apply(VirtualMachine vm) {
      vm.result = spartan.Runtime.currentNS();
      vm.popFrame();
    }
  };
  
  // (set-current-ns! ns)
  
  public static final Primitive SET_CURRENT_NS = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Namespace ns))
        throw new TypeMismatch();
      spartan.Runtime.currentNS(ns);
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
  
  // (find-ns nsName)
  
  public static final Primitive FIND_NS = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Symbol nsName))
        throw new TypeMismatch();
      try {
        vm.result = spartan.Runtime.getNS(nsName);
      }
      catch (NoSuchNamespace err) {
        vm.result = Nil.VALUE;
      }
      vm.popFrame();
    }
  };
  
  // (the-ns nsName)
  
  public static final Primitive THE_NS = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Symbol nsName))
        throw new TypeMismatch();
      vm.result = spartan.Runtime.getNS(nsName);
      vm.popFrame();
    }
  };
  
  // (ns-bind symbol value [ns])
  
  public static final Primitive NS_BIND = new Primitive(Signature.variadic(2, 1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Symbol symbol))
        throw new TypeMismatch();
      var value = vm.popArg();
      if (!((vm.args.isEmpty() ? spartan.Runtime.currentNS() : vm.popArg()) instanceof Namespace ns))
        throw new TypeMismatch();
      if (!symbol.isSimple())
        throw new InvalidArgument();
      ns.bind(symbol, value);
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
  
  // (ns-resolve symbol [ns])
  
  public static final Primitive NS_RESOLVE = new Primitive(Signature.variadic(1, 1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Symbol symbol))
        throw new TypeMismatch();
      if (!((vm.args.isEmpty() ? spartan.Runtime.currentNS() : vm.popArg()) instanceof Namespace ns))
        throw new TypeMismatch();
      if (!symbol.isSimple())
        throw new InvalidArgument();
      vm.result = ns.lookup(symbol);
      vm.popFrame();
    }
  };
  
  // (ns-symbols [ns])
  
  public static final Primitive NS_SYMBOLS = new Primitive(Signature.variadic(0, 1)) {
    public void apply(VirtualMachine vm) {
      if (!((vm.args.isEmpty() ? spartan.Runtime.currentNS() : vm.popArg()) instanceof Namespace ns))
        throw new TypeMismatch();
      vm.result = List.of(ns.symbols());
      vm.popFrame();
    }
  };
  
  // (ns-alias ns-name alias [ns])
  
  public static final Primitive NS_ALIAS = new Primitive(Signature.variadic(2, 1)) {    
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Symbol nsName))
        throw new TypeMismatch();
      if (!(vm.popArg() instanceof Symbol alias))
        throw new TypeMismatch();      
      if (!((vm.args.isEmpty() ? spartan.Runtime.currentNS() : vm.popArg()) instanceof Namespace toNS))
        throw new TypeMismatch();
      if (!nsName.isSimple())
        throw new InvalidArgument();
      if (!alias.isSimple())
        throw new InvalidArgument();
      toNS.addAlias(alias, spartan.Runtime.getNS(nsName));
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
  
  // (current-time-ms)
  
  public static final Primitive CURRENT_TIME_MS = new Primitive(Signature.fixed(0)) {
    public void apply(VirtualMachine vm) {
      vm.result = Int.valueOf(System.currentTimeMillis());
      vm.popFrame();
    }
  };
}
