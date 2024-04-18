package spartan.builtins;

import spartan.data.*;
import spartan.errors.TypeMismatch;
import spartan.errors.InvalidArgument;
import spartan.errors.NoSuchPackage;
import spartan.errors.MultipleDefinition;
import spartan.errors.UnboundSymbol;
import spartan.runtime.VirtualMachine;

public final class PackageLib
{  
  public static final Primitive CURRENT_PACKAGE = new Primitive(0, false) {
    public void apply(VirtualMachine vm) {
      vm.result = spartan.Runtime.currentPackage();
      vm.popFrame();
    }
  };
  
  public static final Primitive SET_CURRENT_PACKAGE = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof spartan.data.Package pkg))
        throw new TypeMismatch();
      spartan.Runtime.currentPackage(pkg);
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
  
  public static final Primitive MAKE_PACKAGE = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Symbol pkgName))
        throw new TypeMismatch();
      var pkg = new spartan.data.Package(pkgName, CorePackage.getInstance());
      spartan.Runtime.addPackage(pkg);
      vm.result = pkg;
      vm.popFrame();
    }
  };
  
  public static final Primitive FIND_PACKAGE = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Symbol pkgName))
        throw new TypeMismatch();
      vm.result = spartan.Runtime.getPackage(pkgName).orElseThrow(() -> new NoSuchPackage(pkgName));
      vm.popFrame();
    }
  };
  
  public static final Primitive TRY_FIND_PACKAGE = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Symbol pkgName))
        throw new TypeMismatch();
      var pkg = spartan.Runtime.getPackage(pkgName);
      vm.result = pkg.isPresent() ? pkg.get() : Nil.VALUE;
      vm.popFrame();
    }
  };
  
  public static final Primitive BIND = new Primitive(3, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof spartan.data.Package pkg))
        throw new TypeMismatch();
      if (!(vm.popArg() instanceof Symbol symbol))
        throw new TypeMismatch();
      if (!symbol.isSimple())
        throw new InvalidArgument();
      var value = vm.popArg();
      pkg.bind(symbol, value, () -> new MultipleDefinition(symbol));
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
  
  public static final Primitive RESOLVE = new Primitive(2, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof spartan.data.Package pkg))
        throw new TypeMismatch();
      if (!(vm.popArg() instanceof Symbol symbol))
        throw new TypeMismatch();
      if (!symbol.isSimple())
        throw new InvalidArgument();
      vm.result = pkg.lookup(symbol).orElseThrow(() -> new UnboundSymbol(pkg.name(), symbol));
      vm.popFrame();
    }
  };
  
  public static final Primitive BOUND_SYMBOLS = new Primitive(1, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof spartan.data.Package pkg))
        throw new TypeMismatch();
      vm.result = List.of(pkg.symbols());
      vm.popFrame();
    }
  };
      
  public static final Primitive ADD_LOCAL_ALIAS = new Primitive(3, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof spartan.data.Package targetPackage))
        throw new TypeMismatch();
      if (!(vm.popArg() instanceof spartan.data.Package sourcePackage))
        throw new TypeMismatch();
      if (!(vm.popArg() instanceof Symbol alias))
        throw new TypeMismatch();
      if (!alias.isSimple())
        throw new InvalidArgument();
      targetPackage.addPackageAlias(alias, sourcePackage);
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
  
  public static final Primitive IMPORT = new Primitive(4, false) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof spartan.data.Package targetPackage))
        throw new TypeMismatch();
      if (!(vm.popArg() instanceof spartan.data.Package sourcePackage))
        throw new TypeMismatch();
      if (!(vm.popArg() instanceof Symbol symbol))
        throw new TypeMismatch();
      if (!symbol.isSimple())
        throw new InvalidArgument();
      if (!(vm.popArg() instanceof Symbol alias))
        throw new TypeMismatch();
      if (!alias.isSimple())
        throw new InvalidArgument();
      targetPackage.doImport(sourcePackage, symbol, alias);
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
}
