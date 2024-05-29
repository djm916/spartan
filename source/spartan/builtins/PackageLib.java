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
  // (make-package package-name)
  
  public static final Primitive MAKE = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Symbol pkgName))
        throw new TypeMismatch();
      var pkg = new spartan.data.Package(pkgName, CorePackage.INSTANCE);
      spartan.Runtime.addPackage(pkg);
      vm.result = pkg;
      vm.popFrame();
    }
  };
  
  // (current-package)
  
  public static final Primitive CURRENT_PACKAGE = new Primitive(Signature.fixed(0)) {
    public void apply(VirtualMachine vm) {
      vm.result = spartan.Runtime.currentPackage();
      vm.popFrame();
    }
  };
  
  // (find-package package-name)
  
  public static final Primitive FIND = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Symbol pkgName))
        throw new TypeMismatch();
      vm.result = spartan.Runtime.getPackage(pkgName).orElseThrow(() -> new NoSuchPackage(pkgName));
      vm.popFrame();
    }
  };
  
  // (package-exists? package-name)
  
  public static final Primitive EXISTS = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Symbol pkgName))
        throw new TypeMismatch();
      vm.result = Bool.valueOf(spartan.Runtime.getPackage(pkgName).isPresent());
      vm.popFrame();
    }
  };
  
  public static final Primitive TRY_FIND_PACKAGE = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Symbol pkgName))
        throw new TypeMismatch();
      var pkg = spartan.Runtime.getPackage(pkgName);
      vm.result = pkg.isPresent() ? pkg.get() : Nil.VALUE;
      vm.popFrame();
    }
  };
  
  // (package-bind package symbol value)
  // (package-bind symbol value [package])
  
  public static final Primitive BIND = new Primitive(Signature.fixed(3)) {
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
  
  // (package-resolve package symbol)
  // (package-resolve symbol [package])
  
  public static final Primitive RESOLVE = new Primitive(Signature.fixed(2)) {
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
  
  // (package-symbols [package])
  
  public static final Primitive SYMBOLS = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof spartan.data.Package pkg))
        throw new TypeMismatch();
      vm.result = List.of(pkg.symbols());
      vm.popFrame();
    }
  };
  
  // (package-alias target-package source-package alias)
  // (package-alias package-name alias [package])
  
  public static final Primitive ADD_ALIAS = new Primitive(Signature.variadic(2, 1)) {    
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Symbol pkgName))
        throw new TypeMismatch();
      if (!(vm.popArg() instanceof Symbol alias))
        throw new TypeMismatch();      
      if (!((vm.args.isEmpty() ? spartan.Runtime.currentPackage() : vm.popArg()) instanceof spartan.data.Package targetPackage))
        throw new TypeMismatch();
      if (!pkgName.isSimple())
        throw new InvalidArgument();
      if (!alias.isSimple())
        throw new InvalidArgument();
      var sourcePackage = spartan.Runtime.getPackage(pkgName).orElseThrow(() -> new NoSuchPackage(pkgName));
      targetPackage.addPackageAlias(alias, sourcePackage);
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
}
