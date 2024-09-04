package spartan.builtins;

import spartan.data.*;
import spartan.data.Package; // shadows java.lang.Package
import spartan.data.Void; // shadows java.lang.Void
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
      var pkg = new Package(pkgName, CorePackage.INSTANCE);
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
  
  // (set-current-package! package)
  
  public static final Primitive SET_CURRENT_PACKAGE = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Package pkg))
        throw new TypeMismatch();
      spartan.Runtime.currentPackage(pkg);
      vm.result = Void.VALUE;
      vm.popFrame();
    }
  };
  
  // (find-package package-name)
  
  public static final Primitive FIND = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Symbol pkgName))
        throw new TypeMismatch();
      try {
        vm.result = spartan.Runtime.getPackage(pkgName);
      }
      catch (NoSuchPackage err) {
        vm.result = Void.VALUE;
      }
      vm.popFrame();
    }
  };
  
  // (the-package package-name)
  
  public static final Primitive GET = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Symbol pkgName))
        throw new TypeMismatch();
      vm.result = spartan.Runtime.getPackage(pkgName);
      vm.popFrame();
    }
  };
      
  // (package-bind symbol value [package])
  
  public static final Primitive BIND = new Primitive(Signature.variadic(2, 1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Symbol symbol))
        throw new TypeMismatch();
      var value = vm.popArg();
      if (!((vm.args.isEmpty() ? spartan.Runtime.currentPackage() : vm.popArg()) instanceof Package pkg))
        throw new TypeMismatch();
      if (!symbol.isSimple())
        throw new InvalidArgument();
      pkg.bind(symbol, value);
      vm.result = Void.VALUE;
      vm.popFrame();
    }
  };
  
  // (package-resolve symbol [package])
  
  public static final Primitive RESOLVE = new Primitive(Signature.variadic(1, 1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Symbol symbol))
        throw new TypeMismatch();
      if (!((vm.args.isEmpty() ? spartan.Runtime.currentPackage() : vm.popArg()) instanceof Package pkg))
        throw new TypeMismatch();
      if (!symbol.isSimple())
        throw new InvalidArgument();
      vm.result = pkg.lookup(symbol);
      vm.popFrame();
    }
  };
  
  // (package-symbols [package])
  
  public static final Primitive SYMBOLS = new Primitive(Signature.variadic(0, 1)) {
    public void apply(VirtualMachine vm) {
      if (!((vm.args.isEmpty() ? spartan.Runtime.currentPackage() : vm.popArg()) instanceof Package pkg))
        throw new TypeMismatch();
      vm.result = List.of(pkg.symbols());
      vm.popFrame();
    }
  };
  
  // (package-alias package-name alias [package])
  
  public static final Primitive ALIAS = new Primitive(Signature.variadic(2, 1)) {    
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Symbol pkgName))
        throw new TypeMismatch();
      if (!(vm.popArg() instanceof Symbol alias))
        throw new TypeMismatch();      
      if (!((vm.args.isEmpty() ? spartan.Runtime.currentPackage() : vm.popArg()) instanceof Package targetPackage))
        throw new TypeMismatch();
      if (!pkgName.isSimple())
        throw new InvalidArgument();
      if (!alias.isSimple())
        throw new InvalidArgument();
      var sourcePackage = spartan.Runtime.getPackage(pkgName);
      targetPackage.addPackageAlias(alias, sourcePackage);
      vm.result = Void.VALUE;
      vm.popFrame();
    }
  };
}
