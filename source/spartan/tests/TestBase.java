package spartan.tests;

import spartan.parsing.Reader;
import spartan.compiling.Compiler;
import spartan.data.Datum;
import spartan.data.Symbol;
import spartan.data.Namespace;
import spartan.runtime.VirtualMachine;
import java.io.EOFException;

public abstract class TestBase
{
  public static Datum eval(String code)
  {
    // Set up a namespace "testbed" as the current one
    //var testNS = new Namespace(Symbol.of("testbed"), spartan.builtins.CoreNS.INSTANCE);
    //spartan.Runtime.add(testPkg);
    //spartan.Runtime.currentPackage(testPkg);
    spartan.Runtime.enterNS(Symbol.of("testbed"));
    // Evaluate each top-level form in the source code
    try (Reader r = Reader.forString(code)) {
      var vm = new VirtualMachine();
      var compiler = new Compiler(vm);
      try {
        while (true) {
          vm.eval(compiler.compile(r.read()));
        }
      }
      catch (EOFException ex) {
        // end of file; end loop
      }
      return vm.result;
    }
  }
}
