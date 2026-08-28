package spartan.tests;

import spartan.parsing.Reader;
import spartan.compiling.Compiler;
import spartan.data.Datum;
import spartan.data.Symbol;
import spartan.data.Module;
import spartan.runtime.VirtualMachine;
import java.io.EOFException;

public abstract class TestBase
{
  public static Datum eval(String code)
  {
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
