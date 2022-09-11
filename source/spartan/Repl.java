package spartan;

import spartan.parsing.Reader;
import spartan.compiling.Compiler;
import spartan.runtime.VirtualMachine;
import spartan.errors.Error;
import java.io.IOException;

public final class Repl
{
  public static void startRepl(GlobalEnv globals)
  throws IOException
  {
    try (Reader reader = Reader.forConsole()) {
      var vm = new VirtualMachine(globals);
      var compiler = new Compiler(vm);
            
      while (true) {
        try {
          var exp = reader.read();
          if (exp == null)
            return;
          var result = vm.eval(compiler.compile(exp));
          System.out.println(result.repr());
        }
        catch (Error err) {
          System.err.println(err);
        }
      }
    }
  }  
}
