package spartan;

import spartan.parsing.Reader;
import spartan.compiling.Compiler;
import spartan.runtime.VirtualMachine;
import spartan.errors.Error;
import java.io.IOException;

/** This class implements the interactive REPL (Read, Eval, Print Loop) */
public final class Repl
{
  public static void start(GlobalEnv globals)
  throws IOException
  {
    System.out.println(IntroMessage);
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
  
  // Intro text displayed when entering the REPL
  private static final String IntroMessage = """
Welcome to the Spartan interactive interpreter!
Enter expressions to be evaluated.
Enter Control-D (on Linux) or Control-Z (on Windows) to exit.""";

}
