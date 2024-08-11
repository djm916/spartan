package spartan;

import spartan.parsing.Reader;
import java.io.EOFException;
import spartan.compiling.Compiler;
import spartan.runtime.VirtualMachine;
import spartan.errors.Error;

/** Interactive REPL (Read, Eval, Print Loop) */
public final class Repl
{
  public static void start()
  {
    System.out.println(INTRO_MESSAGE);
    
    try (Reader reader = Reader.forConsole()) {
      var vm = new VirtualMachine();
      var compiler = new Compiler(vm);
      
      while (true) {
        try {
          System.out.print(String.format("%s>", spartan.Runtime.currentPackage().name().repr()));
          System.out.println(vm.eval(compiler.compile(reader.read())).repr());
        }
        catch (EOFException ex) {
          break;
        }
        catch (Error err) {
          System.err.println(err);
        }
      }
    }
  }
  
  // Intro text displayed when entering the REPL
  private static final String INTRO_MESSAGE = """
Welcome to the Spartan interactive interpreter!
Enter expressions to be evaluated.
Enter Control-D (on Linux) or Control-Z (on Windows) to exit.""";

}
