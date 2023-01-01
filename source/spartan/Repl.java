package spartan;

import spartan.parsing.Reader;
import spartan.compiling.Compiler;
import spartan.runtime.VirtualMachine;
import spartan.errors.Error;

/** This class implements the interactive REPL (Read, Eval, Print Loop) */
public final class Repl
{
  public static void start()
  {
    System.out.println(INTRO_MESSAGE);
    
    try (Reader reader = Reader.forConsole()) {
      var vm = new VirtualMachine();
      var compiler = new Compiler(vm);
      
      do {
        try {
          var exp = reader.read();
          if (exp == null)
            break;        
          System.out.println(vm.eval(compiler.compile(exp)).repr());
        }
        catch (Error err) {
          System.err.println(err);
        }
      }
      while (true);
    }
  }
  
  // Intro text displayed when entering the REPL
  private static final String INTRO_MESSAGE = """
Welcome to the Spartan interactive interpreter!
Enter expressions to be evaluated.
Enter Control-D (on Linux) or Control-Z (on Windows) to exit.""";

}
