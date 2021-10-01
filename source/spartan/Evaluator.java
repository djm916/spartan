package spartan;

import spartan.parsing.Reader;
import spartan.compiling.Compiler;
import spartan.runtime.VirtualMachine;
import spartan.runtime.GlobalEnv;
import spartan.errors.SyntaxError;
import spartan.errors.CompileError;
import spartan.errors.RuntimeError;
import java.io.IOException;

public final class Evaluator
{
  public static void evalInteractive(GlobalEnv globals)
  {
    try (Reader reader = Reader.forConsole()) {
      var compiler = new Compiler();
      var vm = new VirtualMachine(globals);
      
      while (true) {
        try {
          var exp = reader.read();
          if (exp == null)
            return;
          System.out.println("reader = " + exp.datum.repr());
          var result = vm.eval(compiler.compile(exp));
          System.out.println("eval = " + result.repr());
        }
        catch (SyntaxError | CompileError | RuntimeError ex) {
          System.err.println(ex);
        }
        catch (IOException ex) {
          System.err.println(String.format("error: %s", ex.getMessage()));
        }
      }
    }
    catch (IOException ex) {
      System.err.println(String.format("error: %s", ex.getMessage()));
    }
  }
  
  public static void evalFile(String fileName, GlobalEnv globals)
  {
    try (Reader reader = Reader.forFile(fileName)) {
      var compiler = new Compiler();
      var vm = new VirtualMachine(globals);

      while (true) {
        try {
          var exp = reader.read();
          if (exp == null)
            return;
          vm.eval(compiler.compile(exp));
        }
        catch (SyntaxError | CompileError | RuntimeError ex) {
          System.err.println(ex);
          return;
        }
        catch (IOException ex) {
          System.err.println(String.format("error: %s", ex.getMessage()));
          return;
        }
      }
    }
    catch (IOException ex) {
      System.err.println(String.format("error: %s", ex.getMessage()));
    }
  }
}
