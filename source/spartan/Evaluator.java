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
        catch (SyntaxError | CompileError | RuntimeError ex) {
          System.err.println(ex);
        }
      }
    }
  }
  
  public static void loadFile(String fileName, GlobalEnv globals)
  throws IOException
  {
    try (Reader reader = Reader.forFile(fileName)) {
      var vm = new VirtualMachine(globals);
      var compiler = new Compiler(vm);
      
      try {
        while (true) {
          var exp = reader.read();
          if (exp == null)
            return;
          vm.eval(compiler.compile(exp));
        }
      }
      catch (SyntaxError | CompileError | RuntimeError ex) {
        System.err.println(ex);
        return;
      }
    }
  }
}
