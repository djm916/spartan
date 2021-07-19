package spartan;

import spartan.errors.SyntaxError;
import spartan.errors.CompileError;
import spartan.errors.RuntimeError;
import spartan.parsing.Reader;
import spartan.parsing.SourceValue;
import spartan.compiling.Compiler;
import spartan.runtime.Inst;
import spartan.runtime.VirtualMachine;
import spartan.runtime.GlobalEnv;
import spartan.data.Value;
import spartan.data.Symbol;
import spartan.builtins.Builtins;
import java.io.IOException;

public class Main
{
  private static final GlobalEnv globals = new GlobalEnv();  
  private static final String IntroMessage =  
    "Welcome to the Spartan interpreter!\n" +
    "Enter expressions to be evaluated.\n" +
    "Enter Control-D (on Linux) or Control-Z (on Windows) to exit.";

  static
  {
    globals.bind(Symbol.get("+"), Builtins.Add);
    globals.bind(Symbol.get("-"), Builtins.Sub);
    globals.bind(Symbol.get("*"), Builtins.Mul);
    globals.bind(Symbol.get("/"), Builtins.Div);
    globals.bind(Symbol.get("="), Builtins.Eq);
    globals.bind(Symbol.get("/="), Builtins.Ne);
    globals.bind(Symbol.get("<"), Builtins.Lt);
    globals.bind(Symbol.get(">"), Builtins.Gt);
    globals.bind(Symbol.get("<="), Builtins.Le);
    globals.bind(Symbol.get(">="), Builtins.Ge);
    globals.bind(Symbol.get("~"), Builtins.Neg);
  }
  
  public static void main(String[] args) throws IOException
  {
    Reader reader = Reader.readStdin();
    Compiler compiler = new Compiler();
    VirtualMachine vm = new VirtualMachine(globals);
    
    System.out.println(IntroMessage);
    do {
      //System.out.print(">");
      try {
        SourceValue sourceValue = reader.read();
        if (sourceValue == null)
          break;
        //System.out.println("reader: " + sourceValue.value.repr());
        Inst code = compiler.compile(sourceValue);
        Value result = vm.exec(code);
        System.out.println(result.repr());
        //System.out.println("frames: " + vm.frameCount);
      }
      catch (IOException ex) {
        System.out.println("error: " + ex.getMessage());
      }
      catch (SyntaxError | CompileError | RuntimeError ex) {
        System.out.println(ex);
      }
    }
    while (true);
  }
}
