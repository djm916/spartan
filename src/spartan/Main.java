package spartan;


import spartan.errors.SyntaxError;
import spartan.errors.CompileError;
import spartan.errors.RuntimeError;
import spartan.errors.Error;
import spartan.ast.*;
import spartan.parsing.Parser;
import spartan.runtime.Inst;
import spartan.runtime.VirtualMachine;
import spartan.data.Value;
import java.io.IOException;
import java.io.FileNotFoundException;

public class Main
{
  private static final String USAGE = 
    "Usage: java -jar Spartan.jar";
  
  public static void main(String[] args) throws IOException
  {
    Parser parser = Parser.parseStdin();
    VirtualMachine vm = new VirtualMachine();
    
    do {
      System.out.print(">");
      try {
        Expr exp = parser.next();
        if (exp == null)
          break;
        System.out.println(exp.sexp());
        exp.analyze(null);
        Inst code = exp.compile(false, null);
        Value result = vm.exec(code);
        System.out.println(result.repr());
      }
      catch (IOException ex) {
        System.out.println("error: " + ex.getMessage());
      }
      catch (SyntaxError ex) {
        System.out.println(ex);
      }
      catch (CompileError ex) {
        System.out.println(ex);
      }
      catch (RuntimeError ex) {
        System.out.println(ex);
      }
    }
    while (true);
  }
}
