package spartan;


import spartan.errors.SyntaxError;
import spartan.errors.CompileError;
import spartan.errors.RuntimeError;
import spartan.errors.Error;
import spartan.parsing.Reader;
import spartan.parsing.SourceValue;
import spartan.compiling.Compiler;
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
    Reader reader = Reader.readStdin();
    Compiler compiler = new Compiler();
    VirtualMachine vm = new VirtualMachine();
    
    do {
      System.out.print(">");
      try {
        SourceValue sourceValue = reader.read();
        if (sourceValue == null)
          break;
        System.out.println("reader: " + sourceValue.value.repr());
        //Position position = sourceValue.positionMap.get(value);
        //if (position != null)
          //System.out.println(String.format("(%d %d)", position.line, position.column));
        Inst code = compiler.compile(sourceValue);
        System.out.println("code: " + code);
        Value result = vm.exec(code);
        System.out.println("result: " + result.repr());
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
