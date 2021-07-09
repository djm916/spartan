package spartan;


import spartan.errors.SyntaxError;
import spartan.errors.CompileError;
import spartan.errors.RuntimeError;
import spartan.errors.Error;
import spartan.parsing.Reader;
import spartan.parsing.SourceValue;
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
    //VirtualMachine vm = new VirtualMachine();
    
    do {
      System.out.print(">");
      try {
        SourceValue sourceValue = reader.read();
        if (sourceValue == null)
          break;
        Value value = sourceValue.value;
        System.out.println(value.repr());
        Position position = sourceValue.positionMap.get(value);
        if (position != null)
          System.out.println(String.format("(%d %d)", position.line, position.column));
        //exp.analyze(null);
        //Inst code = exp.compile(false, null);
        //Value result = vm.exec(code);
        //System.out.println(result.repr());
      }
      catch (IOException ex) {
        System.out.println("error: " + ex.getMessage());
      }
      catch (SyntaxError ex) {
        System.out.println(ex);
      }
/*
      catch (CompileError ex) {
        System.out.println(ex);
      }
      catch (RuntimeError ex) {
        System.out.println(ex);
      }
*/
    }
    while (true);
  }
}
