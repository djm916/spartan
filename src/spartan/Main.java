package spartan;

import spartan.errors.SyntaxError;
import spartan.errors.CompileError;
import spartan.errors.RuntimeError;
import spartan.parsing.Reader;
import spartan.parsing.SourceDatum;
import spartan.compiling.Compiler;
import spartan.runtime.Inst;
import spartan.runtime.VirtualMachine;
import spartan.runtime.BaseEnv;
import spartan.data.Datum;
import spartan.data.Symbol;
import java.io.IOException;

public class Main
{
  private static final String IntroMessage =  
    "Welcome to the Spartan interpreter!\n" +
    "Enter expressions to be evaluated.\n" +
    "Enter Control-D (on Linux) or Control-Z (on Windows) to exit.";

  public static void main(String[] args) throws IOException
  {
    Reader reader = Reader.readStdin();
    Compiler compiler = new Compiler();
    VirtualMachine vm = new VirtualMachine(new BaseEnv());
    
    System.out.println(IntroMessage);
    do {
      //System.out.print(">");
      try {
        SourceDatum sourceDatum = reader.read();
        if (sourceDatum == null)
          break;
        //System.out.println("reader: " + sourceDatum.value.repr());
        Inst code = compiler.compile(sourceDatum);
        Datum result = vm.exec(code);
        System.out.println(result.repr());
        //System.out.println("frames: " + vm.frameCount);
      }
      catch (IOException ex) {
        System.out.println("error: " + ex.getMessage());
      }
      catch (SyntaxError | CompileError ex) {
        System.out.println(ex);
      }
      catch (RuntimeError ex) {
        System.out.println(ex);
        vm.reset();
      }
    }
    while (true);
  }
}
