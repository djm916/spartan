package spartan;


import spartan.errors.SyntaxError;
import spartan.errors.CompileError;
import spartan.errors.RuntimeError;
import spartan.errors.Error;
import spartan.parsing.Parser;
import spartan.ast.Program;
import spartan.runtime.Inst;
import spartan.runtime.VirtualMachine;
import spartan.data.Value;
import java.io.IOException;
import java.io.FileNotFoundException;

public class Main
{
  private static final String USAGE = 
    "Usage: java -jar Spartan.jar <file> <arg> ...";
  
  public static void main(String[] args)
  {
    if (args.length < 1) {
      System.out.println(USAGE);
      return;
    }
    
    String scriptName = args[0];
    
    
    Program prog = null;;
    
    try {
      prog = Parser.parseFile(scriptName);
    }
    catch (SyntaxError ex) {
      System.out.println(ex);
      return;
    }
    catch (IOException ex) {
      System.out.println("error: " + ex.getMessage());
      return;
    }
    
    Inst code = null;
    
    try {
      code = prog.compile();
    }
    catch (Error ex) {
      System.out.println(ex);
      return;
    }
    catch (CompileError ex) {
      System.out.println(ex);
      return;
    }

    System.out.println(prog.sexp());
    
    Value result = null;
    
    try {
      result = new VirtualMachine().exec(code);
    }
    catch (RuntimeError ex) {
      System.out.println(ex);
      return;
    }
    
    System.out.println(result.repr());
  }
}
