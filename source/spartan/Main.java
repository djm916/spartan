package spartan;

import spartan.data.List;
import spartan.data.Text;
import spartan.data.Symbol;
import spartan.parsing.Reader;
import spartan.compiling.Compiler;
import spartan.runtime.GlobalEnv;
import spartan.runtime.BaseEnv;
import spartan.runtime.VirtualMachine;
import spartan.errors.SyntaxError;
import spartan.errors.CompileError;
import spartan.errors.RuntimeError;
import java.io.IOException;
import java.io.EOFException;

public class Main
{
  private static final String UsageMessage =
    "Usage: java -jar Spartan.jar [option...] [file] [args...]\n\n" +
    "Options:\n" +
    "  --help        Display this message and exit immediately";
  
  private static final String IntroMessage =  
    "Welcome to the Spartan interpreter!\n" +
    "Enter expressions to be evaluated.\n" +
    "Enter Control-D (on Linux) or Control-Z (on Windows) to exit.";
  
  private static String inputFile;
  private static List sysArgs = List.Empty;
  private static int returnCode;
  
  public static void main(String[] args)
  {
    parseCommandLine(args);
    
    GlobalEnv globals = new BaseEnv();
    globals.bind(Symbol.get("sys/args"), sysArgs);
    
    try {
      if (inputFile == null)
        returnCode = evalInteractive(globals);
      else
        returnCode = evalFile(inputFile, globals);
      System.exit(returnCode);
    }
    catch (IOException ex) {
      System.err.println("error: " + ex.getMessage());
      System.exit(-1);
    }
  }
  
  private static int evalInteractive(GlobalEnv globals) throws IOException
  {
    System.out.println(IntroMessage);
    
    try (Reader reader = Reader.forConsole()) {
      Compiler compiler = new Compiler();
      VirtualMachine vm = new VirtualMachine(globals);
      
      while (true) {
        try {
          System.out.println(vm.eval(compiler.compile(reader.read())).repr());
        }
        catch (EOFException ex) {
          return 0;
        }
        catch (SyntaxError | CompileError | RuntimeError ex) {
          System.err.println(ex);
        }
      }
    }
  }
  
  private static int evalFile(String fileName, GlobalEnv globals) throws IOException
  {
    try (Reader reader = Reader.forFile(fileName)) {
      Compiler compiler = new Compiler();
      VirtualMachine vm = new VirtualMachine(globals);

      while (true) {
        try {
          vm.eval(compiler.compile(reader.read()));
        }
        catch (EOFException ex) {
          return 0;
        }
        catch (SyntaxError | CompileError | RuntimeError ex) {
          System.err.println(ex);
          return -1;
        }
      }
    }
  }
  
  private static void parseCommandLine(String[] args)
  {
    for (int i = 0; i < args.length; ++i) {
      if (args[i].equals("--help")) {
        System.out.println(UsageMessage);
        System.exit(0);
      }
      else {
        inputFile = args[i];
        for (int j = args.length - 1; j > i; --j)
          sysArgs = new List(new Text(args[j]), sysArgs);
        return;
      }
    }
  }
}
