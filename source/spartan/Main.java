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

public class Main
{
  private static final String UsageMessage = """
Usage: java -jar Spartan.jar [option...] [args...]

Options:
  --help            Display this message
  --file <path>     Execute script at <path>""";
  
  private static final String IntroMessage = """
Welcome to the Spartan interpreter!
Enter an expression. It will be evaluated and the result shown.
Enter Control-D (on Linux) or Control-Z (on Windows) to exit.""";

  private static String inputFile;
  private static List sysArgs = List.Empty;
  
  static
  {
    System.out.println("loading libspartan.dll...");
    System.loadLibrary("libspartan");
  }
  
  public static void main(String[] args) throws IOException
  {
    parseCommandLine(args);
    
    var globals = new BaseEnv();
    globals.bind(Symbol.get("sys/args"), sysArgs);
    
    if (inputFile == null)
      evalInteractive(globals);
    else
      evalFile(inputFile, globals);
  }
  
  private static void evalInteractive(GlobalEnv globals) throws IOException
  {
    System.out.println(IntroMessage);
    
    try (Reader reader = Reader.forConsole()) {
      var compiler = new Compiler();
      var vm = new VirtualMachine(globals);
      
      while (true) {
        try {
          var exp = reader.read();
          if (exp == null)
            System.exit(0);
          System.out.println("reader = " + exp.datum.repr());
          var result = vm.eval(compiler.compile(exp));
          System.out.println("eval = " + result.repr());
        }
        catch (SyntaxError | CompileError | RuntimeError ex) {
          System.err.println(ex);
        }
      }
    }
  }
  
  private static void evalFile(String fileName, GlobalEnv globals) throws IOException
  {
    try (Reader reader = Reader.forFile(fileName)) {
      var compiler = new Compiler();
      var vm = new VirtualMachine(globals);

      while (true) {
        try {
          var exp = reader.read();
          if (exp == null)
            System.exit(0);
          vm.eval(compiler.compile(exp));
        }
        catch (SyntaxError | CompileError | RuntimeError ex) {
          System.err.println(ex);
          System.exit(-1);
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
      else if (args[i].equals("--file")) {
        if (i >= args.length) {
          System.err.println("--file requires argument");
          System.exit(-1);
        }
        inputFile = args[i + 1];
      }
      else {
        for (int j = args.length - 1; j >= i; --j)
          sysArgs = new List(new Text(args[j]), sysArgs);
        break;
      }
    }
  }
}
