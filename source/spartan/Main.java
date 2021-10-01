package spartan;

import spartan.data.List;
import spartan.data.Text;
import spartan.data.Symbol;
import spartan.runtime.BaseEnv;

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
  
  public static void main(String[] args)
  {
    parseCommandLine(args);
    
    var globals = new BaseEnv();
    globals.bind(Symbol.get("sys/args"), sysArgs);
    
    if (inputFile == null) {
      System.out.println(IntroMessage);
      Evaluator.evalInteractive(globals);
    }
    else
      Evaluator.evalFile(inputFile, globals);
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
          sysArgs = List.cons(new Text(args[j]), sysArgs);
        break;
      }
    }
  }
}
