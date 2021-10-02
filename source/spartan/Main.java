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

  private static String script;
  private static List scriptArgs = List.Empty;
  
  static
  {
    System.out.println("loading libspartan.dll...");
    System.loadLibrary("libspartan");
  }
  
  public static void main(String[] args)
  {
    parseCommandLine(args);
    
    var globals = new BaseEnv();
    globals.bind(Symbol.get("sys/args"), scriptArgs);
    
    if (script == null) {
      System.out.println(IntroMessage);
      Evaluator.evalInteractive(globals);
    }
    else
      Evaluator.evalFile(script, globals);
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
          System.err.println("error: --file option requires argument");
          System.exit(-1);
        }
        script = args[i + 1];
        gatherScriptArgs(args, i + 2);
      }
    }
  }
  
  private static void gatherScriptArgs(String[] args, int start)
  {
    for (int i = args.length - 1; i >= start; --i)
      scriptArgs = List.cons(new Text(args[i]), scriptArgs);
  }
}
