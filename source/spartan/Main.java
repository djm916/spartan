package spartan;

import picocli.CommandLine;
import picocli.CommandLine.Command;
import picocli.CommandLine.Option;
import picocli.CommandLine.Parameters;
import java.util.concurrent.Callable;
import spartan.data.List;
import spartan.data.Text;
import spartan.data.Symbol;
import spartan.runtime.GlobalEnv;
import spartan.errors.Error;

@Command(name = "Spartan",
         description = "The Spartan interpreter CLI",
         mixinStandardHelpOptions = true,
         showEndOfOptionsDelimiterInUsageHelp = true,
         sortOptions = false)
public class Main implements Callable<Integer>
{
  // Intro text displayed when entering the REPL
  private static final String ReplIntro = """
Welcome to the Spartan interactive interpreter!
Enter expressions to be evaluated.
Enter Control-D (on Linux) or Control-Z (on Windows) to exit.""";
  
  // Path to code file to be pre-loaded
  private static final String BuiltinsFilePath = "./stdlib/builtins.s";
  
  // Path to the script file to execute (or null if none given)
  @Option(names = "--file", paramLabel = "path", description = "path to script file")
  private String scriptPath;
  
  // List of arguments passed to the script (or null if none given)
  @Parameters(paramLabel = "args", description = "script arguments")
  private String[] scriptArgs;
  
  static
  {
    //System.out.println("loading libspartan.dll...");
    
    // Load "libspartan.dll", containing JNI native code dependencies
    System.loadLibrary("libspartan");
  }
  
  public static void main(String[] args) //throws java.io.IOException
  {
    System.exit(new CommandLine(new Main()).execute(args));
  }
  
  public Integer call() throws Exception
  {
    var globals = GlobalEnv.createBasis();
    
    Evaluator.loadFile(BuiltinsFilePath, globals);
    
    if (scriptPath == null) {
      System.out.println(ReplIntro);
      Evaluator.startRepl(globals);      
    }
    else {
      globals.bind(Symbol.get("sys/args"), makeArgsList());
      Evaluator.loadFile(scriptPath, globals);
    }
    
    return 0;
  }
  
  private List makeArgsList()
  {
    if (scriptArgs == null)
      return List.Empty;
    
    List args = List.Empty;
    
    for (int i = scriptArgs.length - 1; i >= 0; --i)
      args = List.cons(new Text(scriptArgs[i]), args);
    
    return args;
  }
}
