package spartan;

import picocli.CommandLine;
import picocli.CommandLine.Command;
import picocli.CommandLine.Option;
import picocli.CommandLine.Parameters;
import java.util.concurrent.Callable;
import java.util.logging.Logger;
import spartan.data.List;
import spartan.data.Text;
import spartan.data.Symbol;
import spartan.runtime.GlobalEnv;
import spartan.errors.Error;
import spartan.errors.LoadError;

@Command(name = "Spartan",
         description = "The Spartan interpreter CLI",
         mixinStandardHelpOptions = true,
         showEndOfOptionsDelimiterInUsageHelp = true,
         sortOptions = false)

public class Main implements Callable<Integer>
{
  private static final Logger log = Logger.getLogger(Main.class.getName());
  
  // Path to the script file to execute (or null if none given)
  @Option(names = "--file", paramLabel = "path", description = "path to script file")
  private String scriptPath;
  
  // List of arguments passed to the script (or null if none given)
  @Parameters(paramLabel = "args", description = "script arguments")
  private String[] scriptArgs;
  
  public static void main(String[] args) //throws java.io.IOException
  {
    System.exit(new CommandLine(new Main()).execute(args));
  }
  
  public Integer call() throws Exception
  {
    // Load JNI Libraries
    if (Config.OS_TYPE == Config.OSType.WINDOWS) {
      try {
        Loader.loadDLL("libspartan.dll");
      }
      catch (LoadError err) {
        System.err.println(err.getMessage());
      }
    }
    
    // Bootstrap global environment
    try {
      Loader.load(Config.BUILTINS_FILE_PATH);
    }
    catch (LoadError err) {
      System.err.println(err.getMessage());
    }
    
    // Bind list of system arguments
    GlobalEnv.bind(new Symbol("sys/args"), makeArgsList());
    
    if (scriptPath == null) {
      Repl.start();      
    }
    else {
      try {
        Loader.load(scriptPath);
      }
      catch (LoadError err) {
        System.err.println(err.getMessage());
      }
    }
    
    return 0;
  }
  
  private List makeArgsList()
  {
    if (scriptArgs == null)
      return List.EMPTY;
    
    List args = List.EMPTY;
    
    for (int i = scriptArgs.length - 1; i >= 0; --i)
      args = List.cons(new Text(scriptArgs[i]), args);
    
    return args;
  }
}
