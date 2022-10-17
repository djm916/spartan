package spartan;

import picocli.CommandLine;
import picocli.CommandLine.Command;
import picocli.CommandLine.Option;
import picocli.CommandLine.Parameters;
import java.util.concurrent.Callable;
import java.nio.file.Path;
import spartan.data.List;
import spartan.data.Text;
import spartan.data.Symbol;
import spartan.errors.Error;

@Command(name = "Spartan",
         description = "The Spartan interpreter CLI",
         mixinStandardHelpOptions = true,
         showEndOfOptionsDelimiterInUsageHelp = true,
         sortOptions = false)

public class Main implements Callable<Integer>
{  
  // Path to the script file to execute (or null if none given)
  @Option(names = "--file", paramLabel = "path", description = "path to script file")
  private String scriptPath;
  
  // List of arguments passed to the script (or null if none given)
  @Parameters(paramLabel = "args", description = "script arguments")
  private String[] scriptArgs;
  
  static
  {
    //System.getProperties().list(System.out);
    // Load JNI native code libraries
    var spartanHome = System.getenv("SPARTANHOME");
    var osName = System.getProperty("os.name").toLowerCase();
    if (osName.startsWith("win"))
      System.load(Path.of(spartanHome, "libspartan.dll").toString());
  }
  
  public static void main(String[] args) //throws java.io.IOException
  {
    System.exit(new CommandLine(new Main()).execute(args));
  }
  
  public Integer call() throws Exception
  {
    var globals = GlobalEnv.createBasis();
    
    Loader.load(Config.BUILTINS_FILE_PATH, globals);
    
    globals.bind(new Symbol("sys/args"), makeArgsList());
    
    if (scriptPath == null) {
      Repl.start(globals);      
    }
    else {      
      Loader.load(scriptPath, globals);
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
