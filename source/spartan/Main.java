package spartan;

import picocli.CommandLine;
import picocli.CommandLine.Command;
import picocli.CommandLine.Option;
import picocli.CommandLine.Parameters;
import picocli.CommandLine.ITypeConverter;
import java.util.concurrent.Callable;
import java.util.logging.Logger;
import java.nio.file.Path;
import spartan.data.List;
import spartan.data.Text;
import spartan.data.Symbol;
import spartan.errors.Error;
import spartan.errors.LoadError;

@Command(name = "spartan",
         description = "Invoke the Spartan interpreter.",
         mixinStandardHelpOptions = true,
         showEndOfOptionsDelimiterInUsageHelp = true,
         sortOptions = false)

public class Main implements Callable<Integer>
{
  private static class TextConverter implements ITypeConverter<Text> {
    public Text convert(String arg) {
      return new Text(arg);
    }
  };
  
  private static final Logger log = Logger.getLogger(Main.class.getName());
  
  // Optional script to execute
  @Parameters(index = "0", arity = "0..1", paramLabel = "script", description = "script path")
  private String scriptPath;
  
  // List of arguments passed to the script (or null if none given)
  @Parameters(index = "1", arity = "0..*", paramLabel = "args", description = "script arguments",
    converter = TextConverter.class)
  private Text[] scriptArgs = new Text[0];
  
  public static void main(String[] args)
  {
    System.exit(new CommandLine(new Main()).execute(args));
  }
  
  public Integer call() throws Exception
  {    
    // Load JNI Libraries
    if (Config.OS_TYPE == Config.OSType.WINDOWS) {
      try {
        Loader.loadDLL(Config.HOME_DIR.resolve(Path.of("libspartan.dll")));
      }
      catch (LoadError err) {
        System.err.println(err.getMessage());
      }
    }
    else {
      // TODO: Load JNI libraries on other OS than Windows
    }

    // Bootstrap global environment with built-in definitions and procedures
    try {
      spartan.Runtime.boot();
    }
    catch (LoadError err) {
      System.err.println(err.getMessage());
    }
    
    // 
    if (scriptPath != null) {
      // Bind symbol containing the list of script arguments, as string/text values
      spartan.Runtime.currentPackage().bind(Symbol.of("*command-line-args*"), List.of(scriptArgs));
      
      try {
        Loader.load(scriptPath);
      }
      catch (LoadError err) {
        System.err.println(err.getMessage());
      }
    }
    else {
      Repl.start();
    }

    return 0; // Successful exit
  }
}
