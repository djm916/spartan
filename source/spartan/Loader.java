package spartan;

import spartan.parsing.Reader;
import spartan.compiling.Compiler;
import spartan.runtime.VirtualMachine;
import spartan.errors.Error;
import spartan.errors.LoadError;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.InvalidPathException;
import java.util.logging.Logger;

/** This class is responsible for locating, reading, and evaluating source code files. */
public final class Loader
{
  public static void load(String fileName, GlobalEnv globals)
  throws IOException
  {
    var path = resolvePath(fileName);
    
    if (Config.Debug)
      log.info(() -> "loading \"" + path + "\"");
    
    try (Reader reader = Reader.forFile(path)) {
      var vm = new VirtualMachine(globals);
      var compiler = new Compiler(vm);
      
      //TODO: Better handling of errors bubbling up from loaded file
      try {
        while (true) {
          var exp = reader.read();
          if (exp == null)
            return;
          vm.eval(compiler.compile(exp));
        }
      }
      catch (Error err) {
        System.err.println(err);
      }
    }
  }
  
  private static String resolvePath(String fileName)
  {
    try {
      var givenPath = Path.of(fileName);
      
      // Absolute path doesn't require search
      if (givenPath.isAbsolute())
        return givenPath.toString();
      
      for (var searchDir : Config.LoadSearchDirs) {
        var tryPath = searchDir.resolve(givenPath);
        if (Config.Debug)
          log.info(() -> "Attempting to load \"" + tryPath + "\"");
        if (Files.exists(tryPath))
          return tryPath.toString();
      }
    }
    catch (InvalidPathException ex) {
      throw new LoadError(fileName);
    }
    
    throw new LoadError(fileName);
  }
  
  private static final Logger log = Logger.getLogger(Loader.class.getName());
}
