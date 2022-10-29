package spartan;

import spartan.parsing.Reader;
import spartan.compiling.Compiler;
import spartan.runtime.VirtualMachine;
import spartan.errors.Error;
import spartan.errors.LoadError;
import java.io.FileNotFoundException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.InvalidPathException;
import java.util.logging.Logger;

/** This class is responsible for locating, reading, and evaluating source code files. */
public final class Loader
{
  public static void load(Path file, GlobalEnv globals)
  {
    var path = resolvePath(file);
    
    if (Config.LOG_DEBUG)
      log.info(() -> "loading \"" + path + "\"");
    
    try (var reader = Reader.forFile(path)) {
      var vm = new VirtualMachine(globals);
      var compiler = new Compiler(vm);
      
      //TODO: Better handling of errors bubbling up from loaded file
      try {
        for (var exp : reader) {
          vm.eval(compiler.compile(exp));
        }
      }
      catch (Error err) {
        System.err.println(err);
      }
    }
    catch (FileNotFoundException ex) {
      // cannot occur in this context, as resolvePath will detect this condition
    }
  }
  
  private static Path resolvePath(Path path)
  {
    try {      
      // Absolute path doesn't require search
      if (path.isAbsolute())
        return path;
      
      // Search paths in environment variable SPARTAN_PATH
      for (var searchDir : Config.LOAD_SEARCH_DIRS) {
        var tryPath = searchDir.resolve(path);
        if (Config.LOG_DEBUG)
          log.info(() -> "Attempting to load \"" + path + "\"");
        if (Files.exists(path))
          return path;
      }
    }
    catch (InvalidPathException ex) {
      throw new LoadError(path.toString()); // The given path was invalid
    }
    
    throw new LoadError(path.toString()); // No file was found
  }
  
  private static final Logger log = Logger.getLogger(Loader.class.getName());
}
