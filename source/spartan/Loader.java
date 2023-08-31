package spartan;

import spartan.parsing.Reader;
import spartan.compiling.Compiler;
import spartan.runtime.VirtualMachine;
import spartan.errors.Error;
import spartan.errors.LoadError;
import java.io.FileNotFoundException;
import java.io.EOFException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.InvalidPathException;
import java.util.logging.Logger;

/**
 * Loads source files.
 *
 * Loading a file includes locating, reading, and evaluating source code files.
 * Files are evaluated using a new virtual machine, distinct from the calling
 * virtual machine.
 * Definitions in the loaded file will be stored in the given global environment.
 * Normally, the global environment should be shared between virtual machine
 * instances.
 */
public final class Loader
{
  /**
   * Loads the given file.
   *
   * This method should be used when the file is not known to be a valid path.
   *
   * @param file the file to load
   * @param globals the global environment
   * @throws LoadError if the file cannot be located or does not constitute a valid path
   */
  public static void load(String file)
  {
    try {
      load(Path.of(file));
    }
    catch (InvalidPathException ex) {
      throw new LoadError(file); // Path is invalid
    }
  }
  
  /**
   * Loads the given file.
   *
   * This method should be used when the file is known to be a valid path.
   *
   * @param file the file to load
   * @param globals the global environment
   * @throws LoadError if the file cannot be located
   */
  public static void load(Path file)
  {
    var path = resolvePath(file);
    
    if (Config.LOG_DEBUG)
      log.info(() -> "loading \"" + path + "\"");
    
    try (var reader = Reader.forFile(path)) {
      var vm = new VirtualMachine();
      var compiler = new Compiler(vm);
      
      //System.out.println(String.format("in Loader.load(), before loading %s, current namespace = %s", path, spartan.Runtime.currentNS().name().repr()));
      
      var previousNS = spartan.Runtime.currentNS();
      
      try {        
        while (true) {
          vm.eval(compiler.compile(reader.read()));
        }
      }
      catch (EOFException ex) {
        // nothing to do
      }
      catch (Error err) {
        System.err.println(err);
      }
      finally {
        spartan.Runtime.currentNS(previousNS);
      }
      
      //System.out.println(String.format("in Loader.load(), after loading %s, current namespace = %s", path, spartan.Runtime.currentNS().name().repr()));
    }
    catch (FileNotFoundException ex) {
      throw new LoadError(path.toString()); // No file was found
    }
  }
  
  public static void loadDLL(String file)
  {
    try {
      var libPath = Config.HOME_DIR.resolve(file);
      if (Config.LOG_DEBUG)
        log.info(String.format("loading \"%s\"", libPath.toString()));
      System.load(libPath.toString());
    }
    catch (UnsatisfiedLinkError | InvalidPathException ex) {
      if (Config.LOG_DEBUG)
        log.severe(String.format("unable to load \"%s\": %s", file, ex.toString()));
      throw new LoadError(file);
    }
  }
  
  private static Path resolvePath(Path path)
  {
    // Absolute path doesn't require search of SPARTAN_PATH
    if (path.isAbsolute())
      return path;
    
    // Search paths in environment variable SPARTAN_PATH
    for (var searchDir : Config.LOAD_SEARCH_DIRS) {
      var tryPath = searchDir.resolve(path);
      if (Config.LOG_DEBUG)
        log.info(() -> String.format("trying %s", tryPath));
      if (Files.exists(path))
        return path;
    }
    
    throw new LoadError(path.toString()); // No file was found
  }
  
  private static final Logger log = Logger.getLogger(Loader.class.getName());
}
