package spartan;

import spartan.parsing.Reader;
import spartan.compiling.Compiler;
import spartan.runtime.VirtualMachine;
import spartan.errors.Error;
import spartan.errors.IOError;
import java.io.FileNotFoundException;
import java.io.EOFException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.InvalidPathException;
import java.util.logging.Logger;
import java.util.Set;
import java.util.HashSet;

/**
 * Methods for loading source code files and native libraries.
 *
 * "Loading" includes locating, parsing, and evaluating source code files.
 * Files are located via a recursive search of the directories specified by the
 * environment variable SPARTAN_PATH.
 * Files are evaluated using a new virtual machine, distinct from the calling
 * virtual machine.
 * Definitions in the loaded file are stored in the global environment,
 * shared by all virtual machine instances.
 */
public final class Loader
{
  /**
   * Loads a file, checking for an invalid path.
   *
   * This method should be used when the path is NOT known to be a valid.
   *
   * @param file the path of the file to load
   * @throws IOError if the file cannot be loaded for any reason
   */
  public static void load(String file)
  {
    try {
      load(Path.of(file));
    }
    catch (InvalidPathException ex) {
      throw new IOError(ex);
    }
  }
  
  /**
   * Loads a file.
   *
   * This method should be used when the file is known to be a valid path.
   *
   * @param file the path of the file to load
   * @throws IOError if the file cannot be loaded for any reason
   */
  public static void load(Path file)
  {
    var path = resolvePath(file);
    
    if (loadCache.contains(path))
      return;
    
    loadCache.add(path);
    
    if (Config.LOG_DEBUG)
      log.info(() -> "loading \"" + path + "\"");
    
    try (var reader = Reader.forFile(path)) {
      var vm = new VirtualMachine();
      var compiler = new Compiler(vm);
      
      var savePackage = spartan.Runtime.currentPackage();
      
      try {        
        while (true) {
          vm.eval(compiler.compile(reader.read()));
        }
      }
      catch (EOFException ex) {
        // end of file; end loop
      }
      catch (Error err) {
        System.err.println(err);
      }
      finally {
        spartan.Runtime.currentPackage(savePackage);
      }      
    }
    catch (FileNotFoundException ex) {
      throw new IOError(ex);
    }
  }
  
  /** Load a .DLL native library on a Windows system.
   *
   * @param path path to the .DLL file to load
   * @throws IOError if the .DLL could not be loaded for any reason
   */
  public static void loadDLL(Path path)
  {
    try {
      if (Config.LOG_DEBUG)
        log.info(String.format("loading \"%s\"", path.toString()));
      System.load(path.toString());
    }
    catch (UnsatisfiedLinkError | InvalidPathException ex) {
      if (Config.LOG_DEBUG)
        log.severe(String.format("unable to load \"%s\": %s", path.toString(), ex.toString()));
      throw new IOError(ex);
    }
  }
  
  private static Path resolvePath(Path path)
  {
    // Absolute path doesn't require search of SPARTAN_PATH
    if (path.isAbsolute())
      return path.normalize();
    
    // Search paths in environment variable SPARTAN_PATH
    for (var searchDir : Config.SEARCH_DIRS) {
      var tryPath = searchDir.resolve(path);
      if (Config.LOG_DEBUG)
        log.info(() -> String.format("trying %s", tryPath));
      if (Files.exists(tryPath))
        return tryPath.normalize().toAbsolutePath();
    }
    
    throw new IOError(IOError.ErrorCode.FILE_NOT_FOUND);
  }
  
  private static final Logger log = Logger.getLogger(Loader.class.getName());
  private static final Set<Path> loadCache = new HashSet<>();
}
