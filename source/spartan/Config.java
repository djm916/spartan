package spartan;

import spartan.errors.Error;
import java.nio.file.Path;
import java.nio.file.InvalidPathException;
import java.nio.charset.StandardCharsets;
import java.nio.charset.Charset;
import java.util.stream.Stream;
import java.util.List;
import java.util.random.RandomGenerator;
import java.util.logging.Logger;

/** Global system configuration parameters. */
public final class Config
{
  private static final Logger log = Logger.getLogger(Config.class.getName());
  
  /** Enables or disables debug logging. Set via the Java system property "spartan.debug-logging".
      Valid values are "true" and "false". Defaults to "false". */
  public static final boolean LOG_DEBUG = Boolean.valueOf(System.getProperty("spartan.debug-logging", "false"));
  
  /** Enables or disables tail call optimization. Valid values are "true" and "false". Defaults to "true". */
  public static final boolean TAIL_CALLS = Boolean.valueOf(System.getProperty("spartan.optimize-tail-calls", "true"));
  
  /** Enables or disables emitting bytecode listings. Valid values are "true" and "false". Defaults to "true". */
  public static final boolean EMIT_BYTECODE = Boolean.valueOf(System.getProperty("spartan.emit-bytecode", "true"));
  
  /** Enables or disables logging of macro expansion results. Valid values are "true" and "false". Defaults to "true". */
  public static final boolean SHOW_MACRO_EXPANSION = Boolean.valueOf(System.getProperty("spartan.show-macro-expansion", "true"));
  
  public static boolean ALLOW_REDEFINITION = false;
  
  /** The default character encoding for converting text to and from binary. Default is UTF-8.
      This encoding is used when reading source code files. */
  public static final Charset DEFAULT_ENCODING = StandardCharsets.UTF_8;
  
  /** The default decimal display precision. Default is 3. */
  public static final int DEFAULT_DECIMAL_PRECISION = 3;
  
  /** Home directory of the Spartan implementation. Set via the environment variable "SPARTAN_HOME".
   * If SPARTAN_HOME is not set, defaults to the current working directory.
   */
  public static final Path HOME_DIR = initHomeDir();
  
  /** The list of directories to search when loading a file. Set via the environment variable "SPARTANPATH".
   * If set, SPARTANPATH must contain a list of directories separated by a (system-specific) delimiter.
   * If SPARTANPATH is not set, defaults to the current working directory.
   */
  public static final List<Path> SEARCH_DIRS = initLoadSearchDirs();
  
  /** The system random number generator. */
  public static final RandomGenerator DEFAULT_RNG = RandomGenerator.getDefault();
  
  /** Enumeration of different operating system types. */
  public static enum OSType {
    WINDOWS, UNIX, MACOS, UNKNOWN
  };
  
  /** The current operating system type */
  public static final OSType OS_TYPE = getOSType();
  
  private Config() {}
  
  private static String getEnv(String key, String def)
  {
    var result = System.getenv(key);
    return result == null ? def : result;
  }
  
  private static Path initHomeDir()
  {
    var spartanHome = getEnv("SPARTANHOME", ".");
    
    if (LOG_DEBUG)
      log.info(() -> String.format("SPARTANHOME is \"%s\"", spartanHome));
    
    try {
      return Path.of(spartanHome);
    }
    catch (InvalidPathException ex) {
      System.err.println("warning: SPARTANHOME is invalid or not set. Defaulting to current directory.\n");
      return Path.of(".");
    }
  }
  
  private static List<Path> initLoadSearchDirs()
  {
    var spartanPath = getEnv("SPARTANPATH", ".");
    
    if (LOG_DEBUG)
      log.info(() -> String.format("SPARTANPATH is \"%s\"", spartanPath));
    
    var searchDirs = spartanPath.split(System.getProperty("path.separator"));
    try {
      return Stream.of(searchDirs).map(Path::of).toList();
    }
    catch (InvalidPathException ex) {
      System.err.println("warning: SPARTANPATH is invalid or not set. Defaulting to current directory.\n");
      return List.of(Path.of("."));
    }
  }
    
  private static OSType getOSType()
  {
    var osName = System.getProperty("os.name");
    
    if (LOG_DEBUG)
      log.info(String.format("OS is \"%s\"", osName));
    
    if (osName == null) {
      return OSType.UNKNOWN;
    }
        
    osName = osName.toLowerCase();
    
    if (osName.startsWith("win")) {
      return OSType.WINDOWS;
    }
    else {
      return OSType.UNKNOWN;
    }
  }
}
