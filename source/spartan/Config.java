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

/** Contains global system configuration parameters. */
public final class Config
{
  private static final Logger log = Logger.getLogger(Config.class.getName());
  
  /** Enables or disables debug logging. Configured with the "spartan.debug" Java system property. Should
      be "true" or "false". Defaults to "false". */
  public static final boolean LOG_DEBUG = Boolean.valueOf(System.getProperty("spartan.debug", "false"));
  
  /** The default character encoding for converting to and from binary data.
      This encoding is assumed when reading source code files. */
  public static final Charset DEFAULT_ENCODING = StandardCharsets.UTF_8;
  
  /** The default decimal display precision */
  public static final int DEFAULT_DECIMAL_PRECISION = 3;
    
  public static final Path HOME_DIR = initHomeDir();
  
  /** This source file (which is pre-loaded) bootstraps the initial environment with
      a core set of built-in procedures and macros. */
  public static final Path BUILTINS_FILE_PATH = initBuiltinsPath();
  
  /** The list of directories to search when loading a file. Populated from the environment variable
      SPARTANPATH. SPARTANPATH must contain a list of directories, separated by a (system-specific) delimiter.
      If SPARTANPATH is not set, defaults to the current working directory. */
  public static final List<Path> LOAD_SEARCH_DIRS = initLoadSearchDirs();
  
  public static final RandomGenerator DEFAULT_RNG = RandomGenerator.getDefault();
  
  public static enum OSType {
    WINDOWS, UNIX, MACOS, UNKNOWN
  };
  
  public static final OSType OS_TYPE = getOSType();
    
  private Config() {}
  
  private static Path initHomeDir()
  {
    var spartanHome = System.getenv("SPARTANHOME");
    
    if (LOG_DEBUG)
      log.info(() -> String.format("SPARTANHOME is \"%s\"", spartanHome));
    
    if (spartanHome == null) {
      System.err.println("SPARTANHOME environment variable is not defined.\n" +
                         "Please set it to the base path of the Spartan installation directory.");
      System.exit(-1);
    }
    
    try {
      return Path.of(spartanHome);
    }
    catch (InvalidPathException ex) {
      throw new Error(String.format("invalid SPARTANHOME \"%s\"", spartanHome));
    }
  }
  
  private static List<Path> initLoadSearchDirs()
  {
    var spartanPath = System.getenv("SPARTANPATH");
    if (spartanPath == null)
      spartanPath = ".";
    
    var searchDirs = spartanPath.split(System.getProperty("path.separator"));
    try {
      return Stream.of(searchDirs).map(Path::of).toList();
    }
    catch (InvalidPathException ex) {
      throw new Error(String.format("invalid SPARTANPATH \"%s\"", spartanPath));
    }
  }
  
  private static Path initBuiltinsPath()
  {
    return HOME_DIR.resolve(Path.of("stdlib", "builtins.s"));
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
