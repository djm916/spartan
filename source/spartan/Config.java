package spartan;

import spartan.errors.Error;
import java.text.NumberFormat;
import java.nio.file.Path;
import java.nio.file.InvalidPathException;
import java.nio.charset.StandardCharsets;
import java.nio.charset.Charset;
import java.util.stream.Stream;
import java.util.List;
import java.util.random.RandomGenerator;

/** Contains global system configuration parameters. */
public final class Config
{
  /** The default character encoding for converting to and from binary data.
      This encoding is assumed when reading source code files. */
  public static final Charset DEFAULT_ENCODING = StandardCharsets.UTF_8;
  
  /** The default decimal display precision */
  public static final int DEFAULT_DECIMAL_PRECISION = 3;
  
  /** Global NumberFormatter instance */
  public static final NumberFormat NUMERIC_FORMATTER = NumberFormat.getNumberInstance();
  
  /** This source file (which is pre-loaded) bootstraps the initial environment with
      a core set of built-in procedures and macros. */
  public static final String BUILTINS_FILE_PATH = "stdlib/builtins.s";
  
  /** The list of directories to search when loading a file. Populated from the environment variable
      SPARTANPATH. SPARTANPATH must contain a list of directories, separated by a (system-specific) delimiter.
      If SPARTANPATH is not set, defaults to the current working directory. */
  public static final List<Path> LOAD_SEARCH_DIRS = initLoadSearchDirs();
  
  /** Enables or disables debug logging. Configured with the "spartan.debug" Java system property. Should
      be "true" or "false". Defaults to "false". */
  public static final boolean LOG_DEBUG = Boolean.valueOf(System.getProperty("spartan.debug", "false"));
  
  public static final RandomGenerator DEFAULT_RNG = RandomGenerator.getDefault();
  
  static {
    NUMERIC_FORMATTER.setMinimumFractionDigits(DEFAULT_DECIMAL_PRECISION);
    NUMERIC_FORMATTER.setMaximumFractionDigits(DEFAULT_DECIMAL_PRECISION);    
  }
  
  private Config() {}
  
  private static List<Path> initLoadSearchDirs()
  {
    var spartanPath = getSpartanPath();
    var dirs = spartanPath.split(System.getProperty("path.separator"));
    try {
      return Stream.of(dirs).map(Path::of).toList();
    }
    catch (InvalidPathException ex) {
      throw new Error(String.format("invalid SPARTANPATH \"%s\"", spartanPath));
    }
  }
  
  private static String getSpartanPath()
  {
    final var spartanPath = System.getenv("SPARTANPATH");
    return spartanPath == null ? "." : spartanPath;
  }
}
