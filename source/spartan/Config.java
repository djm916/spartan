package spartan;

import spartan.errors.Error;
import java.text.NumberFormat;
import java.nio.file.Path;
import java.nio.file.InvalidPathException;
import java.util.stream.Stream;
import java.util.List;

/** Contains global system configuration parameters. */
public final class Config
{
  /** The default character encoding for converting to and from binary data.
      This encoding is assumed when reading source code files. */
  public static final String DefaultEncoding = "UTF-8";
  
  /** The default decimal display precision */
  public static final int DefaultDecimalPrecision = 3;
  
  /** Global NumberFormatter instance */
  public static final NumberFormat NumberFormatter = NumberFormat.getNumberInstance();
  
  /** This source file (which is pre-loaded) bootstraps the initial environment with
      a core set of built-in procedures and macros. */
  public static final String BuiltinsFilePath = "stdlib/builtins.s";
  
  /** The list of directories to search when loading a file. Populated from the environment variable
      SPARTANPATH. SPARTANPATH must contain a list of directories, separated by a (system-specific) delimiter.
      If SPARTANPATH is not set, defaults to the current working directory. */
  public static final List<Path> LoadSearchDirs = initLoadSearchDirs();
  
  /** Enables or disables debug logging. Configured with the "spartan.debug" Java system property. Should
      be "true" or "false". Defaults to "false". */
  public static final boolean Debug = Boolean.valueOf(System.getProperty("spartan.debug", "false"));
  
  static {
    NumberFormatter.setMinimumFractionDigits(DefaultDecimalPrecision);
    NumberFormatter.setMaximumFractionDigits(DefaultDecimalPrecision);    
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
