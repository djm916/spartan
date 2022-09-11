package spartan;

import spartan.errors.Error;
import java.text.NumberFormat;
import java.nio.file.Path;
import java.nio.file.InvalidPathException;
import java.util.stream.Stream;
import java.util.List;
import java.util.logging.Logger;

public final class Config
{
  public static final String DefaultEncoding = "UTF-8";
  public static final int DefaultDecimalPrecision = 3;
  public static final NumberFormat NumberFormatter;
  public static final String BuiltinsFilePath = "stdlib/builtins.s";
  public static final List<Path> LoadPaths;
  //public static final Path HomePath;
  public static final boolean Debug;  
  private static final Logger log = Logger.getLogger(Config.class.getName());
  
  static {
    NumberFormatter = NumberFormat.getNumberInstance();
    NumberFormatter.setMinimumFractionDigits(DefaultDecimalPrecision);
    NumberFormatter.setMaximumFractionDigits(DefaultDecimalPrecision);    
    Debug = Boolean.valueOf(System.getProperty("spartan.debug", "false"));
    LoadPaths = getLoadPaths();
  }
  
  private static List<Path> getLoadPaths()
  {
    final var spartanPath = getSpartanPath();
    if (Debug)
      log.info(() -> "SPARTANPATH = " + spartanPath);
    var paths = spartanPath.split(System.getProperty("path.separator"));
    try {
      return Stream.of(paths).map(Path::of).toList();
    }
    catch (InvalidPathException ex) {
      throw new Error(String.format("invalid SPARTANPATH \"%s\"", spartanPath));
    }
  }
  
  private static String getSpartanPath()
  {
    final var path = System.getenv("SPARTANPATH");
    return path == null ? "." : path;
  }
}
