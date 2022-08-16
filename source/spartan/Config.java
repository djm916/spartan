package spartan;

import java.text.NumberFormat;

public final class Config
{
  public static final String DefaultEncoding = "UTF-8";
  
  public static final int DefaultDecimalPrecision = 3;
  public static final NumberFormat NumberFormatter = NumberFormat.getNumberInstance();
  
  static {
    NumberFormatter.setMinimumFractionDigits(DefaultDecimalPrecision);
    NumberFormatter.setMaximumFractionDigits(DefaultDecimalPrecision);
  }
}
