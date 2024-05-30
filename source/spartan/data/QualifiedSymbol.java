package spartan.data;

public final class QualifiedSymbol extends Symbol
{
  private final String packageName;   // package (qualified) portion
  private final String baseName;    // base (unqualified) portion
  
  /**
   * Create a new, uninterned qualified symbol
   */
  public QualifiedSymbol(String name, String packageName, String baseName)
  {
    super(name);
    this.packageName = packageName;
    this.baseName = baseName;
  }
  
  public QualifiedSymbol(String packageName, String baseName)
  {
    this(packageName + ":" + baseName, packageName, baseName);
  }
  
  public String packageName()
  {
    return packageName;
  }
  
  public String baseName()
  {
    return baseName;
  }
  
  public boolean isQualified()
  {
    return true;
  }
}
