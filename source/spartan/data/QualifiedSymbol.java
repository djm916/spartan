package spartan.data;

public final class QualifiedSymbol extends Symbol
{
  private final String nsName;   // namespace (qualified) portion
  private final String baseName; // base (unqualified) portion
  
  /**
   * Create a new, uninterned qualified symbol
   */
  public QualifiedSymbol(String name, String nsName, String baseName)
  {
    super(name);
    this.nsName = nsName;
    this.baseName = baseName;
  }
  
  public QualifiedSymbol(String nsName, String baseName)
  {
    this(nsName + ":" + baseName, nsName, baseName);
  }
  
  public String nameSpace()
  {
    return nsName;
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
