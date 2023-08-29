package spartan.data;

public final class QualifiedSymbol extends Symbol
{
  private final String packageName;   // namespace (qualified) portion
  private final String bareName;      // bare (unqualified) portion
  
  /**
   * Create a new, uninterned qualified symbol
   */
  public QualifiedSymbol(String name, String packageName, String bareName)
  {
    super(name);
    this.packageName = packageName;
    this.bareName = bareName;
  }
    
  public String packageName()
  {
    return packageName;
  }
  
  public String bareName()
  {
    return bareName;
  }
}
