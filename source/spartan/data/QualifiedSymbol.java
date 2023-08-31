package spartan.data;

public final class QualifiedSymbol extends Symbol
{
  private final String nameSpace;   // namespace (qualified) portion
  private final String baseName;    // base (unqualified) portion
  
  /**
   * Create a new, uninterned qualified symbol
   */
  public QualifiedSymbol(String name, String nameSpace, String baseName)
  {
    super(name);
    this.nameSpace = nameSpace;
    this.baseName = baseName;
  }
    
  public String nameSpace()
  {
    return nameSpace;
  }
  
  public String baseName()
  {
    return baseName;
  }
}
