package spartan.data;

public final class QualifiedSymbol extends Symbol
{
  private final String moduleName;   // namespace (qualified) portion
  private final String baseName; // base (unqualified) portion
  
  /**
   * Create a new, uninterned qualified symbol
   */
  public QualifiedSymbol(String name, String moduleName, String baseName)
  {
    super(name);
    this.moduleName = moduleName;
    this.baseName = baseName;
  }
  
  public QualifiedSymbol(String moduleName, String baseName)
  {
    this(moduleName + ":" + baseName, moduleName, baseName);
  }
  
  public QualifiedSymbol(Symbol moduleName, Symbol baseName)
  {
    this(moduleName.name(), baseName.name());
  }
  
  public String moduleName()
  {
    return moduleName;
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
