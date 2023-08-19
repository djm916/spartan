package spartan.data;

public final class QualifiedSymbol extends Symbol
{
  private final String nameSpace;   // namespace (qualified) portion
  private final String bareName;    // bare (unqualified) portion
  
  /**
   * Create a new, uninterned qualified symbol
   */
  public QualifiedSymbol(String name, String nameSpace, String bareName)
  {
    super(name);
    this.nameSpace = nameSpace;
    this.bareName = bareName;
  }
}
