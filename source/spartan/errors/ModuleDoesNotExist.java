package spartan.errors;

import spartan.data.Symbol;

public class ModuleDoesNotExist extends Error
{
  private static final String MSG_FMT = "module \"%s\" does not exist";
  
  public ModuleDoesNotExist(Symbol moduleName)
  {
    this(moduleName, null);
  }
  
  public ModuleDoesNotExist(Symbol moduleName, SourceInfo source)
  {
    super(String.format(MSG_FMT, moduleName.repr()), source);
  }
}
