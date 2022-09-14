package spartan.compiling;

import spartan.data.List;
import spartan.data.Symbol;

/** This class implements a lexical environment for local variables. */
class Scope
{
  /** Create a Scope with the given parent and an empty set of variables.
      @param parent The parent scope
  */
  Scope(Scope parent)
  {
    this.parent = parent;
    this.names = List.Empty;
  }
  
  /** Create a Scope with the given parent and set of variables.
      @param parent The parent scope
      @param names The set of variables
  */
  Scope(Scope parent, List names)
  {
    this.parent = parent;
    this.names = names;
  }
  
  /** Extend this scope with a new variable binding. The extended scope
      includes the given variable as well as all variables previously
      bound by this scope and all its ancestors.
      @param name The variable to bind
      @return The extended scope
  */
  Scope bind(Symbol name)
  {
    return new Scope(parent, names.append(name));
  }
  
  /** Lookup a variable in the environment: this scope and all its ancestors.
      @param name The variable to look up
      @return The index of the variable, or null if not found.
  */
  DeBruijnIndex lookup(Symbol name)
  {
    return lookup(this, name, 0);
  }
  
  private static DeBruijnIndex lookup(Scope scope, Symbol name, int depth)
  {
    int offset = offsetOf(scope.names, name, 0);
    if (offset >= 0)
      return new DeBruijnIndex(depth, offset);
    else if (scope.parent != null)
      return lookup(scope.parent, name, depth + 1);
    else
      return null;
  }
  
  private static int offsetOf(List names, Symbol name, int offset)
  {
    if (names == List.Empty)
      return -1;
    else if (((Symbol)names.car()).equals(name))
      return offset;
    else
      return offsetOf(names.cdr(), name, offset + 1);
  }

  private final Scope parent;
  private final List names;
}
