package spartan.compiling;

import spartan.data.List;
import spartan.data.Symbol;
import java.util.Optional;

/** This class implements a lexical environment for local variables. */
class Scope
{
  static final Scope EMPTY = new Scope(null) {
    @Override
    protected DeBruijnIndex lookup(Symbol name, int depth) {
      return null;
    }
  };
  
  /** Create a new empty, scope that extends this. */
  Scope extend()
  {
    return new Scope(this, List.EMPTY);
  }
  
  /** Create a new scope that extends this with a given set of variables. */
  Scope extend(List vars)
  {
    return new Scope(this, vars);
  }
  
  /** Binds the given variable in this scope.
      @param name The variable to bind
      @return The extended scope
  */
  Scope bind(Symbol name)
  {
    return new Scope(parent, vars.append(name));
  }
  
  /** Lookup a variable in the environment represented by this scope.
      @param name The variable to look up
      @return The (optional) index of the variable
  */
  Optional<DeBruijnIndex> lookup(Symbol name)
  {
    return Optional.ofNullable(lookup(name, 0));
  }
  
  protected DeBruijnIndex lookup(Symbol name, int depth)
  {
    int offset = vars.indexOf(s -> name.isEqual((Symbol)s));
    if (offset >= 0)
      return new DeBruijnIndex(depth, offset);
    else
      return parent.lookup(name, depth + 1);
  }  

  private Scope(Scope parent)
  {
    this.parent = parent;
    this.vars = List.EMPTY;
  }
  
  /** Create a Scope with the given parent and set of variables.
      @param parent The parent scope
      @param vars The set of variables
  */
  private Scope(Scope parent, List vars)
  {
    this.parent = parent;
    this.vars = vars;
  }
    
  private final Scope parent;
  private final List vars;
}
