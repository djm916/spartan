package spartan.compiling;

import spartan.data.List;
import spartan.data.Symbol;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.Supplier;

/** This class implements a lexical environment for local variables. */
class Scope
{
  static final Scope Empty = new Scope(null);
 
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
    return lookup(name, 0);
  }
  
  /** Lookup a variable in the environment represented by this scope.
      If the variable is found, returns the value of invoking ifPresent on the
      variable's index. Otherwise, returns the value of invoking getAbsent.
      @param name The variable to look up
      @param ifPresent The function to invoke if the variable was found
      @param ifAbsent The supplier to invoke if the variable was not found
      @return The value returned by ifPresent or ifAbsent
  */
  <R> R lookupOrElse(Symbol name, Function<DeBruijnIndex, R> ifPresent, Supplier<R> ifAbsent)
  {
    var maybeIndex = lookup(name);
    if (maybeIndex.isPresent())
      return ifPresent.apply(maybeIndex.get());
    else
      return ifAbsent.get();
  }
  
  private Optional<DeBruijnIndex> lookup(Symbol toFind, int depth)
  {
    int offset = vars.indexOf(toFind, (name) -> toFind.equals((Symbol)name));
    if (offset >= 0)
      return Optional.of(new DeBruijnIndex(depth, offset));
    else if (parent != null)
      return parent.lookup(toFind, depth + 1);
    else
      return Optional.empty();
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
