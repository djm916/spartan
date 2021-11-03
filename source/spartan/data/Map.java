package spartan.data;

import java.util.Set;
import java.util.stream.Stream;
import java.util.stream.Collectors;
import spartan.errors.Error;
import spartan.errors.WrongNumberArgs;
import spartan.errors.NoSuchElement;
import spartan.errors.TypeMismatch;
import spartan.builtins.CoreLib;
import spartan.data.Symbol;
import spartan.runtime.VirtualMachine;

public class Map extends Callable
{
  public static Map fromList(List elems) throws TypeMismatch
  {
    var result = new Map();
    for (; elems != List.Empty; elems = elems.cdr()) {
      if (elems.car().type() != Type.Symbol)
        throw new TypeMismatch();
      var key = (Symbol) elems.car();
      if (elems.cdr() == List.Empty)
        throw new TypeMismatch();
      elems = elems.cdr();
      var value = elems.car();
      result.members.put(key, value);
    }
    return result;
  }
  
  public final Type type()
  {
    return Type.Map;
  }
  
  public final String repr()
  {
    return members.entrySet().stream()
      .map(e -> String.format("%s %s", e.getKey().repr(), e.getValue().repr()))
      .collect(Collectors.joining(" ", "{", "}"));
  }
  
  public final Datum at(Symbol key) throws NoSuchElement
  {
    Datum value = members.get(key);
    if (value == null)
      throw new NoSuchElement();
    return value;
  }

  public final int length()
  {
    return members.size();
  }
  
  public boolean eq(Map that) throws TypeMismatch
  {
    if (this.length() != that.length())
      return false;
        
    for (Symbol key : this.members.keySet()) {
      if (!that.members.containsKey(key))
        return false;
      if (!CoreLib.eq(this.members.get(key), that.members.get(key)))
        return false;
    }
    
    return true;
  }

  public void apply(VirtualMachine vm) throws Error
  {
    if (vm.peekArg().type() != Type.Symbol)
      throw new TypeMismatch();
    vm.result = at((Symbol)vm.popArg());
    vm.popFrame();
  }
  
  private Map()
  {
    super(1, false);
    members = new java.util.IdentityHashMap<>();
  }
  
  private final java.util.Map<Symbol, Datum> members;
}
