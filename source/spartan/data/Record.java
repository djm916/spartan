package spartan.data;

import java.util.Set;
import java.util.Map;
import java.util.IdentityHashMap;
import java.util.stream.Stream;
import java.util.stream.Collectors;
import spartan.errors.Error;
import spartan.errors.WrongNumberArgs;
import spartan.errors.NoSuchElement;
import spartan.errors.TypeMismatch;
import spartan.builtins.Core;
import spartan.data.Symbol;
import spartan.runtime.VirtualMachine;

public class Record extends Datum implements Callable
{
  public static Record fromList(List elems) throws TypeMismatch
  {
    Record result = new Record();
    for (; elems != List.Empty; elems = elems.cdr()) {
      if (elems.car().type() != Type.Symbol)
        throw new TypeMismatch();
      Symbol key = (Symbol)elems.car();
      if (elems.cdr() == List.Empty)
        throw new TypeMismatch();
      Datum value = elems.cdr().car();
      elems = elems.cdr();
      result.members.put(key, value);
    }
    return result;
  }
  
  public final Type type()
  {
    return Type.Record;
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
  
  public boolean eq(Record that) throws TypeMismatch
  {
    if (this.length() != that.length())
      return false;
        
    for (Symbol key : this.members.keySet()) {
      if (!that.members.containsKey(key))
        return false;
      if (!Core.eq(this.members.get(key), that.members.get(key)))
        return false;
    }
    
    return true;
  }

  public void apply(VirtualMachine vm, int numArgs) throws Error
  {
    if (numArgs != 1)
      throw new WrongNumberArgs();
    if (vm.peekArg().type() != Type.Symbol)
      throw new TypeMismatch();
    vm.result = at((Symbol)vm.popArg());
    vm.popFrame();
  }
  
  private Record()
  {
    members = new IdentityHashMap<>();
  }
  
  private final Map<Symbol, Datum> members;
}
