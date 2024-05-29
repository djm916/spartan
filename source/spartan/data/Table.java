package spartan.data;

import java.util.stream.Collectors;
import java.util.IdentityHashMap;
import spartan.errors.InvalidArgument;
import spartan.errors.TypeMismatch;
import spartan.errors.WrongNumberArgs;
import spartan.errors.NoSuchElement;
import spartan.runtime.VirtualMachine;

public final class Table implements Datum, IAssoc, ILen, IFun
{
  public static Table fromList(List elems)
  {
    var result = new Table();
    while (!elems.isEmpty()) {      
      var key = elems.car();
      elems = elems.cdr();
      if (elems.isEmpty())
        throw new WrongNumberArgs();
      var val = elems.car();
      result.set(key, val);
      elems = elems.cdr();
    }
    return result;
  }
  
  public Table()
  {
    this(DEFAULT_CAPACITY);
  }
  
  public Table(int capacity)
  {
    this.map = new IdentityHashMap<>(capacity);
  }
  
  @Override // Datum
  public Type type()
  {
    return TypeRegistry.TABLE_TYPE;
  }
    
  public Datum get(Symbol key)
  {
    var value = map.get(key);
    if (value == null)
      throw new NoSuchElement();
    return value;
  }
  
  public void set(Symbol key, Datum value)
  {
    map.put(key, value);
  }
  
  public Datum findOrElse(Datum key, Datum theDefault)
  {
    var value = map.get(key);
    return value == null ? theDefault : value;
  }
  
  public boolean contains(Datum key)
  {
    return map.get(key) == null;
  }
    
  @Override // ILen
  public int length()
  {
    return map.size();
  }
  
  @Override // ILen
  public boolean isEmpty()
  {
    return map.isEmpty();
  }
  
  @Override // IAssoc
  public Datum get(Datum key)
  {
    if (!(key instanceof Symbol s))
      throw new TypeMismatch();
    return get(s);
  }
  
  @Override // IAssoc
  public void set(Datum key, Datum value)
  {
    if (!(key instanceof Symbol s))
      throw new TypeMismatch();
    set(s, value);
  }
  
  @Override // IFun
  public void apply(VirtualMachine vm)
  {
    vm.result = get(vm.popArg());
    vm.popFrame();
  }
  
  @Override // IFun
  public Signature signature()
  {
    return SIG;
  }
  
  public List keys()
  {
    return List.of(map.keySet());
  }
  
  public List values()
  {
    return List.of(map.values());
  }
  
  public List entries()
  {
    var builder = new List.Builder();
    map.entrySet().forEach((e) -> builder.add(List.of(e.getKey(), e.getValue())));
    return builder.build();
  }
  
  private static final Signature SIG = Signature.fixed(1);
  private static final int DEFAULT_CAPACITY = 8;
  private final IdentityHashMap<Symbol, Datum> map;
}
