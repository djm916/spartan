package spartan.data;

import java.util.stream.Collectors;
import java.util.IdentityHashMap;
import spartan.errors.InvalidArgument;
import spartan.errors.TypeMismatch;
import spartan.errors.WrongNumberArgs;
import spartan.errors.NoSuchElement;
import spartan.runtime.VirtualMachine;
import spartan.compiling.Signature;

public final class Struct implements Datum, IAssoc, ILen, IFun
{
  public static Struct create(Type type, List members)
  {
    var result = new Struct(type, members.length());
    while (!members.isEmpty()) {
      var key = members.car();
      members = members.cdr();
      if (members.isEmpty())
        throw new WrongNumberArgs();
      var val = members.car();
      result.set(key, val);
      members = members.cdr();
    }
    return result;
  }
  
  public Struct(Type type)
  {
    this(type, DEFAULT_CAPACITY);
  }
  
  public Struct(Type type, int capacity)
  {
    this.type = type;
    this.map = new IdentityHashMap<>(capacity);
  }
  
  @Override // Datum
  public Type type()
  {
    return type;
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
  
  private static final Signature SIG = new Signature(1, false);
  private static final int DEFAULT_CAPACITY = 8;
  private final Type type;
  private final IdentityHashMap<Symbol, Datum> map;
}
