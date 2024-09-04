package spartan.data;

import java.util.Map;
import java.util.WeakHashMap;
import java.lang.ref.WeakReference;
import java.lang.ref.Reference;
import java.lang.ref.ReferenceQueue;

public class SymbolCache
{
  int size()
  {
    return cache.size();
  }
  
  /**
   * Retrieves the value associated with the given key.
   * If the key is not currently mapped to any value, the given supplier
   * is called, its return value is stored in the cache, and returned.
   *
   * @param key a search key
   * @param supplier a value supplier
   * @return the value associated with key
   */
  public Symbol get(String name)
  {
    var ref = cache.get(name);
    if (ref == null || ref.get() == null)
      return put(name, new Symbol(name));
    else
      return ref.get();
  }
    
  public Symbol intern(Symbol s)
  {
    var ref = cache.get(s.name());
    if (ref == null || ref.get() == null)
      return put(s.name(), s);
    else
      return ref.get();
  }
  
  private Symbol put(String name, Symbol value)
  {
    cache.put(name, new WeakReference<Symbol>(value, unused));
    purgeUnused();
    return value;
  }
  
  private void purgeUnused()
  {
    var used = cache.values();
    Reference<? extends Symbol> ref = null;
    while ((ref = unused.poll()) != null)
      used.remove(ref);
  }
  
  private final Map<String, WeakReference<Symbol>> cache = new WeakHashMap<>();
  private final ReferenceQueue<Symbol> unused = new ReferenceQueue<>();
}
