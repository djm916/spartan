package spartan.util;

import java.util.Map;
import java.util.WeakHashMap;
import java.lang.ref.WeakReference;
import java.lang.ref.Reference;
import java.lang.ref.ReferenceQueue;
import java.util.function.Supplier;

/**
 * A simple cache (key/value mapping) that stores weak references to its values.
 *
 * @param K the type of key
 * @param V the type of value
 */
public class WeakCache<K, V>
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
  public V get(K key, Supplier<V> supplier)
  {
    var ref = cache.get(key);
    if (ref == null || ref.get() == null)
      return put(key, supplier.get());
    else
      return ref.get();
  }
  
  private V put(K key, V value)
  {
    cache.put(key, new WeakReference<V>(value, unused));
    purgeUnused();
    return value;
  }
  
  private void purgeUnused()
  {
    var used = cache.values();
    Reference<? extends V> ref = null;
    while ((ref = unused.poll()) != null)
      used.remove(ref);
  }
  
  private final Map<K, WeakReference<V>> cache = new WeakHashMap<>();
  private final ReferenceQueue<V> unused = new ReferenceQueue<>();
}
