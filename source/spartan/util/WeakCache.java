package spartan.util;

import java.util.Map;
import java.util.WeakHashMap;
import java.util.Set;
import java.lang.ref.WeakReference;
import java.lang.ref.Reference;
import java.lang.ref.ReferenceQueue;
import java.util.function.Supplier;
import java.util.logging.Logger;

/**
 * A simple cache (key/value mapping) that stores weak references to its values.
 *
 */
public class WeakCache<K, V>
{
  public int size()
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
  
  public Set<K> keySet()
  {
    return cache.keySet();
  }
  
  private V put(K key, V value)
  {
    cache.put(key, new WeakReference<V>(value, unused));
    purgeUnused();
    return value;
  }
  
  private void purgeUnused()
  {
    final var startSize = size();
    var used = cache.values();
    Reference<? extends V> ref = null;
    while ((ref = unused.poll()) != null) {
      used.remove(ref);
    }
    final var endSize = size();
    final var numPurged = startSize - endSize;
    if (spartan.Config.LOG_DEBUG && numPurged > 0) {
      log.info(() -> String.format("symbol cache: purged %d symbols; %d symbols interned\n", numPurged, endSize));
    }
  }
  
  private final Map<K, WeakReference<V>> cache = new WeakHashMap<>();
  private final ReferenceQueue<V> unused = new ReferenceQueue<>();
  private static final Logger log = Logger.getLogger(WeakCache.class.getName());
}
