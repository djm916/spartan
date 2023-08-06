package spartan.data;

import java.util.function.LongFunction;

/** 
 * The "small integer optimization" can reduce memory usage and the related garbage collection delays
 * caused by frequent allocation of many small objects (integers in this case). It is a special
 * case of the Flyweight pattern, which attempts to avoid creating new objects by re-using existing ones.
 * The basic strategy is to maintain a cache of (some) previously created objects. Then, if an object is
 * requested and a suitable one already exists, it can be returned rather than creating one anew.
 * Obviously, not every object can or should be cached, so it is wise to only cache objects that are
 * expected to be frequently requested.
 * The "small integer optimization" typically caches only a limited range of integers centered near 0.
 * For example, 0, 1, and -1 tend to appear very frequently in most programs. As another example, the
 * the byte values in [0, 255] are ubiquitous in I/O applications. The choice of exactly which set of
 * integers to cache is somewhat flexible, involving space/time trade-offs. For this reason the range
 * endpoints are configurable. The default is [-127, 255] since it covers the full range of bytes as well
 * as some portion of negative values.
 * The interface is simple: the user requests the (unique) Int object corresponding to a given long value,
 * and the cache returns it. Whether the Int already existed, or was newly created, is transparent to the user.
 * In this implementation, a given Int is allocated at most once, when first requested. If an Int is never
 * requested, it is never allocated. This on-demand allocation strategy can potentially conserve a lot of space
 * wasted by pre-allocated yet unrequested values, at a minimal time cost.
 * The cache interface is an instance of a Factory Method, a pattern which provides control over allocations by preventing
 * access to raw constructors. However, because the Int constructor is private by design, it isn't possible for the cache
 * to call it directly. This problem is solved using the Strategy pattern. The interface is extended to accept
 * a user-defined "supplier" function which assumes direct responsibility for allocation, and the cache 
 * simply delegates to it as needed. The "supplier" is defined in the Int class, so its able to call the constructor.
 */
class IntCache
{
  /**
   * Creates a cache for Ints whose value falls in the range [min, max] (inclusive)
   */
  IntCache(int min, int max)
  {
    this.min = min;
    this.max = max;
    this.slots = new Int[max - min + 1];
  }
  
  IntCache()
  {
    this(-127, 255);
  }
  
  /**
   * 
   *
   * If the given value is not eligible for caching (i.e., is outside the specified range),
   * the supplier is called and its value returned. Otherwise, if the value has not been
   * cached, the supplier is called and its value is cached. Finally, if the value has been
   * cached, the cached value is returned.
   *
   * @param value a long
   * @param supplier called to create a Int
   * @return an Int
   */
  Int get(long value, LongFunction<Int> supplier)
  {
    // First check if the value *may* have been cached
    if (value >= min && value <= max) {
      int index = (int)value - min; // Need a non-negative index!
      // Is it cached?
      if (slots[index] == null)
        slots[index] = supplier.apply(value); // No, go ahead and cache a new instance
      // Now return cached value
      return slots[index];
    }
    // Not possible to cache this value, allocate a new instance
    else return supplier.apply(value);
  }
  
  private final int min;
  private final int max;
  private final Int[] slots;
}
