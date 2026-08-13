package spartan.runtime;

import spartan.data.Datum;
import spartan.data.List;
import java.util.logging.Logger;

public final class ListCache
{
  public static int reusedCount = 0;
  public static int cacheSize = 0;
  public static int maxCacheSize = 0;
  
  public static List get(Datum first, List rest)
  {
    if (freeList.isEmpty())
      return List.adjoin(first, rest);
    
    ++reusedCount;
    --cacheSize;

    var list = freeList;
    freeList = freeList.rest();
    list.setFirst(first);
    list.setRest(rest);
    return list;
  }
  
  public static void put(List list)
  {
    ++cacheSize;
    if (cacheSize > maxCacheSize)
      maxCacheSize = cacheSize;
    
    list.setFirst(null);
    list.setRest(freeList);
    freeList = list;
  }
  
  public static void logMemoryUsageStats()
  {
    log.info(() -> String.format("arglist cache: max. cache size: %d; %d total nodes reused", maxCacheSize, reusedCount));
  }
  
  private static List freeList = List.EMPTY;
  private static final Logger log = Logger.getLogger(ListCache.class.getName());
}
