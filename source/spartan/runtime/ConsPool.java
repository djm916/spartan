package spartan.runtime;

import spartan.data.Datum;
import spartan.data.List;

public final class ConsPool
{
  public static int reusedCount = 0;
  public static int cacheSize = 0;
  public static int maxCacheSize = 0;
  
  public static List get(Datum car, List cdr)
  {
    if (freeList.isEmpty())
      return List.cons(car, cdr);
    
    ++reusedCount;
    --cacheSize;

    var cons = freeList;
    freeList = freeList.cdr();
    cons.setCar(car);
    cons.setCdr(cdr);
    return cons;
  }
  
  public static void put(List cons)
  {
    ++cacheSize;
    if (cacheSize > maxCacheSize)
      maxCacheSize = cacheSize;
    
    cons.setCar(null);
    cons.setCdr(freeList);
    freeList = cons;
  }
  
  private static List freeList = List.EMPTY;
}
