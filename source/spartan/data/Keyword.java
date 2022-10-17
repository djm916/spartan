package spartan.data;

import java.util.Map;
import java.util.HashMap;
import java.lang.ref.WeakReference;
import java.lang.ref.Reference;
import java.lang.ref.ReferenceQueue;

public final class Keyword extends Datum
{
  private static final Map<String, WeakReference<Keyword>> interned = new HashMap<>();
  private static final ReferenceQueue unused = new ReferenceQueue();
  
  public static Keyword of(String id)
  {
    var result = interned.get(id);
    if (result == null || result.get() == null) {
      var symbol = new Keyword(id);
      interned.put(id, new WeakReference<>(symbol, unused));
      purgeUnusedKeywords();
      return symbol;
    }
    return result.get();
  }
  
  @Override
  public Type type()
  {
    return Type.KEYWORD;
  }
  
  @Override
  public String repr()
  {
    return id;
  }
  
  private Keyword(String id)
  {
    this.id = id;
  }
  
  private static void purgeUnusedKeywords()
  {
    var used = interned.values();
    Reference<Keyword> ref = null;
    while ((ref = unused.poll()) != null)
      used.remove(ref);
  }
  
  private static int nextUniqueId;
  private final String id;
}
