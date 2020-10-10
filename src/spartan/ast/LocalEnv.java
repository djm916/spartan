package spartan.ast;

class LocalEnv
{
  static final LocalEnv Empty = new LocalEnv(null, null) {
    public Index lookup(String id)
    {
      return null;
    }
  };
  
  private final LocalEnv parent;
  private final String id;
  
  private LocalEnv(String id, LocalEnv parent)
  {
    this.id = id;
    this.parent = parent;
  }
  
  LocalEnv bind(String id)
  {
    return new LocalEnv(id, this);
  }
  
  Index lookup(String id)
  {
    int depth = lookup(id, 0);
    return depth < 0 ? null : new Index(depth, false);
  }
  
  private int lookup(String id, int depth)
  {
    if (id.equals(this.id))
      return depth;
    else if (parent != null)
      return parent.lookup(id, depth + 1);
    else
      return -1;
  }
}
