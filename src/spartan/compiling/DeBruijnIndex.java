package spartan.compiling;

class DeBruijnIndex
{
  public final int depth;
  public final int offset;
  
  DeBruijnIndex(int depth, int offset)
  {
    this.depth = depth;
    this.offset = offset;
  }
}
