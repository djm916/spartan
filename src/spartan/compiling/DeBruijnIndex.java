package spartan.compiling;

public class DeBruijnIndex
{
  public final int depth;
  public final int offset;
  
  public DeBruijnIndex(int depth, int offset)
  {
    this.depth = depth;
    this.offset = offset;
  }
}
