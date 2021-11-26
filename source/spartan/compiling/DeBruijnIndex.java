package spartan.compiling;

/* Each lexical variable in a scope is located by a unique
   pair of integers (depth, offset). Depth is the number
   of intervening environment frames, and offset is the
   index of the variable within that environment.
*/
class DeBruijnIndex
{
  final int depth;
  final int offset;
  
  DeBruijnIndex(int depth, int offset)
  {
    this.depth = depth;
    this.offset = offset;
  }
}
