package spartan.ast;

class Index
{
  final int depth;
  final boolean global;
  boolean isSet;
  
  Index(int depth, boolean global)
  {
    this.depth = depth;
    this.global = global;
  }
  
  boolean isSet()
  {
    return isSet;
  }
  
  void set()
  {
    isSet = true;
  }
}
