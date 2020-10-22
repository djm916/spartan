package spartan.ast;

interface Variable
{
  String id();
  boolean global();
  int depth();
  boolean hasValue();
  void setValue();
}
