package spartan.compiling;

/** A DeBruijnIndex is a pair of integers (depth, offset) which uniquely
  locates a variable in a statically-scoped, lexical environment.
  Recall that a lexical environment consists of a chain of environment
  frames, each of which contains a set of bindings. The depth indicates
  the number of intervening frames, and offset is the position of the
  variable within that frame.
*/
record DeBruijnIndex(int depth, int offset) {}
