package spartan.compiling;

import spartan.runtime.Inst;

/**
  Represents the basic data of a user-defined procedure (closure or macro),
  including the bytecode of the procedure body, and the procedure signature.
*/
public record Procedure(Inst body, Signature sig) {}
