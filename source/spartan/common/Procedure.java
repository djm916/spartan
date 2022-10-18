package spartan.common;

import spartan.runtime.Inst;

/**
  Represents a procedure (closure, macro, etc.). Includes the bytecode of
  the procedure body, the minimum
  number of required arguments, and whether or not the procedure is variadic. */
public record Procedure(Inst body, Signature sig) {}
