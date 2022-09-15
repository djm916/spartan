package spartan.compiling;

import spartan.runtime.Inst;

/** A procedure template, including the bytecode of its body, the minimum
  number of required arguments, and whether or not the procedure is variadic. */
public record ProcTemplate(Inst code, int requiredArgs, boolean isVariadic) {}
