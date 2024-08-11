package spartan.compiling;

import spartan.runtime.Inst;
import spartan.data.Signature;

/**
  A user-defined procedure (closure or macro).
  
  @param body the instructions that make up the procedure's body
  @param sig the procedure's signature
*/
public record Procedure(Inst body, Signature sig) {}
