package spartan.data;

import spartan.runtime.CallFrame;

public record Continuation(CallFrame frame, Signature sig) implements Datum, IFun
{
  public Continuation(CallFrame frame)
  {
    this(frame, Signature.fixed(1));
  }
}
