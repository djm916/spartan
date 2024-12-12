package spartan.data;

import spartan.runtime.Kon;

public record Kontinue(Kon kon, Signature sig) implements Datum, IFun
{
  public Kontinue(Kon kon)
  {
    this(kon, Signature.fixed(1));
  }
}
