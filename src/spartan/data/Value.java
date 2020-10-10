package spartan.data;

import spartan.errors.TypeMismatch;

public abstract class Value
{
  public abstract Type type();
  public abstract String repr();
}
