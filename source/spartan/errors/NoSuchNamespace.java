package spartan.errors;

import spartan.data.Symbol;

public class NoSuchNamespace extends Error
{
  public NoSuchNamespace(Symbol nsName)
  {
    super("no such namespace: " + nsName.repr());
  }
}
