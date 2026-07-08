package spartan.runtime;

public record Halt() implements Inst
{
  public Inst next()
  {
    return null;
  }
}
