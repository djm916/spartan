package spartan.runtime;

public record Halt() implements Inst
{
  Inst next()
  {
    return null;
  }
}
