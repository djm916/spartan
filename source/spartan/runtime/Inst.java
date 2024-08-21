package spartan.runtime;

/**
 * Abstract base class for all VM instructions.
 */
public abstract class Inst
{
  /**
   * The next instruction to execute after this.
   * Note that some instructions do not make use of this.
   */
  final Inst next;
  
  /**
   * Subclasses are required to call this constructor.
   *
   * @param next the next instruction to execute, or {@code null} if none.
   */
  protected Inst(Inst next)
  {
    this.next = next;
  }
  
  /**
   * Evaluates this {@code Inst}.
   * 
   * Subclasses of {@code Inst} are required to implement this method.
   * They should modify the {@code VirtualMachine} state in whatever way
   * is necessary to implement their functionality.
   * At a minimum, they should set {@link VirtualMachine#control} so that
   * evaluation will continue.
   *
   * @param vm the {@code VirtualMachine} instance
   * @throws Error if an error occurs
   */
  public abstract void eval(VirtualMachine vm);
}
