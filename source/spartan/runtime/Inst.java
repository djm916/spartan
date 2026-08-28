package spartan.runtime;

/**
 * Abstract base class for all VM instructions.
 */
public sealed interface Inst
permits Apply, Halt, Jump, JumpFalse, JumpTrue, JumpNoArgs, JumpNoMatch,
        LoadConst, LoadGlobal, LoadLocal, LoadLocal0, MakeClosure,
        PopArg, PopEnv, PopFrame, PopRestArgs, PushArg, PushEnv, PushFrame,
        Raise, StoreGlobal, StoreLocal, StoreLocal0, Nop
{
  public Inst next();
}
