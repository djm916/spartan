package spartan.runtime;

/**
 * Abstract base class for all VM instructions.
 */
public sealed interface Inst
permits Apply, BindGlobal, BranchFalse, BranchTrue, Halt, Jump, LoadConst, LoadGlobal,
        LoadLocal, LoadLocal0, MakeClosure, Match, PopArg, PopEnv, PopFrame, PopRestArgs,
        PushArg, PushEnv, PushFrame, Raise, StoreGlobal, StoreLocal, StoreLocal0
{}
