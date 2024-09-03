package spartan.runtime;

import spartan.errors.SourceInfo;

/**
 * Implements application of a {@link spartan.data.IFun}.
 */
public record Apply(int numArgs, SourceInfo source, Inst next) implements Inst
{
}
