package spartan.runtime;

import spartan.data.List;
import spartan.parsing.Position;

/**
 * A single frame in a call stack.
 *
 * Stores all the information needed to resume execution following a procedure call.
 *
 * @param parent the parent (caller's) frame, or {@code null} if none
 * @param env the saved environment
 * @param args the saved argument list
 * @param returnTo the instruction to return to when the call is complete
 * @param position the source position where the call occurs
 */
public record Kon(Kon parent, Env env, List args, Inst returnTo, Position position) {}
