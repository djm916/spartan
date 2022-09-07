package spartan.runtime;

import spartan.data.List;
import spartan.parsing.Position;

public record Frame(Frame parent, LocalEnv locals, List args, Inst returnTo, Position position) {};
