package spartan.compiling;

import spartan.runtime.Inst;

public record ProcTemplate(Inst code, int requiredArgs, boolean isVariadic) {}
