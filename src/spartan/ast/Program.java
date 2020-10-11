package spartan.ast;

import spartan.runtime.Inst;
import spartan.runtime.StoreGlobal;
import spartan.runtime.LoadGlobal;
import spartan.errors.CompileError;
import spartan.errors.Error;
import java.util.List;
import java.util.Iterator;
import java.util.stream.Collectors;

public class Program
{
  private final List<Binding> defs;
  private Index main;
  
  public Program(List<Binding> defs)
  {
    this.defs = defs;
  }
  
  public Inst compile() throws CompileError, Error
  {
    analyze(new GlobalEnv(), LocalEnv.Empty);
    
    return compile(0, defs.iterator(), new LoadGlobal(main.depth, null));
  }
  
  public String sexp()
  {
    return String.format("(Prog %s)",
      defs.stream()
          .map(b -> b.sexp())
          .collect(Collectors.joining(" ")));
  }
 
  private void analyze(GlobalEnv globals, LocalEnv locals) throws CompileError, Error
  {
    for (Binding b : defs) {
      globals.bind(b.id);
    }
    
    for (Binding b : defs) {
      b.init.analyze(globals, locals);
    }
    
    main = globals.lookup("main");
    if (main == null)
      throw new Error("\"main\" not defined");
  }
  
  private Inst compile(int depth, Iterator<Binding> defs, Inst next)
  {
    if (!defs.hasNext())
      return next;
    else
      return defs.next().init.compile(false,
             new StoreGlobal(depth,
             compile(depth + 1, defs, next)));
  }
}
