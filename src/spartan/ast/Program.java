package spartan.ast;

import spartan.runtime.Inst;
import spartan.runtime.StoreGlobal;
import spartan.runtime.LoadGlobal;
import spartan.errors.CompileError;
import spartan.errors.Error;
import spartan.errors.MultipleDefinition;
import java.util.List;
import java.util.Iterator;
import java.util.stream.Collectors;

public class Program
{
  private final List<Binding> defs;
  private Variable main;
  
  public Program(List<Binding> defs)
  {
    this.defs = defs;
  }
  
  public Inst compile() throws CompileError, Error
  {
    analyze(new GlobalEnv(), new LocalEnv());
    
    return compile(0, defs.iterator(), new LoadGlobal(main.depth(), null));
    //return compile(0, defs.iterator(), null);
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
      try {
        globals.bind(b.id);
      }
      catch (MultipleDefinition err) {
        throw new CompileError(err.getMessage(), b.pos);
      }
    }
    
    for (Binding b : defs) {
      b.init.analyze(globals, locals, false);
      globals.lookup(b.id).get().setValue();
    }
    
    main = globals.lookup("main").orElseThrow(() -> new Error("\"main\" not defined"));
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
