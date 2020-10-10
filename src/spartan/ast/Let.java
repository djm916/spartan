package spartan.ast;

import spartan.runtime.Inst;
import spartan.runtime.PushLocal;
import spartan.runtime.PopLocal;
import spartan.runtime.StoreLocal;
import spartan.errors.CompileError;
import spartan.Position;
import java.util.List;
import java.util.stream.Collectors;
import java.util.Iterator;

public class Let extends Expr
{
  private final List<Binding> bindings;
  private final Expr body;
  
  public Let(List<Binding> bindings, Expr body, Position position)
  {
    super(position);
    this.bindings = bindings;
    this.body = body;
  }
  
  public String sexp()
  {
    return String.format("(Let (%s) %s)",
      bindings.stream().map(b -> b.sexp()).collect(Collectors.joining(" ")),
      body.sexp());
  }
  
  public void analyze(GlobalEnv globals, LocalEnv locals) throws CompileError
  {
    for (Binding b : bindings) {
      b.init.analyze(globals, locals);
      locals = locals.bind(b.id);
    }
    
    body.analyze(globals, locals);
  }
  
  public Inst compile(Inst next)
  {
    return compileBindings(bindings.iterator(), body.compile(new PopLocal(bindings.size(), next)));
  }

  private Inst compileBindings(Iterator<Binding> bindings, Inst next)
  {
    if (!bindings.hasNext())
      return next;
    else
      return bindings.next().init.compile(
        new PushLocal(1,
        new StoreLocal(0, compileBindings(bindings, next))));
  }
}
