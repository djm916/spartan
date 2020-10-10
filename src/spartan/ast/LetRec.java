package spartan.ast;

import spartan.Position;
import spartan.runtime.Inst;
import spartan.runtime.PushLocal;
import spartan.runtime.PopLocal;
import spartan.runtime.StoreLocal;
import spartan.errors.CompileError;
import java.util.List;
import java.util.stream.Collectors;
import java.util.Iterator;

public class LetRec extends Expr
{
  private final List<Binding> bindings;
  private final Expr body;
  
  public LetRec(List<Binding> bindings, Expr body, Position position)
  {
    super(position);
    this.bindings = bindings;
    this.body = body;
  }
  
  public String sexp()
  {
    return String.format("(LetRec (%s) %s)",
      bindings.stream().map(b -> b.sexp()).collect(Collectors.joining(" ")),
      body.sexp());
  }
  
  public void analyze(GlobalEnv globals, LocalEnv locals) throws CompileError
  {
    for (Binding b : bindings) {
      locals = locals.bind(b.id);
    }

    for (Binding b : bindings) {
      b.init.analyze(globals, locals);
    }
    
    body.analyze(globals, locals);
  }
  
  public Inst compile(Inst next, boolean tailContext)
  {
    final int n = bindings.size();
    return new PushLocal(n, compileBindings(n - 1, bindings.iterator(), body.compile(new PopLocal(n, next), tailContext)));
  }

  private Inst compileBindings(int depth, Iterator<Binding> bindings, Inst next)
  {
    if (!bindings.hasNext())
      return next;
    else
      return bindings.next().init.compile(new StoreLocal(depth, compileBindings(depth - 1, bindings, next)), false);
  }
}
