package spartan.ast;

import spartan.Position;
import spartan.runtime.Inst;
import spartan.runtime.MakeRecord;
import spartan.runtime.PushArg;
import spartan.errors.CompileError;
import java.util.List;
import java.util.Iterator;
import java.util.stream.Stream;
import java.util.stream.Collectors;

public class Record extends Expr
{
  public Record(List<Binding> members, Position position)
  {
    super(position);
    this.members = members;
  }
  
  public String sexp()
  {
    return String.format("(Record %s)",
       members.stream()
      .map(b -> String.format("(%s %s)", b.id, b.init.sexp()))
      .collect(Collectors.joining(" ", "(", ")")));
  }
  
  public void analyze(GlobalEnv globals, LocalEnv locals) throws CompileError
  {
    for (Binding b : members)
      b.init.analyze(globals, locals);
  }
  
  public Inst compile(Inst next)
  {
    return compile(members.iterator(), new MakeRecord(labelsArray(), next));
  }
  
  private Inst compile(Iterator<Binding> members, Inst next)
  {
    if (!members.hasNext())
      return next;
    else
      return members.next().init.compile(new PushArg(compile(members, next)));
  }
  
  private String[] labelsArray()
  {
    return members.stream().map(b -> b.id.intern()).toArray(String[]::new);
  }
  
  private final List<Binding> members;
}
