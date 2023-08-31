package spartan.builtins;

import spartan.Namespace;
import spartan.data.Symbol;
import spartan.data.InputPort;
import spartan.data.OutputPort;
import static spartan.builtins.PortLib.*;

public final class PortNS extends Namespace
{
  public PortNS()
  {
    super(Symbol.of("port"));
  }
  
  {
    bind(Symbol.of("open"), OPEN);
    bind(Symbol.of("close"), CLOSE);
    bind(Symbol.of("read"), READ);
    bind(Symbol.of("write"), WRITE);
    bind(Symbol.of("stdin"), InputPort.STDIN);
    bind(Symbol.of("stdout"), OutputPort.STDOUT);
    bind(Symbol.of("open?"), IS_OPEN);
    bind(Symbol.of("position"), POSITION);
    bind(Symbol.of("seek"), SEEK);
    bind(Symbol.of("length"), LENGTH);
  }
}
