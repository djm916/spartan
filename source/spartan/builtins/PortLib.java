package spartan.builtins;

import spartan.data.*;
import spartan.data.Void; // shadows java.lang.Void
import spartan.errors.TypeMismatch;
import spartan.errors.InvalidArgument;
import spartan.errors.IOError;
import spartan.runtime.VirtualMachine;
import java.nio.file.OpenOption;
import java.nio.file.StandardOpenOption;
import java.io.IOException;
import java.io.FileNotFoundException;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.InvalidPathException;
import java.nio.file.AccessDeniedException;
import java.nio.channels.ClosedChannelException;
import java.util.Map;
import java.util.IdentityHashMap;
import java.util.HashSet;

public final class PortLib
{
  private static final Map<Symbol, OpenOption> openOptionMap = new IdentityHashMap<>();
  
  static {
    openOptionMap.put(Symbol.of("read"), StandardOpenOption.READ);
    openOptionMap.put(Symbol.of("write"), StandardOpenOption.WRITE);
    openOptionMap.put(Symbol.of("create"), StandardOpenOption.CREATE);
    openOptionMap.put(Symbol.of("append"), StandardOpenOption.APPEND);
    openOptionMap.put(Symbol.of("truncate"), StandardOpenOption.TRUNCATE_EXISTING);
  }
  
  private static OpenOption[] lowerOpenOptions(List options)
  {
    var result = new HashSet<OpenOption>();
    for (var elem : options) {
      if (!(elem instanceof Symbol kw))
        throw new TypeMismatch();
      var opt = openOptionMap.get(kw);
      if (opt == null)      
        throw new InvalidArgument();
      result.add(opt);
    }
    return result.toArray(OpenOption[]::new);
  }
  
  private static List ok(Datum result)
  {
    return List.of(result, Void.VALUE);
  }
  
  private static List fail(IOError err)
  {
    //return List.of(Void.VALUE, Int.valueOf(err.errorNo()));
    return List.of(Void.VALUE, new Text(err.getMessage()));
  }
  
  // (port-open-file path options)
  
  public static final Primitive OPEN = new Primitive(Signature.fixed(2)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Text path))
        throw new TypeMismatch();
      if (!(vm.popArg() instanceof List options))
        throw new TypeMismatch();
      var optionArray = lowerOpenOptions(options);
      try {
        vm.result = ok(new FilePort(path.str(), optionArray));
      }
      catch (IOError err) {
        //System.out.println("caught IOError with error code " + err.errorNo());
        vm.result = fail(err);
      }
      finally {
        vm.popFrame();
      }
    }
  };
  
  // (port-close port)
  
  public static final Primitive CLOSE = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Port port))
        throw new TypeMismatch();
      try {
        port.close();
        vm.result = ok(Void.VALUE);
      }
      catch (IOError err) {
        vm.result = fail(err);
      }
      finally {
        vm.popFrame();
      }
    }
  };
  
  // (port-open? port)
  
  public static final Primitive IS_OPEN = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Port port))
        throw new TypeMismatch();
      try {
        vm.result = ok(Bool.valueOf(port.isOpen()));
      }
      catch (IOError err) {
        vm.result = fail(err);
      }
      finally {
        vm.popFrame();
      }
    }
  };
  
  // (port-read port buffer start count)
  
  public static final Primitive READ = new Primitive(Signature.fixed(4)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Port port && vm.popArg() instanceof Bytes bytes && vm.popArg() instanceof IInt start && vm.popArg() instanceof IInt count))
        throw new TypeMismatch();
      try {
        vm.result = ok(Int.valueOf(port.read(bytes.buffer(), start.intValue(), count.intValue())));
      }
      catch (IOError err) {
        vm.result = fail(err);
      }
      finally {
        vm.popFrame();
      }
    }
  };
  
  // (port-write port buffer start count)
  
  public static final Primitive WRITE = new Primitive(Signature.fixed(4)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Port port && vm.popArg() instanceof Bytes bytes && vm.popArg() instanceof IInt start && vm.popArg() instanceof IInt count))
        throw new TypeMismatch();
      try {
        vm.result = ok(Int.valueOf(port.write(bytes.buffer(), start.intValue(), count.intValue())));
      }
      catch (IOError err) {
        vm.result = fail(err);
      }
      finally {
        vm.popFrame();
      }
    }
  };
  
  // (port-position port)
  
  public static final Primitive POSITION = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Port port))
        throw new TypeMismatch();
      try {
        vm.result = ok(Int.valueOf(port.position()));
      }
      catch (IOError err) {
        vm.result = fail(err);
      }
      finally {
        vm.popFrame();
      }
    }
  };
  
  // (port-seek port position)
  
  public static final Primitive SEEK = new Primitive(Signature.fixed(2)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Port port && vm.popArg() instanceof IInt position))
        throw new TypeMismatch();
      try {
        port.seek(position.longValue());
        vm.result = ok(Void.VALUE);
      }
      catch (IOError err) {
        vm.result = fail(err);
      }
      finally {
        vm.popFrame();
      }
    }
  };
  
  // (port-size port)
  
  public static final Primitive SIZE = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Port port))
        throw new TypeMismatch();
      try {
        vm.result = ok(Int.valueOf(port.size()));
      }
      catch (IOError err) {
        vm.result = fail(err);
      }
      finally {
        vm.popFrame();
      }
    }
  };
}
