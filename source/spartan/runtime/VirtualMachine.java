package spartan.runtime;

import spartan.data.Datum;
import spartan.data.List;
import spartan.data.Symbol;
import spartan.data.IFun;
import spartan.data.Primitive;
import spartan.data.Closure;
import spartan.data.Kontinue;
import spartan.errors.Error;
import spartan.errors.WrongNumberArgs;
import spartan.errors.TypeMismatch;
import spartan.errors.MultipleDefinition;
import spartan.errors.UnboundSymbol;
import spartan.errors.NoSuchPackage;
import spartan.parsing.Position;

/**
 * Represents state of the virtual machine.
 */
public final class VirtualMachine
{
  /**
   * Value of the last evaluated expression.
   */
  public Datum result;
  
  /**
   * List of procedure arguments.
   */
  public List args = List.EMPTY;
  
  /**
   * The next instruction to evaluate.
   */
  public Inst control;
  
  /**
   * The lexical environment: a pointer to the head of the chain of environment frames
   */
  public Env env;
  
  /**
   * The current continuation: a pointer to the head of the chain of continuation frames
   */
  public Kon kon;
  
  private static void bindGlobal(Symbol packageName, Symbol baseName, Datum value)
  {
    spartan.Runtime.getPackage(packageName).bind(baseName, value);
  }
  
  private static Datum loadGlobal(Symbol packageName, Symbol baseName)
  {
    return spartan.Runtime.getPackage(packageName).lookup(baseName);
  }
  
  private static void storeGlobal(Symbol packageName, Symbol baseName, Datum value)
  {
    spartan.Runtime.getPackage(packageName).store(baseName, value);
  }
  
  private static Datum loadLocal(Env env, int depth, int offset)
  {
    for (; depth > 0; --depth)
      env = env.parent();
    return env.get(offset);
  }
  
  private static void storeLocal(Env env, int depth, int offset, Datum value)
  {
    for (; depth > 0; --depth)
      env = env.parent();
    env.set(offset, value);
  }
  
  public Datum eval(Inst code)
  {
    control = code;
    
    try {
      while (control != null) {
        switch (control) {
          case Apply inst: {
            try {
              apply(inst.numArgs());
            }
            catch (Error err) {
              err.setSource(inst.source());
              throw err;
            }
            break;
          }
          case BindGlobal inst: {
            try {
              bindGlobal(inst.packageName(), inst.baseName(), result);
              control = inst.next();
            }
            catch (Error err) {
              err.setSource(inst.source());
              throw err;
            }
            break;
          }
          case BranchFalse inst: {
            control = !result.boolValue() ? inst.right() : inst.left();
            break;
          }
          case BranchTrue inst: {
            control = result.boolValue() ? inst.right() : inst.left();
            break;
          }
          case Halt inst: {
            control = null;
            break;
          }
          case Jump inst: {
            control = inst.target();
            break;
          }
          case LoadConst inst: {
            result = inst.value();
            control = inst.next();
            break;
          }
          case LoadGlobal inst: {
            try {
              result = loadGlobal(inst.packageName(), inst.baseName());
              control = inst.next();
            }
            catch (Error err) {
              err.setSource(inst.source());
              throw err;
            }
            break;
          }
          case LoadLocal inst: {
            result = loadLocal(env, inst.depth(), inst.offset());
            control = inst.next();
            break;
          }
          case LoadLocal0 inst: {
            result = env.get(inst.offset());
            control = inst.next();
            break;
          }
          case MakeClosure inst: {
            result = new Closure(inst.proc().body(), inst.proc().sig(), env);
            control = inst.next();
            break;
          }
          case PopArg inst: {
            result = popArg();
            control = inst.next();
            break;
          }
          case PopEnv inst: {
            env = env.parent();
            control = inst.next();
            break;
          }
          case PopFrame inst: {
            popFrame();
            break;
          }
          case PopRestArgs inst: {
            result = popRestArgs();
            control = inst.next();
            break;
          }
          case PushArg inst: {
            pushArg(result);
            control = inst.next();
            break;
          }
          case PushEnv inst: {
            env = new Env(inst.numSlots(), env);
            control = inst.next();
            break;
          }
          case PushFrame inst: {
             pushFrame(inst.returnTo(), inst.position());
             control = inst.next();
             break;
          }
          case StoreGlobal inst: {
            try {
              storeGlobal(inst.packageName(), inst.baseName(), result);
              control = inst.next();
            }
            catch (Error err) {
              err.setSource(inst.source());
              throw err;
            }
            break;
          }
          case StoreLocal inst: {
            storeLocal(env, inst.depth(), inst.offset(), result);
            control = inst.next();
            break;
          }
          case StoreLocal0 inst: {
            env.set(inst.offset(), result);
            control = inst.next();
            break;
          }
        }
      }
    }
    catch (Error err) {
      err.setBackTrace(generateBackTrace());
      reset();
      throw err;
    }
    
    // Assert VM returns to its default state
    assert control == null;
    assert args == List.EMPTY;
    assert env == null;
    assert kon == null;
        
    return result;
  }
  
  public void pushArg(Datum x)
  {
    args = ConsPool.get(x, args);
  }
  
  public Datum popArg()
  {
    var result = args.car();
    var old = args;
    args = args.cdr();
    ConsPool.put(old);
    return result;
  }
  
  public List popRestArgs()
  {
    var old = args;
    args = List.EMPTY;
    return old;
  }
  
  public void pushFrame(Inst returnTo, Position position)
  {
    kon = new Kon(kon, env, args, returnTo, position);
    args = List.EMPTY;
  }
  
  public void popFrame()
  {
    control = kon.returnTo();
    env = kon.env();
    args = kon.args();
    kon = kon.parent();
  }
  
  public void apply(int numArgs)
  {
    if (!(result instanceof IFun f))
      throw new TypeMismatch();
    
    if (!f.accepts(numArgs))
      throw new WrongNumberArgs();
    
    switch (f) {
      case Primitive p: {
        p.apply(this);
        return;
      }
      case Closure c: {
        env = c.env();
        control = c.body();
        return;
      }
      case Kontinue k: {
        kon = k.kon();
        result = popArg();
        popFrame();
        return;
      }
    }
  }
  
  public void reset()
  {
    control = null;
    args = List.EMPTY;
    env = null;
    kon = null;
  }
  
  public java.util.List<Position> generateBackTrace()
  {
    var backTrace = new java.util.ArrayList<Position>();
    for (var f = this.kon; f != null; f = f.parent())
      if (f.position() != null)
        backTrace.add(f.position());
    return backTrace;
  }
}
