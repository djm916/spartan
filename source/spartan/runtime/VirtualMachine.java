package spartan.runtime;

import spartan.data.Datum;
import spartan.data.List;
import spartan.data.Symbol;
import spartan.data.IFun;
import spartan.data.Primitive;
import spartan.data.Closure;
import spartan.data.Kontinue;
import spartan.compiling.Procedure;
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
          case Apply(var numArgs, var source, var next): {
            try {
              apply(numArgs);
            }
            catch (Error err) {
              err.setSource(source);
              throw err;
            }
            break;
          }
          case BindGlobal(var packageName, var baseName, var source, var next): {
            try {
              bindGlobal(packageName, baseName, result);
              control = next;
            }
            catch (Error err) {
              err.setSource(source);
              throw err;
            }
            break;
          }
          case BranchFalse(var left, var right): {
            control = !result.boolValue() ? right : left;
            break;
          }
          case BranchTrue(var left, var right): {
            control = result.boolValue() ? right : left;
            break;
          }
          case Halt(): {
            control = null;
            break;
          }
          case Jump j: {
            control = j.target();
            break;
          }
          case LoadConst(var value, var next): {
            result = value;
            control = next;
            break;
          }
          case LoadGlobal(var packageName, var baseName, var source, var next): {
            try {
              result = loadGlobal(packageName, baseName);
              control = next;
            }
            catch (Error err) {
              err.setSource(source);
              throw err;
            }
            break;
          }
          case LoadLocal(var depth, var offset, var next): {
            result = loadLocal(env, depth, offset);
            control = next;
            break;
          }
          case LoadLocal0(var offset, var next): {
            result = env.get(offset);
            control = next;
            break;
          }
          case MakeClosure(Procedure(var body, var sig), var next): {
            result = new Closure(body, sig, env);
            control = next;
            break;
          }
          case Match(var pattern, var left, var right): {
            control = pattern.match(result, env) ? left : right;
            break;
          }
          case PopArg(var next): {
            result = popArg();
            control = next;
            break;
          }
          case PopEnv(var next): {
            env = env.parent();
            control = next;
            break;
          }
          case PopFrame(): {
            popFrame();
            break;
          }
          case PopRestArgs(var next): {
            result = popRestArgs();
            control = next;
            break;
          }
          case PushArg(var next): {
            pushArg(result);
            control = next;
            break;
          }
          case PushEnv(var numSlots, var next): {
            env = new Env(numSlots, env);
            control = next;
            break;
          }
          case PushFrame(var returnTo, Position position, var next): {
             pushFrame(returnTo, position);
             control = next;
             break;
          }
          case Raise(var err, _): {
            throw err;
          }
          case StoreGlobal(var packageName, var baseName, var source, var next): {
            try {
              storeGlobal(packageName, baseName, result);
              control = next;
            }
            catch (Error err) {
              err.setSource(source);
              throw err;
            }
            break;
          }
          case StoreLocal(var depth, var offset, var next): {
            storeLocal(env, depth, offset, result);
            control = next;
            break;
          }
          case StoreLocal0(var offset, var next): {
            env.set(offset, result);
            control = next;
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
      case Closure(var body, _, var savedEnv): {
        env = savedEnv;
        control = body;
        return;
      }
      case Kontinue(var savedKon): {
        kon = savedKon;
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
