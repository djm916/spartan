package spartan.runtime;

import java.util.Map;
import java.util.Set;
import java.util.IdentityHashMap;
import java.util.HashSet;

public class CodeListing
{
  public static String generate(Inst code)
  {
    var sb = new StringBuilder();
    var ctx = new Context();
    
    generateLabels(code, ctx);
    
    sb.append("\n; procedures section\n\n");
    
    for (var body : ctx.procEntries) {
      emitListing(body, ctx, sb);
      sb.append("---------------\n");
    }
    
    sb.append("\n; main section\n\n");
    
    emitListing(code, ctx, sb);
    
    return sb.toString();
  }
  
  private static class Context {
    int labelCounter = 0;
    Map<Inst, String> labels = new IdentityHashMap<>();
    Set<Inst> procEntries = new HashSet<>();
    
    String genLabel()
    {
      return "L" + (labelCounter++);
    };
  }
  
  /**
   * Compilation pass 1
   *
   *   - Generate and assign labels for all jumps and procedure entry points
   */
  private static void generateLabels(Inst code, Context ctx)
  {
    while (code != null) {
      switch (code) {
        case Apply(_, _, var next): {
          code = next;
          break;
        }
        case BindGlobal(_, _, _, var next): {
          code = next;
          break;
        }
        case BranchFalse(var left, var right): {
          ctx.labels.put(right, ctx.genLabel());
          generateLabels(left, ctx);
          code = right;
          break;
        }
        case BranchTrue(var left, var right): {
          ctx.labels.put(right, ctx.genLabel());
          generateLabels(left, ctx);
          code = right;
          break;
        }
        case Halt(): {
          code = null;
          break;
        }
        case Jump j: {
          ctx.labels.put(j.target(), ctx.genLabel());
          code = null;
          break;
        }
        case LoadConst(_, var next): {
          code = next;
          break;
        }
        case LoadGlobal(_, _, _, var next): {
          code = next;
          break;
        }
        case LoadLocal(_, _, var next): {
          code = next;
          break;
        }
        case LoadLocal0(_, var next): {
          code = next;
          break;
        }
        case MakeClosure(var proc, var next): {
          ctx.labels.put(proc.body(), ctx.genLabel());
          ctx.procEntries.add(proc.body());
          generateLabels(proc.body(), ctx);
          code = next;
          break;
        }
        case Match(_, var left, var right): {
          ctx.labels.put(right, ctx.genLabel());
          generateLabels(left, ctx);
          code = right;
          break;
        }
        case PopArg(var next): {
          code = next;
          break;
        }
        case PopEnv(var next): {
          code = next;
          break;
        }
        case PopFrame(): {
          code = null;
          break;
        }
        case PopRestArgs(var next): {
          code = next;
          break;
        }
        case PushArg(var next): {
          code = next;
          break;
        }
        case PushEnv(_, var next): {
          code = next;
          break;
        }
        case PushFrame(_, _, var next): {
          code = next;
          break;
        }
        case Raise(_, var next): {
          code = next;
          break;
        }
        case StoreGlobal(_, _, _, var next): {
          code = next;
          break;
        }
        case StoreLocal(_, _, var next): {
          code = next;
          break;
        }
        case StoreLocal0(_, var next): {
          code = next;
          break;
        }
      }
    }
  }
  
  /**
   * Compilation pass 2
   *
   *   - Emit flattened code
   */
  private static void emitListing(Inst code, Context ctx, StringBuilder sb)
  {
    while (code != null) {
      // emit (optional) label
      if (ctx.labels.containsKey(code))
        sb.append(ctx.labels.get(code) + ":");      
      sb.append("\t");
      sb.append(emit(code, ctx));
      sb.append("\n");
      switch (code) {
        case Apply(_, _, var next): {
          code = next;
          break;
        }
        case BindGlobal(_, _, _, var next): {
          code = next;
          break;
        }
        case BranchFalse(var left, var right): {
          emitListing(left, ctx, sb);
          code = right;
          break;
        }
        case BranchTrue(var left, var right): {
          emitListing(left, ctx, sb);
          code = right;
          break;
        }
        case Halt(): {
          code = null;
          break;
        }
        case Jump j: {          
          code = null;
          break;
        }
        case LoadConst(_, var next): {
          code = next;
          break;
        }
        case LoadGlobal(_, _, _, var next): {
          code = next;
          break;
        }
        case LoadLocal(_, _, var next): {
          code = next;
          break;
        }
        case LoadLocal0(_, var next): {
          code = next;
          break;
        }
        case MakeClosure(_, var next): {
          code = next;
          break;
        }
        case Match(_, var left, var right): {
          emitListing(left, ctx, sb);
          code = right;
          break;
        }
        case PopArg(var next): {
          code = next;
          break;
        }
        case PopEnv(var next): {
          code = next;
          break;
        }
        case PopFrame(): {
          code = null;
          break;
        }
        case PopRestArgs(var next): {
          code = next;
          break;
        }
        case PushArg(var next): {
          code = next;
          break;
        }
        case PushEnv(_, var next): {
          code = next;
          break;
        }
        case PushFrame(_, _, var next): {
          code = next;
          break;
        }
        case Raise(_, var next): {
          code = next;
          break;
        }
        case StoreGlobal(_, _, _, var next): {
          code = next;
          break;
        }
        case StoreLocal(_, _, var next): {
          code = next;
          break;
        }
        case StoreLocal0(_, var next): {
          code = next;
          break;
        }
      }
    }
  }
  
  private static String emit(Inst code, Context ctx)
  {
    return switch (code) {
      case Apply(var numArgs, _, _) -> String.format("(apply %d)", numArgs);
      case BindGlobal(var nsName, var baseName, _, _) -> String.format("(bind-global %s:%s)", nsName.str(), baseName.str());
      case BranchFalse(var left, var right) -> String.format("(branch-true %s)", ctx.labels.get(right));
      case BranchTrue(var left, var right)-> String.format("(branch-false %s)", ctx.labels.get(right));
      case Halt() -> "(halt)";
      case Jump j -> String.format("(jump %s)", ctx.labels.get(j.target()));
      case LoadConst(var value, _) -> String.format("(load-const %s)", value.repr());
      case LoadGlobal(var nsName, var baseName, _, _) -> String.format("(load-global %s:%s)", nsName.str(), baseName.str());
      case LoadLocal(var depth, var offset, _) -> String.format("(load-local %d %d)", depth, offset);
      case LoadLocal0(var offset, _) -> String.format("(load-local 0 %d)", offset);
      case MakeClosure(var proc, _) -> String.format("(make-closure %s)", ctx.labels.get(proc.body()));
      case Match(var pattern, _, var right) -> String.format("(match %s %s)", pattern.toString(), ctx.labels.get(right));
      case PopArg inst -> "(pop-arg)";
      case PopRestArgs inst -> "(pop-arg*)";
      case PopEnv inst -> "(pop-env)";
      case PopFrame inst -> "(pop-frame)";
      case PushArg inst -> "(push-arg)";
      case PushEnv(var numSlots, _) -> String.format("(push-env %d)", numSlots);
      case PushFrame inst -> "(push-frame)";
      case Raise inst -> "(raise)";
      case StoreGlobal(var nsName, var baseName, _, _) -> String.format("(store-global %s:%s)", nsName.str(), baseName.str());
      case StoreLocal(var depth, var offset, _) -> String.format("(store-local %d %d)", depth, offset);
      case StoreLocal0(var offset, _) -> String.format("(store-local 0 %d)", offset);
    };
  }
}
