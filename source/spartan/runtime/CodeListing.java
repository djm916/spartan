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
        case Apply inst: {
          code = inst.next();
          break;
        }
        case BindGlobal inst: {
          code = inst.next();
          break;
        }
        case BranchFalse inst: {
          ctx.labels.put(inst.right(), ctx.genLabel());
          generateLabels(inst.left(), ctx);
          code = inst.right();
          break;
        }
        case BranchTrue inst: {
          ctx.labels.put(inst.right(), ctx.genLabel());
          generateLabels(inst.left(), ctx);
          code = inst.right();
          break;
        }
        case Halt inst: {
          code = null;
          break;
        }
        case Jump inst: {
          ctx.labels.put(inst.target(), ctx.genLabel());
          code = null;
          break;
        }
        case LoadConst inst: {
          code = inst.next();
          break;
        }
        case LoadGlobal inst: {
          code = inst.next();
          break;
        }
        case LoadLocal inst: {
          code = inst.next();
          break;
        }
        case LoadLocal0 inst: {
          code = inst.next();
          break;
        }
        case MakeClosure inst: {
          var body = inst.proc().body();
          ctx.labels.put(body, ctx.genLabel());
          ctx.procEntries.add(body);
          generateLabels(body, ctx);
          code = inst.next();
          break;
        }
        case Match(_, var left, var right): {
          ctx.labels.put(right, ctx.genLabel());
          generateLabels(left, ctx);
          code = right;
          break;
        }
        case PopArg inst: {
          code = inst.next();
          break;
        }
        case PopEnv inst: {
          code = inst.next();
          break;
        }
        case PopFrame inst: {
          code = null;
          break;
        }
        case PopRestArgs inst: {
          code = inst.next();
          break;
        }
        case PushArg inst: {
          code = inst.next();
          break;
        }
        case PushEnv inst: {
          code = inst.next();
          break;
        }
        case PushFrame inst: {
          code = inst.next();
          break;
        }
        case Raise inst: {
          code = inst.next();
          break;
        }
        case StoreGlobal inst: {
          code = inst.next();
          break;
        }
        case StoreLocal inst: {
          code = inst.next();
          break;
        }
        case StoreLocal0 inst: {
          code = inst.next();
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
        case Apply inst: {
          code = inst.next();
          break;
        }
        case BindGlobal inst: {
          code = inst.next();
          break;
        }
        case BranchFalse inst: {
          emitListing(inst.left(), ctx, sb);
          code = inst.right();
          break;
        }
        case BranchTrue inst: {
          emitListing(inst.left(), ctx, sb);
          code = inst.right();
          break;
        }
        case Halt inst: {
          code = null;
          break;
        }
        case Jump inst: {          
          code = null;
          break;
        }
        case LoadConst inst: {
          code = inst.next();
          break;
        }
        case LoadGlobal inst: {
          code = inst.next();
          break;
        }
        case LoadLocal inst: {
          code = inst.next();
          break;
        }
        case LoadLocal0 inst: {
          code = inst.next();
          break;
        }
        case MakeClosure inst: {
          code = inst.next();
          break;
        }
        case Match(_, var left, var right): {
          emitListing(left, ctx, sb);
          code = right;
          break;
        }
        case PopArg inst: {
          code = inst.next();
          break;
        }
        case PopEnv inst: {
          code = inst.next();
          break;
        }
        case PopFrame inst: {
          code = null;
          break;
        }
        case PopRestArgs inst: {
          code = inst.next();
          break;
        }
        case PushArg inst: {
          code = inst.next();
          break;
        }
        case PushEnv inst: {
          code = inst.next();
          break;
        }
        case PushFrame inst: {
          code = inst.next();
          break;
        }
        case Raise inst: {
          code = inst.next();
          break;
        }
        case StoreGlobal inst: {
          code = inst.next();
          break;
        }
        case StoreLocal inst: {
          code = inst.next();
          break;
        }
        case StoreLocal0 inst: {
          code = inst.next();
          break;
        }
      }
    }
  }
  
  private static String emit(Inst code, Context ctx)
  {
    return switch (code) {
      case Apply      inst -> String.format("(apply %d)", inst.numArgs());
      case BindGlobal inst -> String.format("(bind-global %s:%s)", inst.packageName().str(), inst.baseName().str());
      case BranchTrue inst -> String.format("(branch-true %s)", ctx.labels.get(inst.right()));
      case BranchFalse inst -> String.format("(branch-false %s)", ctx.labels.get(inst.right()));
      case Halt        inst -> "(halt)";
      case Jump        inst -> String.format("(jump %s)", ctx.labels.get(inst.target()));
      case LoadConst inst -> String.format("(load-const %s)", inst.value().repr());
      case LoadGlobal inst -> String.format("(load-global %s:%s)", inst.packageName().str(), inst.baseName().str());
      case LoadLocal inst -> String.format("(load-local %d %d)", inst.depth(), inst.offset());
      case LoadLocal0 inst -> String.format("(load-local 0 %d)", inst.offset());
      case MakeClosure inst -> String.format("(make-closure %s)", ctx.labels.get(inst.proc().body()));
      case Match(var pattern, _, var right) -> String.format("(match %s %s)", pattern.toString(), ctx.labels.get(right));
      case PopArg inst -> "(pop-arg)";
      case PopRestArgs inst -> "(pop-arg*)";
      case PopEnv inst -> "(pop-env)";
      case PopFrame inst -> "(pop-frame)";
      case PushArg inst -> "(push-arg)";
      case PushEnv inst -> String.format("(push-env %d)", inst.numSlots());
      case PushFrame inst -> "(push-frame)";
      case Raise inst -> "(raise)";
      case StoreGlobal inst -> String.format("(store-global %s:%s)", inst.packageName().str(), inst.baseName().str());
      case StoreLocal inst -> String.format("(store-local %d %d)", inst.depth(), inst.offset());
      case StoreLocal0 inst -> String.format("(store-local 0 %d)", inst.offset());
    };
  }
}
