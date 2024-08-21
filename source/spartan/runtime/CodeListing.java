package spartan.runtime;

import java.util.Map;
import java.util.IdentityHashMap;

public class CodeListing
{
  public static String generate(Inst code)
  {
    var sb = new StringBuilder();
    var ctx = new Context();
    
    generateLabels(code, ctx);
    emitListing(code, ctx, sb);
    
    return sb.toString();
  }
  
  private static class Context {
    int labelCounter = 0;
    Map<Inst, String> labels = new IdentityHashMap<>();
    
    String genLabel()
    {
      return "L" + (labelCounter++);
    };
  }
  
  private static void generateLabels(Inst code, Context ctx)
  {
    // TODO: make the br.next of the (jt) be the "true" branch and the branch.alt be the "false" branch 
    
    while (code != null) {
      if (code instanceof BranchTrue br) {        
        ctx.labels.put(br.alt, ctx.genLabel());
        generateLabels(br.next, ctx);
        code = br.alt;
      }
      else if (code instanceof BranchFalse br) {        
        ctx.labels.put(br.alt, ctx.genLabel());
        generateLabels(br.next, ctx);
        code = br.alt;
      }
      else if (code instanceof Jump jmp) {
        ctx.labels.put(jmp.target, ctx.genLabel());
        code = code.next;
      }
      else {
        code = code.next;
      }
    }
  }
  
  private static void emitListing(Inst code, Context ctx, StringBuilder sb)
  {
    while (code != null) {
      // emit (optional) label
      if (ctx.labels.containsKey(code))
        sb.append(ctx.labels.get(code) + ":");      
      sb.append("\t");
      sb.append(emit(code, ctx));
      sb.append("\n");
      if (code instanceof BranchTrue br) {        
        emitListing(br.next, ctx, sb);
        code = br.alt;
      }
      else if (code instanceof BranchFalse br) {        
        emitListing(br.next, ctx, sb);
        code = br.alt;
      }
      else {
        code = code.next;
      }      
    }
  }
  
  private static String emit(Inst code, Context ctx)
  {
    return switch (code) {
      case Apply      inst -> String.format("(apply " + inst.numArgs + ")");
      case BindGlobal inst -> String.format("(bind-global %s:%s)", inst.packageName.str(), inst.baseName.str());
      case BranchTrue inst -> String.format("(branch-true %s)", ctx.labels.get(inst.alt));
      case BranchFalse inst -> String.format("(branch-false %s)", ctx.labels.get(inst.alt));
      case Halt        inst -> "(halt)";
      case Jump        inst -> String.format("(jump %s)", ctx.labels.get(inst.target));
      case LoadConst inst -> String.format("(load-const %s)", inst.x.repr());
      case LoadGlobal inst -> String.format("(load-global %s:%s)", inst.packageName.str(), inst.baseName.str());
      case LoadLocal inst -> String.format("(load-local %d %d)", inst.depth, inst.offset);
      case LoadLocal0 inst -> String.format("(load-local 0 %d)", inst.offset);
      case MakeClosure inst -> "(make-closure)";
      case MakeCont inst -> "(make-cont)";
      case PopArg inst -> "(pop-arg)";
      case PopRestArgs inst -> "(pop-arg*)";
      case PopEnv inst -> "(pop-env)";
      case PopFrame inst -> "(pop-frame)";
      case PushArg inst -> "(push-arg)";
      case PushEnv inst -> String.format("(push-env %d)", inst.numSlots);
      case PushFrame inst -> "(push-frame)";
      case StoreGlobal inst -> String.format("(store-global %s:%s)", inst.packageName.str(), inst.baseName.str());
      case StoreLocal inst -> String.format("(store-local %d %d)", inst.depth, inst.offset);
      case StoreLocal0 inst -> String.format("(store-local 0 %d)", inst.offset);
      default -> throw new IllegalArgumentException("unknown instruction type: " + code.getClass());
    };
  }
}
