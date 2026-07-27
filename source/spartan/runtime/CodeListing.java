package spartan.runtime;

import java.util.Map;
import java.util.Set;
import java.util.IdentityHashMap;
import java.util.HashSet;
import java.io.BufferedWriter;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import spartan.compiling.Procedure;

class Context
{
  private int labelCounter = 0;
  private Map<Inst, String> labels = new IdentityHashMap<>();
  private Set<Inst> procEntries = new HashSet<>();
  
  private String genLabel()
  {
    return "L" + (labelCounter++);
  };
  
  void addLabel(Inst inst)
  {
    if (!labels.containsKey(inst))
      labels.put(inst, genLabel());
  }
  
  String labelFor(Inst inst)
  {
    return labels.get(inst);
  }
  
  void addProcEntry(Inst body)
  {
    procEntries.add(body);
  }
  
  Set<Inst> procEntries()
  {
    return procEntries;
  }
}

public class CodeListing
{
  public static void generate(Inst code) throws IOException
  {
    generate(code, new BufferedWriter(new OutputStreamWriter(System.err, StandardCharsets.UTF_8)));
  }
  
  public static void generate(Inst code, Writer out) throws IOException
  {
    var ctx = new Context();    
    generateLabels(code, ctx);    
    out.write("\n; procedures section\n\n");
    for (var body : ctx.procEntries()) {
      emitListing(body, ctx, out);
      out.write("---------------\n");
    }
    out.write("\n; main section\n\n");    
    emitListing(code, ctx, out);
    out.flush();
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
        case BranchFalse(var target, var next): {
          ctx.addLabel(target);
          code = next;
          break;
        }
        case BranchTrue(var target, var next): {
          ctx.addLabel(target);
          code = next;
          break;
        }
        case Jump inst: {
          ctx.addLabel(inst.target());
          code = inst.next();
          break;
        }
        case MakeClosure(Procedure(var body, _), var next): {
          ctx.addLabel(body);
          ctx.addProcEntry(body);
          generateLabels(body, ctx);
          code = next;
          break;
        }
        case Match(_, var target, var next): {
          ctx.addLabel(target);
          code = next;
          break;
        }
        default: {
          code = code.next();
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
  private static void emitListing(Inst code, Context ctx, Writer out) throws IOException
  {
    while (code != null) {
      // emit (optional) label
      var label = ctx.labelFor(code);
      if (label != null)
        out.write(label + ":");
      out.write("\t");
      out.write(emit(code, ctx));
      out.write("\n");
      code = code.next();
    }
  }
  
  private static String emit(Inst code, Context ctx)
  {
    return switch (code) {
      case Apply(var numArgs, _, _) -> String.format("(apply %d)", numArgs);
      case BranchFalse(var target, _) -> String.format("(branch-false %s)", ctx.labelFor(target));
      case BranchTrue(var target, _)-> String.format("(branch-true %s)", ctx.labelFor(target));
      case Halt() -> "(halt)";
      case Jump inst -> String.format("(jump %s)", ctx.labelFor(inst.target()));
      case LoadConst(var value, _) -> String.format("(load-const %s)", value.repr());
      case LoadGlobal(var symbol, _, _) -> String.format("(load-global %s)", symbol);
      case LoadLocal(var depth, var offset, _) -> String.format("(load-local %d %d)", depth, offset);
      case LoadLocal0(var offset, _) -> String.format("(load-local 0 %d)", offset);
      case MakeClosure(Procedure(var body, _), _) -> String.format("(make-closure %s)", ctx.labelFor(body));
      case Match(var pattern, var target, _) -> String.format("(match %s %s)", pattern.toString(), ctx.labelFor(target));
      case Nop inst -> "(nop)";
      case PopArg inst -> "(pop-arg)";
      case PopRestArgs inst -> "(pop-arg*)";
      case PopEnv inst -> "(pop-env)";
      case PopFrame inst -> "(pop-frame)";
      case PushArg inst -> "(push-arg)";
      case PushEnv(var numSlots, _) -> String.format("(push-env %d)", numSlots);
      case PushFrame inst -> "(push-frame)";
      case Raise inst -> "(raise)";
      case StoreGlobal(var symbol, _, _) -> String.format("(store-global %s)", symbol);
      case StoreLocal(var depth, var offset, _) -> String.format("(store-local %d %d)", depth, offset);
      case StoreLocal0(var offset, _) -> String.format("(store-local 0 %d)", offset);
    };
  }
}
