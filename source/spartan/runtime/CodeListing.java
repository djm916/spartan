package spartan.runtime;

import java.util.Map;
import java.util.IdentityHashMap;

public class CodeListing
{
  public static String generate(Inst code)
  {
    var sb = new StringBuilder();
    var labels = new IdentityHashMap<Inst, String>();
    
    generateLabels(labels, code);
    generateListing(sb, labels, code);
    
    return sb.toString();
  }
  
  private static Integer labelCounter = 0;
  
  private static String genLabel()
  {
    return "L" + (labelCounter++);
  }
  
  private static void generateLabels(Map<Inst, String> labels, Inst code)
  {
    top: while (code != null) {
      if (code instanceof BranchTrue br) {        
        labels.put(br.alt, genLabel());
        generateLabels(labels, br.next);
        code = br.alt;
        continue top;
      }
      if (code instanceof BranchFalse br) {        
        labels.put(br.alt, genLabel());
        generateLabels(labels, br.next);
        code = br.alt;
        continue top;
      }
      else if (code instanceof Jump jmp) {
        labels.put(jmp.target, genLabel());
      }
      else if (code instanceof PushFrame fr) {
        generateLabels(labels, fr.next);
        code = fr.returnTo;
        continue top;
      }
      code = code.next;
    }
  }
  
  private static void generateListing(StringBuilder sb, Map<Inst, String> labels, Inst code)
  {    
    top: while (code != null) {
      
      // emit (optional) label
      if (labels.containsKey(code))
        sb.append(labels.get(code) + ":");
      
      sb.append("\t");
      
      switch (code) {
        case Apply inst: {
          sb.append("(apply " + inst.numArgs + ")\n");
          break;
        }
        case BindGlobal inst: {
          sb.append("(bind-global " + inst.packageName.repr() + ":" + inst.baseName.repr() + ")\n");
          break;
        }
        case BranchTrue inst: {
          sb.append(String.format("(branch-true %s)\n", labels.get(inst.alt)));
          generateListing(sb, labels, inst.next);
          code = inst.alt;
          continue top;
        }
        case BranchFalse inst: {
          sb.append(String.format("(branch-false %s)\n", labels.get(inst.alt)));
          generateListing(sb, labels, inst.next);
          code = inst.alt;
          continue top;
        }
        case Halt inst: {
          sb.append("(halt)\n");
          break;
        }
        case Jump inst: {
          sb.append(String.format("(jump %s)\n", labels.get(inst.target)));
          break;
        }
        case LoadConst inst: {
          sb.append("(load-const " + inst.x.repr() + ")\n");
          break;
        }
        case LoadGlobal inst: {
          var symb = inst.packageName.repr() + ":" + inst.baseName.repr();
          sb.append("(load-global " + symb + ")\n");
          break;
        }
        case LoadLocal inst: {
          sb.append("(load-local " + inst.depth + " " + inst.offset + ")\n");
          break;
        }
        case LoadLocal0 inst: {
          sb.append("(load-local 0 " + inst.offset + ")\n");
          break;
        }
        case MakeClosure inst: {
          sb.append("(make-closure)\n");
          break;
        }
        case MakeCont inst: {
          sb.append("(make-cont)\n");
          break;
        }
        case PopArg inst: {
          sb.append("(pop-arg)\n");
          break;
        }
        case PopRestArgs inst: {
          sb.append("(pop-arg*)\n");
          break;
        }
        case PopEnv inst: {
          sb.append("(pop-env)\n");
          break;
        }
        case PopFrame inst: {
          sb.append("(pop-frame)\n");
          break;
        }        
        case PushArg inst: {
          sb.append("(push-arg)\n");
          break;
        }
        case PushEnv inst: {
          sb.append(String.format("(push-env %d)\n", inst.numSlots));
          break;
        }
        case PushFrame inst: {
          sb.append("(push-frame)\n");
          generateListing(sb, labels, inst.next);
          code = inst.returnTo;
          continue top;
        }
        case StoreGlobal inst: {
          var symb = inst.packageName.repr() + ":" + inst.baseName.repr();
          sb.append("(store-global " + symb + ")\n");
          break;
        }
        case StoreLocal inst: {
          sb.append("(store-local " + inst.depth + " " + inst.offset + ")\n");
          break;
        }
        case StoreLocal0 inst: {
          sb.append("(store-local 0 "+ inst.offset + ")\n");
          break;
        }
        default: {
          throw new IllegalArgumentException("unknown instruction type: " + code.getClass());
        }
      }
      
      //sb.append("\n");      
      code = code.next;
    }
  }
}
