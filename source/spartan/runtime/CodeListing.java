package spartan.runtime;

import java.util.Map;
import java.util.IdentityHashMap;

public class CodeListing
{
  public static String generate(Inst code)
  {
    var sb = new StringBuilder();
    var labels = new IdentityHashMap<Inst, String>();
    
    pass1(labels, code);
    pass2(sb, labels, code);
    
    return sb.toString();
  }
  
  private static Integer labelCounter = 0;
  
  private static String genLabel()
  {
    return "L" + (labelCounter++);
  }
  
  private static void pass1(Map<Inst, String> labels, Inst code)
  {
    top: while (code != null) {
      if (code instanceof Branch br) {
        labels.put(br.ifTrue, genLabel());
        labels.put(br.ifFalse, genLabel());
        pass1(labels, br.ifTrue);
        code = br.ifFalse;
        continue top;
      }
      else if (code instanceof Jump jmp) {
        labels.put(jmp.target, genLabel());
      }
      else if (code instanceof PushFrame fr) {
        pass1(labels, fr.next);
        code = fr.returnTo;
        continue top;
      }
      code = code.next;
    }
  }
  
  private static void pass2(StringBuilder sb, Map<Inst, String> labels, Inst code)
  {    
    top: while (code != null) {
      
      // emit (optional) label
      if (labels.containsKey(code))
        sb.append(labels.get(code) + ":");
      
      switch (code) {
        case Apply inst: {
          sb.append("(apply " + inst.numArgs + ")\n");
          break;
        }
        case BindGlobal inst: {
          sb.append("(bind-global " + inst.packageName.repr() + ":" + inst.baseName.repr() + ")\n");
          break;
        }
        case Branch inst: {
          sb.append(String.format("(branch %s %s)\n", labels.get(inst.ifTrue), labels.get(inst.ifFalse)));
          pass2(sb, labels, inst.ifTrue);
          code = inst.ifFalse;
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
          sb.append("(push-env)\n");
          break;
        }
        case PushFrame inst: {
          sb.append("(push-frame)\n");
          pass2(sb, labels, inst.next);
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
          throw new IllegalArgumentException("unknown instruction type");
        }
      }
      
      //sb.append("\n");      
      code = code.next;
    }
  }
}
