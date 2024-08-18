package spartan.runtime;

import java.util.Map;
import java.util.IdentityHashMap;

/**
 * Abstract base class for all VM instructions.
 */
public abstract class Inst
{
  /**
   * The next instruction to execute after this.
   * Note that some instructions do not make use of this.
   */
  protected final Inst next;
  
  /**
   * Subclasses are required to call this constructor.
   *
   * @param next the next instruction to execute, or {@code null} if none.
   */
  protected Inst(Inst next)
  {
    this.next = next;
  }
  
  /**
   * Evaluates this {@code Inst}.
   * 
   * Subclasses of {@code Inst} are required to implement this method.
   * They should modify the {@code VirtualMachine} state in whatever way
   * is necessary to implement their functionality.
   * At a minimum, they should set {@link VirtualMachine#control} so that
   * evaluation will continue.
   *
   * @param vm the {@code VirtualMachine} instance
   * @throws Error if an error occurs
   */
  public abstract void eval(VirtualMachine vm);
  
  public abstract void emit(StringBuilder sb);
  
  public static String codeListing(Inst code)
  {
    var sb = new StringBuilder();
    var labels = new IdentityHashMap<Inst, String>();
    
    //code.emit(sb);
    codeListingPass1(labels, code);
    codeListing(sb, labels, code);
    
    return sb.toString();
  }
  
  private static Integer labelCounter = 0;
  
  private static String genLabel()
  {
    return "L" + (labelCounter++);
  }
  
  private static void codeListingPass1(Map<Inst, String> labels, Inst code)
  {
    top: while (code != null) {
      if (code instanceof Branch br) {
        labels.put(br.ifTrue, genLabel());
        labels.put(br.ifFalse, genLabel());
        codeListingPass1(labels, br.ifTrue);
        //codeListing(sb, br.right());
        code = br.ifFalse;
        continue top;
      }
      else if (code instanceof Jump jmp) {
        labels.put(jmp.target, genLabel());
      }
      else if (code instanceof PushFrame fr) {
        codeListingPass1(labels, fr.next);
        code = fr.returnTo;
        continue top;
      }
      code = code.next;
    }
  }
  
  private static void codeListing(StringBuilder sb, Map<Inst, String> labels, Inst code)
  {    
    top: while (code != null) {
      
      // emit (optional) label
      if (labels.containsKey(code))
        sb.append(labels.get(code) + ":");
      
      // emit instruction
      code.emit(sb);
      
      if (code instanceof Branch br) {
        //var lab1 = genLabel();
        //var lab2 = genLabel();
        //sb.append(String.format("(branch %s %s)\n", lab1, lab2));
        //labels.put(br.ifTrue, lab1);
        //labels.put(br.ifFalse, lab2);
        sb.append(String.format("(branch %s %s)\n", labels.get(br.ifTrue), labels.get(br.ifFalse)));
        codeListing(sb, labels, br.ifTrue);
        //codeListing(sb, br.right());
        code = br.ifFalse;
        continue top;
      }
      else if (code instanceof Jump jmp) {
        //var lab = genLabel();
        //labels.put(jmp.target, lab);
        //sb.append(String.format("(jump %s)", lab));
        sb.append(String.format("(jump %s)\n", labels.get(jmp.target)));
      }
      else if (code instanceof PushFrame fr) {
        sb.append("(push-frame)\n");
        codeListing(sb, labels, fr.next);
        code = fr.returnTo;
        continue top;
      }
      //sb.append("\n");      
      code = code.next;
    }
  }
}
