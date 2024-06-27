package spartan.builtins;

import spartan.data.*;
import spartan.runtime.VirtualMachine;
import spartan.errors.TypeMismatch;

public final class StringLib
{
  // (string c ...)
  
  public static final Primitive FROM_LIST = new Primitive(Signature.variadic(0)) {
    public void apply(VirtualMachine vm) {
      vm.result = Text.fromCodePoints(vm.popRestArgs());
      vm.popFrame();
    }
  };
  
  // (string-concat s ...)
  
  public static final Primitive CONCAT = new Primitive(Signature.variadic(0)) {
    public void apply(VirtualMachine vm) {
      vm.result = Text.concat(vm.popRestArgs());
      vm.popFrame();
    }
  };
  
  // (string-join delimiter s ...)
  
  public static final Primitive JOIN = new Primitive(Signature.variadic(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Text delim))
        throw new TypeMismatch();
      vm.result = Text.join(delim, vm.popRestArgs());
      vm.popFrame();      
    }
  };
  
  // (string-split string separator [limit])
  
  public static final Primitive SPLIT = new Primitive(Signature.variadic(2, 3)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Text string))
        throw new TypeMismatch();
      if (!(vm.popArg() instanceof Text separator))
        throw new TypeMismatch();
      if (!((vm.args.isEmpty() ? Int.valueOf(0) : vm.popArg()) instanceof IInt limit))
        throw new TypeMismatch();
      vm.result = string.split(separator, limit.intValue());
      vm.popFrame();
    }
  };
  
  // (string-substring string start [end])
  
  public static final Primitive SUBSTR = new Primitive(Signature.variadic(2, 1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Text string))
        throw new TypeMismatch();
      if (!(vm.popArg() instanceof Text.Cursor start))
        throw new TypeMismatch();
      if (!((vm.args.isEmpty() ? string.endCursor() : vm.popArg()) instanceof Text.Cursor end))
        throw new TypeMismatch();
      vm.result = string.substring(start, end);
      vm.popFrame();
    }
  };
  
  // (string-find string substring [start])
  
  public static final Primitive FIND = new Primitive(Signature.variadic(2, 1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Text string))
        throw new TypeMismatch();
      if (!(vm.popArg() instanceof Text substring))
        throw new TypeMismatch();
      if (!((vm.args.isEmpty() ? string.cursor() : vm.popArg()) instanceof Text.Cursor position))
        throw new TypeMismatch();
      vm.result = string.find(substring, position);
      vm.popFrame();
    }
  };
  
  // (string-replace string substring replacement)
  
  public static final Primitive REPLACE = new Primitive(Signature.fixed(3)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Text string))
        throw new TypeMismatch();
      if (!(vm.popArg() instanceof Text substring))
        throw new TypeMismatch();
      if (!(vm.popArg() instanceof Text replacement))
        throw new TypeMismatch();
      vm.result = string.replace(substring, replacement);
      vm.popFrame();
    }
  };
  
  // (string-insert string substring position)
  
  public static final Primitive INSERT = new Primitive(Signature.fixed(3)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Text string))
        throw new TypeMismatch();
      if (!(vm.popArg() instanceof Text substring))
        throw new TypeMismatch();
      if (!(vm.popArg() instanceof Text.Cursor position))
        throw new TypeMismatch();
      vm.result = string.insert(substring, position);
      vm.popFrame();
    }
  };
  
  // (string-delete string start end)
  
  public static final Primitive DELETE = new Primitive(Signature.fixed(3)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Text string))
        throw new TypeMismatch();
      if (!(vm.popArg() instanceof Text.Cursor start))
        throw new TypeMismatch();
      if (!(vm.popArg() instanceof Text.Cursor end))
        throw new TypeMismatch();
      vm.result = string.delete(start, end);
      vm.popFrame();
    }
  };
  
  // (string-reverse string)
  
  public static final Primitive REVERSE = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Text text))
        throw new TypeMismatch();
      vm.result = text.reverse();
      vm.popFrame();
    }
  };
  
  // (string-hash string)
  
  public static final Primitive HASH = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Text text))
        throw new TypeMismatch();
      vm.result = Int.valueOf(text.hashCode());
      vm.popFrame();
    }
  };
  
  // (string-ref string position)
  
  public static final Primitive REF = new Primitive(Signature.fixed(2)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Text string && vm.popArg() instanceof Text.Cursor position))
        throw new TypeMismatch();
      vm.result = string.get(position);
      vm.popFrame();
    }
  };
  
  /**
  
  // (string-set! str idx val)
  
  public static final Primitive SET = new Primitive(Signature.fixed(3)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Text str && vm.popArg() instanceof IInt index))
        throw new TypeMismatch();
      var item = vm.popArg();
      str.set(index.intValue(), item);
      vm.result = Nil.VALUE;
      vm.popFrame();
    }
  };
  
  */
  
  // (string-length string)
  
  public static final Primitive LENGTH = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Text text))
        throw new TypeMismatch();
      vm.result = Int.valueOf(text.length());
      vm.popFrame();
    }
  };
  
  // (string-empty? string)
  
  public static final Primitive IS_EMPTY = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Text text))
        throw new TypeMismatch();
      vm.result = Bool.valueOf(text.length() == 0);
      vm.popFrame();
    }
  };
  
  // (string-cursor-begin string)
  
  public static final Primitive CURSOR_BEGIN = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Text string))
        throw new TypeMismatch();
      vm.result = string.cursor();
      vm.popFrame();
    }
  };
  
  // (string-cursor-end string)
  
  public static final Primitive CURSOR_END = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Text string))
        throw new TypeMismatch();
      vm.result = string.endCursor();
      vm.popFrame();
    }
  };
  
  // (string-cursor-next position)
  
  public static final Primitive CURSOR_NEXT = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Text.Cursor cursor))
        throw new TypeMismatch();
      vm.result = cursor.next();
      vm.popFrame();
    }
  };
  
  // (string-cursor-prev position)
  
  public static final Primitive CURSOR_PREV = new Primitive(Signature.fixed(1)) {
    public void apply(VirtualMachine vm) {
      if (!(vm.popArg() instanceof Text.Cursor cursor))
        throw new TypeMismatch();
      vm.result = cursor.prev();
      vm.popFrame();
    }
  };
  
}
