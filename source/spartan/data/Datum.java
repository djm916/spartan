package spartan.data;

public abstract sealed class Datum
permits Nil, Bool, Numeric, Callable, List, Vector, Map, Symbol, Text, Port, Bytes
{
  public abstract Type type();
  
  /* Returns a String representation of this object, intended to be machine-readable.
  
     If possible, evalutating the output of x.repr() should result in an object
     equivalent to x. However, this behavior is generally not possible for all objects
     and is neither guaranteed nor required.
     
     The default implementation returns a string of the format
     
       #<type @ address>
     
     where type is a string representation of this object's type and address is a hexadecimal
     representation of this object's (unique) memory address.
     
     At the REPL, the repr() of the result of evaluating an expression is what is displayed to
     the user.
  */
  public String repr()
  {
    return String.format("#<%s @ 0x%x>", type(), System.identityHashCode(this));
  }
  
  /* Returns a String representation of this object, intended to be human-readable.
     
     The default implementation just returns repr().
     
     By default, writing an object via an I/O function writes the str() of the object.
   */
  public String str()
  {
    return repr();
  }
}
