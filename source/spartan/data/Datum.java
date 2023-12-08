package spartan.data;

public interface Datum
{
  Type type();
  
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
  default String repr()
  {
    return String.format("#<%s @ 0x%x>", type().name(), System.identityHashCode(this));
  }
  
  /* Returns a String representation of this object, intended to be human-readable.
     
     The default implementation just returns repr().
     
     By default, writing an object via an I/O function writes the str() of the object.
   */
  default String str()
  {
    return repr();
  }
  
  default boolean boolValue()
  {
    return true;
  }
}
