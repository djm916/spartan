package spartan.data;

/** Base interface for all Scheme objects. */
public interface Datum
{
  /** Returns the type of this object. */
  Type type();
  
  /** Returns a String representation of this object, intended to be machine-readable.
  
     If possible, evalutating the output of x.repr() should result in an object
     equivalent to x. However, this behavior is generally not possible for all objects
     and is neither guaranteed nor required.
     
     The default implementation returns a string of the form {@literal #<type @ address>},     
     where type is a string representation of this object's type and address is a hexadecimal
     representation of this object's (unique) memory address.
     
     In the REPL, the result of evaluating an expression is printed using repr().
  */
  default String repr()
  {
    return String.format("#<%s @ 0x%x>", type().name(), System.identityHashCode(this));
  }
  
  /** Returns a String representation of this object, intended to be human-readable.
     
     Delegates to repr() by default.
     By default, writing an object via an I/O function writes the str() of the object.
   */
  default String str()
  {
    return repr();
  }
  
  /** Returns the "truthiness" of this object. */
  default boolean boolValue()
  {
    return true;
  }
}
