package spartan.data;

import spartan.errors.IOError;
import spartan.errors.InvalidArgument;

public abstract class Port extends Datum
{
  public final Type type()
  {
    return Type.Port;
  }
  
  public static Port open(String fileName, String flags)
  {
    if ("rw".equals(flags))
      return new FilePort(fileName);
    
    if ("r".equals(flags))
      return InputPort.fromFile(fileName);
    
    if ("w".equals(flags))
      return OutputPort.fromFile(fileName);
    
    throw new InvalidArgument("invalid file open flags");
  }
  
  public ByteVector read(int numBytes)
  {
    throw new IOError("unsupported I/O operation");
  }
  
  public void write(ByteVector bytes)
  {
    throw new IOError("unsupported I/O operation");
  }
}
