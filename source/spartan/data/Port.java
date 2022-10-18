package spartan.data;

import spartan.errors.IOError;
import spartan.errors.InvalidArgument;

public abstract sealed class Port implements Datum
permits InputPort, OutputPort, FilePort
{
  public static final Int EOF = new Int(-1);
  
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
  
  public Type type()
  {
    return Type.PORT;
  }
  
  public void close()
  {
    throw unsupportedOperation();
  }
  
  public int read(Bytes bytes)
  {
    throw unsupportedOperation();
  }
  
  public int write(Bytes bytes)
  {
    throw unsupportedOperation();
  }
  
  private static IOError unsupportedOperation()
  {
    return new IOError("unsupported I/O operation");
  }
}
