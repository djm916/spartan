package spartan.data;

import spartan.errors.IOError;
import spartan.errors.InvalidArgument;
import java.nio.ByteBuffer;

public abstract sealed class Port implements Datum
permits InputPort, OutputPort, FilePort
{
  public static final Int EOF = Int.valueOf(-1);
  
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
  
  @Override
  public String type()
  {
    return "port";
  }
  
  public void close()
  {
    throw unsupportedOperation();
  }
  
  public abstract boolean isOpen();
  
  public int read(ByteBuffer buffer, int start, int count)
  {
    throw unsupportedOperation();
  }
  
  public int write(ByteBuffer buffer, int start, int count)
  {
    throw unsupportedOperation();
  }
  
  public long position()
  {
    throw unsupportedOperation();
  }
  
  public void seek(long position)
  {
    throw unsupportedOperation();
  }
  
  public void seekRelative(long offset)
  {
    throw unsupportedOperation();
  }
  
  public long size()
  {
    throw unsupportedOperation();
  }
  
  private static IOError unsupportedOperation()
  {
    return new IOError("unsupported I/O operation");
  }
}
