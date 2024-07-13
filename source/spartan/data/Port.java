package spartan.data;

import spartan.errors.IOError;
import java.nio.ByteBuffer;

public abstract sealed class Port implements Datum
permits InputPort, OutputPort, FilePort
{
  public static final Int EOF = Int.valueOf(-1);
  
  @Override
  public Type type()
  {
    return TypeRegistry.PORT_TYPE;
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
  
  public void flush()
  {
    throw unsupportedOperation();
  }
  
  private static IOError unsupportedOperation()
  {
    return new IOError(IOError.ErrorCode.UNSUPPORTED);
  }
}
