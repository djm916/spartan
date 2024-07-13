package spartan.data;

import spartan.errors.IOError;
import spartan.errors.InvalidArgument;
import java.io.OutputStream;
import java.io.BufferedOutputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.FileNotFoundException;
import java.nio.ByteBuffer;
import java.nio.channels.Channels;
import java.nio.channels.WritableByteChannel;

public final class OutputPort extends Port
{
  public static final OutputPort STDOUT = new OutputPort(System.out);
  
  public static OutputPort fromFile(String fileName)
  {
    try {
      return new OutputPort(new BufferedOutputStream(new FileOutputStream(fileName)));
    }
    catch (IOException ex) {
      throw new IOError(ex);
    }
  }
  
  @Override // Port
  public int write(ByteBuffer buffer, int start, int count)
  {
    try {
      buffer.position(start).limit(start + count);
    }
    catch (IllegalArgumentException ex) {
      throw new InvalidArgument();
    }
    
    try {      
      return channel.write(buffer);
    }
    catch (IOException ex) {
      throw new IOError(ex);
    }
  }
  
  @Override // Port
  public void close()
  {
    try {
      channel.close();
    }
    catch (IOException ex) {
      throw new IOError(ex);
    }
  }
  
  @Override // Port
  public boolean isOpen()
  {
    return channel.isOpen();
  }
  
  private OutputPort(OutputStream stream)
  {
    this.channel = Channels.newChannel(stream);
  }
  
  private final WritableByteChannel channel;
}
