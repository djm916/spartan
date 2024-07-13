package spartan.data;

import spartan.errors.IOError;
import spartan.errors.InvalidArgument;
import java.io.InputStream;
import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.FileNotFoundException;
import java.nio.ByteBuffer;
import java.nio.channels.Channels;
import java.nio.channels.ReadableByteChannel;

public final class InputPort extends Port
{
  public static final InputPort STDIN = new InputPort(System.in);
  
  public static InputPort fromFile(String fileName)
  {
    try {
      return new InputPort(new BufferedInputStream(new FileInputStream(fileName)));
    }
    catch (IOException ex) {
      throw new IOError(ex);
    }
  }

  @Override // Port
  public int read(ByteBuffer buffer, int start, int count)
  {
    try {
      buffer.position(start).limit(start + count);
    }
    catch (IllegalArgumentException ex) {
      throw new InvalidArgument();
    }
    
    try {      
      return channel.read(buffer);
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
  
  private InputPort(InputStream stream)
  {
    this.channel = Channels.newChannel(stream);
  }
  
  private final ReadableByteChannel channel;
}
