package spartan.data;

import spartan.errors.IOError;
import java.io.InputStream;
import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.FileNotFoundException;
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
    catch (FileNotFoundException | SecurityException ex) {
      throw new IOError(ex.getMessage());
    }
  }

  public int read(Bytes dst)
  {
    try {
      return channel.read(dst.buffer());
    }
    catch (IOException ex) {
      throw new IOError(ex.getMessage());
    }    
  }
  
  public void close()
  {
    try {
      channel.close();
    }
    catch (IOException ex) {
      throw new IOError(ex.getMessage());
    }
  }
  
  private InputPort(InputStream stream)
  {
    this.channel = Channels.newChannel(stream);
  }
  
  private final ReadableByteChannel channel;
}
