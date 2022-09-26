package spartan.data;

import spartan.errors.IOError;
import java.io.OutputStream;
import java.io.BufferedOutputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.FileNotFoundException;
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
    catch (FileNotFoundException | SecurityException ex) {
      throw new IOError(ex.getMessage());
    }
  }

  public int write(Bytes src)
  {
    try {
      return channel.write(src.buffer());
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
  
  private OutputPort(OutputStream stream)
  {
    this.channel = Channels.newChannel(stream);
  }
  
  private final WritableByteChannel channel;
}
