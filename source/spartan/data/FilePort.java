package spartan.data;

import spartan.errors.Error;
import spartan.errors.IOError;
import java.io.IOException;
import java.io.FileNotFoundException;
import java.nio.channels.FileChannel;
import java.nio.file.Path;
import static java.nio.file.StandardOpenOption.*;

public final class FilePort extends Port
{
  public FilePort(String fileName)
  {
    try {
      this.channel = FileChannel.open(Path.of(fileName), READ, WRITE);
    }
    catch (IOException ex) {
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
  
  private final FileChannel channel;
}
