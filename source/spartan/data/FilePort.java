package spartan.data;

import spartan.errors.Error;
import spartan.errors.IOError;
import spartan.errors.InvalidArgument;
import java.io.IOException;
import java.io.FileNotFoundException;
import java.nio.ByteBuffer;
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
      throw new IOError(ex.getMessage());
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
      throw new IOError(ex.getMessage());
    }
  }
  
  @Override // Port
  public void close()
  {
    try {
      channel.close();
    }
    catch (IOException ex) {
      throw new IOError(ex.getMessage());
    }
  }
  
  @Override // Port
  public boolean isOpen()
  {
    return channel.isOpen();
  }
  
  @Override // Port
  public long position()
  {
    try {
      return channel.position();
    }
    catch (IOException ex) {
      throw new IOError(ex.getMessage());
    }
  }
  
  @Override // Port
  public void seek(long position)
  {
    try {
      channel.position(position);
    }
    catch (IOException ex) {
      throw new IOError(ex.getMessage());
    }
  }
  
  @Override // Port
  public long length()
  {
    try {
      return channel.size();
    }
    catch (IOException ex) {
      throw new IOError(ex.getMessage());
    }
  }
  
  private final FileChannel channel;
}
