package spartan.data;

import spartan.errors.Error;
import spartan.errors.IOError;
import spartan.errors.TypeMismatch;
import spartan.errors.InvalidArgument;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.io.FileNotFoundException;
import java.nio.file.InvalidPathException;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.file.Path;
import java.nio.file.OpenOption;
import java.util.HashSet;
import java.util.Map;
import java.util.IdentityHashMap;
import java.util.Optional;

public final class FilePort extends Port
{
  public FilePort(String fileName, OpenOption... options)
  {
    try {
      this.channel = FileChannel.open(Path.of(fileName), options);
    }
    catch (IOException ex) {
      //System.out.println(String.format("in FilePort, caught IOException of type %s with message %s", ex.getClass().getName(), ex.getMessage()));
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
  
  @Override // Port
  public long position()
  {
    try {
      return channel.position();
    }
    catch (IOException ex) {
      throw new IOError(ex);
    }
  }
  
  @Override // Port
  public void seek(long position)
  {
    try {
      channel.position(position);
    }
    catch (IllegalArgumentException ex) {
      throw new InvalidArgument();
    }
    catch (IOException ex) {
      throw new IOError(ex);
    }
  }
  
  @Override // Port
  public long size()
  {
    try {
      return channel.size();
    }
    catch (IOException ex) {
      throw new IOError(ex);
    }
  }
  
  private final FileChannel channel;
}
