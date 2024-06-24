package spartan.data;

import spartan.errors.Error;
import spartan.errors.IOError;
import spartan.errors.TypeMismatch;
import spartan.errors.InvalidArgument;
import java.io.IOException;
import java.io.FileNotFoundException;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.file.Path;
import java.nio.file.OpenOption;
import java.util.HashSet;
import java.util.Map;
import java.util.IdentityHashMap;
import java.util.Optional;
import static java.nio.file.StandardOpenOption.*;

public final class FilePort extends Port
{
  private static final Map<Symbol, OpenOption> openOptionMap = new IdentityHashMap<>();
  
  static {
    openOptionMap.put(Symbol.of("read"), READ);
    openOptionMap.put(Symbol.of("write"), WRITE);
    openOptionMap.put(Symbol.of("create"), CREATE);
    openOptionMap.put(Symbol.of("append"), APPEND);
    openOptionMap.put(Symbol.of("truncate"), TRUNCATE_EXISTING);
  }
  
  private OpenOption[] parseOpenOptions(List options)
  {
    var result = new HashSet<OpenOption>();
    for (var elem : options) {
      if (!(elem instanceof Symbol kw))
        throw new TypeMismatch();
      var opt = openOptionMap.get(kw);
      if (opt == null)      
        throw new InvalidArgument();
      result.add(opt);
    }
    return result.toArray(OpenOption[]::new);
  }
    
  public FilePort(String fileName, List options)
  {
    var openOptions = parseOpenOptions(options);
    
    try {
      this.channel = FileChannel.open(Path.of(fileName), openOptions);
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
  public long size()
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
