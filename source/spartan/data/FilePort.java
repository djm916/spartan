package spartan.data;

import spartan.errors.Error;
import spartan.errors.IOError;
import java.io.RandomAccessFile;
import java.io.IOException;
import java.io.FileNotFoundException;

public final class FilePort extends Port
{
  public FilePort(String fileName)
  {
    try {
      this.file = new RandomAccessFile(fileName, "rw");
    }
    catch (FileNotFoundException | SecurityException ex) {
      throw new IOError(ex.getMessage());
    }
  }
  
  public Int read(Bytes bytes, Int count)
  {
    if (bytes.remaining() < count.value)
      throw new Error("buffer overflow");
    
    try {      
      var bytesRead = file.read(bytes.getBytes(), bytes.position(), count.value);
      if (bytesRead == -1)
        return EOF;
      bytes.position(bytes.position() + bytesRead);
      return new Int(bytesRead);
    }
    catch (IOException ex) {
      throw new IOError(ex.getMessage());
    }
  }
  
  public void write(Bytes bytes)
  {
    try {
      file.write(bytes.getBytes());
    }
    catch (IOException ex) {
      throw new IOError(ex.getMessage());
    }
  }
  
  public void close()
  {
    try {
      file.close();
    }
    catch (IOException ex) {
      throw new IOError(ex.getMessage());
    }
  }
  
  private final RandomAccessFile file;
}
