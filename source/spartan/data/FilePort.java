package spartan.data;

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
  
  public Bytes read(int numBytes)
  {    
    try {
      var buffer = new byte[numBytes];
      var bytesRead = file.read(buffer, 0, numBytes);
      return new Bytes(buffer, bytesRead);
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
