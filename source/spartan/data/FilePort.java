package spartan.data;

import spartan.errors.IOError;
import java.io.RandomAccessFile;
import java.io.IOException;
import java.io.FileNotFoundException;

public class FilePort extends Port
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
    var bytes = new byte[numBytes];
    try {
      file.read(bytes, 0, numBytes);
    }
    catch (IOException ex) {
      throw new IOError(ex.getMessage());
    }
    return new Bytes(bytes);
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
