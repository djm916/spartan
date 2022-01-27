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
  
  public ByteVector read(int numBytes)
  {
    var bytes = new byte[numBytes];
    try {
      file.read(bytes, 0, numBytes);
    }
    catch (IOException ex) {
      throw new IOError(ex.getMessage());
    }
    return new ByteVector(bytes);
  }
  
  public void write(ByteVector bytes)
  {
    try {
      file.write(bytes.getBytes());
    }
    catch (IOException ex) {
      throw new IOError(ex.getMessage());
    }
  }
    
  private final RandomAccessFile file;
}
