package spartan.data;

import spartan.errors.IOError;
import java.io.InputStream;
import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.FileNotFoundException;

public class InputPort extends Port
{
  public static final InputPort Stdin = new InputPort(System.in);
  
  public static InputPort fromFile(String fileName)
  {
    try {
      return new InputPort(new BufferedInputStream(new FileInputStream(fileName)));
    }
    catch (FileNotFoundException | SecurityException ex) {
      throw new IOError(ex.getMessage());
    }
  }

  public ByteVector read(int numBytes)
  {
    var bytes = new byte[numBytes];
    try {
      stream.read(bytes, 0, numBytes);
    }
    catch (IOException ex) {
      throw new IOError(ex.getMessage());
    }
    return new ByteVector(bytes);
  }
  
  public void close()
  {
    try {
      stream.close();
    }
    catch (IOException ex) {
      throw new IOError(ex.getMessage());
    }
  }
  
  private InputPort(InputStream stream)
  {
    this.stream = stream;
  }
  
  private final InputStream stream;
}
