package spartan.data;

import spartan.errors.IOError;
import java.io.InputStream;
import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.FileNotFoundException;

public final class InputPort extends Port
{
  public static final InputPort STDIN = new InputPort(System.in);
  
  public static InputPort fromFile(String fileName)
  {
    try {
      return new InputPort(new BufferedInputStream(new FileInputStream(fileName)));
    }
    catch (FileNotFoundException | SecurityException ex) {
      throw new IOError(ex.getMessage());
    }
  }

  public Int read(Bytes bytes, Int count)
  {
    try {
      var bytesRead = stream.read(bytes.getBytes(), 0, count.value);
      return new Int(bytesRead);
    }
    catch (IOException ex) {
      throw new IOError(ex.getMessage());
    }    
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
