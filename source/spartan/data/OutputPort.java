package spartan.data;

import spartan.errors.IOError;
import java.io.OutputStream;
import java.io.BufferedOutputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.FileNotFoundException;

public class OutputPort extends Port
{
  public static final OutputPort Stdout = new OutputPort(System.out);
  
  public static OutputPort fromFile(String fileName)
  {
    try {
      return new OutputPort(new BufferedOutputStream(new FileOutputStream(fileName)));
    }
    catch (FileNotFoundException | SecurityException ex) {
      throw new IOError(ex.getMessage());
    }
  }

  public void write(ByteVector bytes)
  {
    try {
      stream.write(bytes.getBytes());
    }
    catch (IOException ex) {
      throw new IOError(ex.getMessage());
    }
  }
    
  private OutputPort(OutputStream stream)
  {
    this.stream = stream;
  }
  
  private final OutputStream stream;
}
