package spartan.data;

import spartan.errors.Error;

public class ByteVector extends Datum
{
  public Type type()
  {
    return Type.ByteVector;
  }
  
  public ByteVector(int n)
  {
    this.bytes = new byte[n];
  }
  
  public ByteVector(byte[] bytes)
  {
    this.bytes = bytes;
  }
  
  public byte[] getBytes()
  {
    return bytes;
  }
  
  public byte getByte(int index)
  {
    return bytes[index];
  }
  
  public Text decode(String encoding)
  {
    try {
      return new Text(new String(bytes, encoding));
    }
    catch (java.io.UnsupportedEncodingException ex) {
      throw new Error("unsupported encoding " + encoding);
    }
  }
  
  private final byte[] bytes;
}
