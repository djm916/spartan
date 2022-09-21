package spartan.data;

import spartan.errors.Error;

public final class Bytes extends Datum
{
  public Type type()
  {
    return Type.BYTES;
  }
  
  public Bytes(int n)
  {
    this.bytes = new byte[n];
    this.size = 0;
  }
  
  public Bytes(byte[] bytes, int n)
  {
    this.bytes = bytes;
    this.size = n;
  }
  
  public byte[] getBytes()
  {
    return bytes;
  }
  
  public byte getByte(int index)
  {
    return bytes[index];
  }
  
  public void setByte(int index, byte value)
  {
    bytes[index] = value;
  }
  
  public int length()
  {
    return size;
  }
  
  public Text decode(String encoding)
  {
    try {
      return new Text(new String(bytes, 0, size, encoding));
    }
    catch (java.io.UnsupportedEncodingException ex) {
      throw new Error("unsupported encoding " + encoding);
    }
  }
  
  private final byte[] bytes;
  private final int size;
}
