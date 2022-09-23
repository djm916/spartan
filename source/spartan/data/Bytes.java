package spartan.data;

import spartan.errors.Error;

public final class Bytes extends Datum
{
  public Type type()
  {
    return Type.BYTES;
  }
  
  public Bytes(int capacity)
  {
    this.bytes = new byte[capacity];
    this.position = 0;
  }
  
  public Bytes(byte[] bytes)
  {
    this.bytes = bytes;
    this.position = 0;
  }
  
  public byte[] getBytes()
  {
    return bytes;
  }
  
  public Int get(Int index)
  {
    return new Int(get(index.value));
  }
  
  public byte get(int index)
  {
    return bytes[index];
  }
  
  public void set(int index, byte value)
  {
    bytes[index] = value;
  }
  
  public int position()
  {
    return position;
  }
  
  public void position(int position)
  {
    this.position = position;
  }
  
  public int length()
  {
    return position;
  }
  
  public int capacity()
  {
    return bytes.length;
  }
  
  public int remaining()
  {
    return bytes.length - position;
  }
  
  public void clear()
  {
    position = 0;
  }
  
  public Text decode(String encoding)
  {
    try {
      return new Text(new String(bytes, 0, position, encoding));
    }
    catch (java.io.UnsupportedEncodingException ex) {
      throw new Error("unsupported encoding " + encoding);
    }
  }
  
  private final byte[] bytes;
  private int position;
}
