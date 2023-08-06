package spartan.data;

import spartan.errors.Error;
import spartan.errors.NoSuchElement;
import spartan.errors.IndexOutOfBounds;
import spartan.errors.InvalidArgument;
import spartan.errors.TypeMismatch;
import spartan.Config;
import java.nio.ByteBuffer;
import java.nio.BufferUnderflowException;
import java.nio.BufferOverflowException;
import java.nio.charset.Charset;

public final class Bytes implements Datum
{
  public static Bytes fromList(List elems)
  {
    //var result = new Bytes(elems.length());
    var buffer = ByteBuffer.allocate(elems.length());
    buffer.clear();
    for (var e : elems) {
      if (!(e instanceof IInt b))
        throw new TypeMismatch();
      buffer.put((byte) b.toInt32()); // TODO: check for loss of value
    }
    buffer.flip();
    return new Bytes(buffer);
  }
  
  @Override
  public String type()
  {
    return "bytes";
  }
  
  public Bytes(int capacity)
  {
    this.buffer = ByteBuffer.allocate(capacity);
  }
  
  public Bytes(byte[] bytes)
  {
    this.buffer = ByteBuffer.wrap(bytes);
  }
  
  public Bytes(ByteBuffer bytes)
  {
    this.buffer = bytes;
  }
  
  public byte[] array()
  {
    return buffer.array();
  }
  
  public ByteBuffer buffer()
  {
    return buffer;
  }
  
  public byte ref(int index)
  {
    try {
      return buffer.get(index);
    }
    catch (IndexOutOfBoundsException ex) {
      throw new NoSuchElement();
    }
  }
  
  public void set(int index, byte value)
  {
    try {
      buffer.put(index, value);
    }
    catch (IndexOutOfBoundsException ex) {
      throw new NoSuchElement();
    }
  }
  
  public int length()
  {
    return buffer.capacity();
  }
    
  public String decode(int start, int count, Charset encoding)
  {
    try {
      return new String(buffer.array(), start, count, encoding);
    }
    catch (IndexOutOfBoundsException ex) {
      throw new InvalidArgument();
    }
  }
  
  private final ByteBuffer buffer;
}
