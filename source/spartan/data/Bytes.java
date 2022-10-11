package spartan.data;

import spartan.errors.Error;
import spartan.errors.NoSuchElement;
import spartan.errors.TypeMismatch;
import java.nio.ByteBuffer;
import java.nio.BufferUnderflowException;
import java.nio.charset.Charset;

public final class Bytes extends Datum
{
  public static Bytes fromList(List elems)
  {
    var result = new Bytes(elems.length());
    for (; !elems.empty(); elems = elems.cdr()) {
      if (!elems.car().type().isInt())
        throw new TypeMismatch();
      var value = ((Integral) elems.car()).intValue();
      result.push((byte) value);
    }
    result.flip();
    return result;
  }
  
  public Type type()
  {
    return Type.BYTES;
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
  
  public byte get(int index)
  {
    return buffer.get(index);
  }
  
  public void set(int index, byte value)
  {
    buffer.put(index, value);
  }
  
  public void push(byte value)
  {
    buffer.put(value);
  }
  
  public byte pop()
  {
    try {
      return buffer.get();
    }
    catch (BufferUnderflowException ex) {
      throw new NoSuchElement();
    }
  }
  
  public int position()
  {
    return buffer.position();
  }
  
  public void position(int newPosition)
  {
    buffer.position(newPosition);
  }
  
  public int limit()
  {
    return buffer.limit();
  }
  
  public void limit(int newLimit)
  {
    buffer.limit(newLimit);
  }
  
  public int capacity()
  {
    return buffer.capacity();
  }
  
  public int remaining()
  {
    return buffer.remaining();
  }
  
  public void clear()
  {
    buffer.clear();
  }
  
  public void flip()
  {
    buffer.flip();
  }
  
  public Text decode(Charset encoding)
  {
    return new Text(encoding.decode(buffer).toString());
  }
  
  private final ByteBuffer buffer;
}
