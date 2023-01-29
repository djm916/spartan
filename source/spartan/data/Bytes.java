package spartan.data;

import spartan.errors.Error;
import spartan.errors.NoSuchElement;
import spartan.errors.IndexOutOfBounds;
import spartan.errors.TypeMismatch;
import java.nio.ByteBuffer;
import java.nio.BufferUnderflowException;
import java.nio.BufferOverflowException;
import java.nio.charset.Charset;

public final class Bytes implements Datum, IAssoc, ISize
{
  public static Bytes fromList(List elems)
  {
    var result = new Bytes(elems.length());
    for (var e : elems) {
      if (!(e instanceof IInt b))
        throw new TypeMismatch();
      result.push((byte) b.intValue()); // TODO: check for loss of value
    }
    result.flip();
    return result;
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
  
  @Override // IAssoc
  public Int get(Datum key)
  {
    if (!(key instanceof IInt i))
      throw new TypeMismatch();
    return new Int(get(i.intValue())); // TODO: cache small integers to avoid allocation
  }
  
  public byte get(int index)
  {
    try {
      return buffer.get(index);
    }
    catch (IndexOutOfBoundsException ex) {
      throw new NoSuchElement();
    }
  }
  
  @Override // IAssoc
  public void set(Datum key, Datum value)
  {
    if (!(key instanceof IInt i && value instanceof IInt b))
      throw new TypeMismatch();
    set(i.intValue(), (byte) b.intValue()); // TODO: check for loss of value
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
  
  @Override // ISize
  public int length()
  {
    return capacity();
  }
  
  @Override // ISize
  public boolean empty()
  {
    return remaining() == 0;
  }
  
  public void push(byte value)
  {
    try {
      buffer.put(value);
    }
    catch (BufferOverflowException ex) {
      throw new IndexOutOfBounds();
    }
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
