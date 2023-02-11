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

public final class Bytes implements Datum, IAssoc, ISize
{
  public static Bytes fromList(List elems)
  {
    //var result = new Bytes(elems.length());
    var buffer = ByteBuffer.allocate(elems.length());
    buffer.clear();
    for (var e : elems) {
      if (!(e instanceof IInt b))
        throw new TypeMismatch();
      buffer.put((byte) b.intValue()); // TODO: check for loss of value
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
    return buffer.capacity();
  }
  
  public Text decode(IInt start, IInt count, Charset encoding)
  {
    return new Text(decode(start.intValue(), count.intValue(), encoding));
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
