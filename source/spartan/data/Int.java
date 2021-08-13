package spartan.data;

public class Int extends Datum
{
  public final int value;
  
  public Int(int value)
  {
    this.value = value;
  }
  
  public final Type type()
  {
    return Type.Int;
  }
  
  public final String repr()
  {
    return Integer.toString(value);
  }
  
  public final Int neg()
  {
    return new Int(-value);
  }
  
  public final Int abs()
  {
    return new Int(Math.abs(value));
  }
  
  public final Int add(Int that)
  {
    return new Int(this.value + that.value);
  }
  
  public final Int sub(Int that)
  {
    return new Int(this.value - that.value);
  }
  
  public final Int mul(Int that)
  {
    return new Int(this.value * that.value);
  }
  
  public final Int div(Int that)
  {
    return new Int(this.value / that.value);
  }
  
  public final Int mod(Int that)
  {
    return new Int(this.value % that.value);
  }
  
  public final boolean eq(Int that)
  {
    return this.value == that.value;
  }
  
  public final int compare(Int that)
  {
    return Integer.compare(this.value, that.value);
  }
}
