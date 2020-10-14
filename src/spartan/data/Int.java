package spartan.data;

public class Int extends Value
{
  public Int(int value)
  {
    this.value = value;
  }
  
  public Type type()
  {
    return Type.Int;
  }
  
  public String repr()
  {
    return Integer.toString(value);
  }
  
  public Int neg()
  {
    return new Int(-value);
  }
  
  public Int add(Int that)
  {
    return new Int(this.value + that.value);
  }
  
  public Int sub(Int that)
  {
    return new Int(this.value - that.value);
  }
  
  public Int mul(Int that)
  {
    return new Int(this.value * that.value);
  }
  
  public Int div(Int that)
  {
    return new Int(this.value / that.value);
  }
  
  public Int mod(Int that)
  {
    return new Int(this.value % that.value);
  }
  
  public boolean eq(Int that)
  {
    return this.value == that.value;
  }
  
  public int compare(Int that)
  {
    return Integer.compare(this.value, that.value);
  }
  
  private final int value;
}
