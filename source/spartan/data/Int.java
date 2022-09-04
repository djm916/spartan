package spartan.data;

import java.math.BigInteger;
import spartan.errors.DivisionByZero;
import spartan.errors.IntegerOverflow;

public final class Int extends Datum
{
  public Int(int value)
  {
    this.value = value;
  }
  
  public Int(String value)
  {
    this.value = Integer.parseInt(value);
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
  
  public Int abs()
  {
    return new Int(Math.abs(value));
  }
  
  public Int add(Int other)
  {
    return new Int(this.value + other.value);
  }
  
  public Int sub(Int other)
  {
    return new Int(this.value - other.value);
  }
  
  public Int mul(Int other)
  {
    return new Int(this.value * other.value);
  }
  
  public Real div(Int other)
  {
    return new Real((double)this.value / (double)other.value);
  }
  
  public Int quotient(Int other)
  {
    try {
      return new Int(this.value / other.value);
    }
    catch (ArithmeticException ex) {
      throw new DivisionByZero();
    }
  }
  
  public Int remainder(Int other)
  {
    try {
      return new Int(this.value % other.value);
    }
    catch (ArithmeticException ex) {
      throw new DivisionByZero();
    }
  }
  
  public boolean eq(Int other)
  {
    return this.value == other.value;
  }
  
  public int compare(Int other)
  {
    return Integer.compare(this.value, other.value);
  }
  
  public final int value;
}
