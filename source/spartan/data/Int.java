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
  
  public BigInt toBigInt()
  {
    return new BigInt((long)value);
  }
  
  public Ratio toRatio()
  {
    return new Ratio(BigInteger.valueOf((long)value), BigInteger.ONE);
  }
  
  public Real toReal()
  {
    return new Real((double)value);
  }
  
  public Complex toComplex()
  {
    return new Complex((double)value, 0.0);
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
  
  public Int div(Int other)
  {
    try {
      return new Int(this.value / other.value);
    }
    catch (ArithmeticException ex) {
      throw new DivisionByZero();
    }
  }
  
  public Int mod(Int other)
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
