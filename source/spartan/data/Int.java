package spartan.data;

import java.math.BigInteger;
import spartan.errors.DivisionByZero;
import spartan.errors.IntegerOverflow;

public final class Int extends Datum
{
  public Int(long value)
  {
    this(BigInteger.valueOf(value));
  }
  
  public Int(BigInteger value)
  {
    this.value = value;
  }
  
  public Int(String value)
  {
    this(new BigInteger(value));
  }
  
  public Type type()
  {
    return Type.Int;
  }
  
  public String repr()
  {
    return value.toString();
  }
    
  public int intValue()
  {
    try {
      return value.intValueExact();
    }
    catch (ArithmeticException ex) {
      throw new IntegerOverflow();
    }
  }
  
  public long longValue()
  {
    try {
      return value.longValueExact();
    }
    catch (ArithmeticException ex) {
      throw new IntegerOverflow();
    }
  }
  
  public double doubleValue()
  {
    return value.doubleValue();
  }
  
  public Ratio toRatio()
  {
    return new Ratio(value, BigInteger.ONE);
  }
  
  public Real toReal()
  {
    return new Real(doubleValue());
  }
  
  public Complex toComplex()
  {
    return new Complex(doubleValue(), 0.0);
  }
  
  public Int neg()
  {
    return new Int(value.negate());
  }
  
  public Int abs()
  {
    return new Int(value.abs());
  }
  
  public Int add(Int other)
  {
    return new Int(this.value.add(other.value));
  }
  
  public Int sub(Int other)
  {
    return new Int(this.value.subtract(other.value));
  }
  
  public Int mul(Int other)
  {
    return new Int(this.value.multiply(other.value));
  }
  
  public Int div(Int other)
  {
    try {
      return new Int(this.value.divide(other.value));
    }
    catch (ArithmeticException ex) {
      throw new DivisionByZero();
    }
  }
  
  public Int mod(Int other)
  {
    try {
      return new Int(this.value.remainder(other.value));
    }
    catch (ArithmeticException ex) {
      throw new DivisionByZero();
    }
  }
  
  public boolean eq(Int other)
  {
    return this.value.equals(other.value);
  }
  
  public int compare(Int other)
  {
    return this.value.compareTo(other.value);
  }
  
  final BigInteger value;
}
