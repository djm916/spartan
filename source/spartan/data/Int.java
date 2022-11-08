package spartan.data;

import spartan.errors.DivisionByZero;
import spartan.errors.IntegerOverflow;

public final class Int implements Datum, Integral
{
  public Int(int value)
  {
    this.value = value;
  }
  
  public Int(String value)
  {
    this(Integer.parseInt(value));
  }
  
  @Override
  public Type type()
  {
    return Type.INT;
  }
  
  @Override
  public String repr()
  {
    return Integer.toString(value);
  }
  
  @Override
  public int intValue()
  {
    return value;
  }
  
  @Override
  public boolean equals(Object other)
  {
    return (other instanceof Int that) && this.value == that.value;
  }
  
  @Override
  public int hashCode()
  {
    return Integer.hashCode(value);
  }
  
  public BigInt toBigInt()
  {
    return new BigInt(value);
  }
  
  public Ratio toRatio()
  {
    return new Ratio(value, 1);
  }
  
  public Real toReal()
  {
    return new Real((double)value);
  }
  
  public Complex toComplex()
  {
    return new Complex((double)value, 0.0);
  }
  
  public Integral neg()
  {
    try {
      return new Int(Math.negateExact(value));
    }
    catch (ArithmeticException ex) {
      return toBigInt().neg();
    }
  }
  
  public Integral abs()
  {
    try {
      return new Int(Math.absExact(value));
    }
    catch (ArithmeticException ex) {
      return toBigInt().abs();
    }
  }
  
  public Integral add(Int that)
  {
    try {
      return new Int(Math.addExact(this.value, that.value));
    }
    catch (ArithmeticException ex) {
      return this.toBigInt().add(that.toBigInt());
    }
  }
  
  public Integral sub(Int that)
  {
    try {
      return new Int(Math.subtractExact(this.value, that.value));
    }
    catch (ArithmeticException ex) {
      return this.toBigInt().sub(that.toBigInt());
    }
  }
  
  public Integral mul(Int that)
  {
    try {
      return new Int(Math.multiplyExact(this.value, that.value));
    }
    catch (ArithmeticException ex) {
      return this.toBigInt().mul(that.toBigInt());
    }
  }
  
  public Ratio div(Int that)
  {
    return new Ratio(this.value, that.value);
  }
  
  public Integral quotient(Int that)
  {
    if (that.value == 0)
      throw new DivisionByZero();
    if (this.value == Integer.MIN_VALUE && that.value == -1)
      return this.toBigInt().quotient(that.toBigInt());
    return new Int(this.value / that.value);
  }
  
  public Int remainder(Int that)
  {
    try {
      return new Int(this.value % that.value);
    }
    catch (ArithmeticException ex) {
      throw new DivisionByZero();
    }
  }
  
  public boolean eq(Int that)
  {
    return this.value == that.value;
  }
  
  public int compare(Int that)
  {
    return Integer.compare(this.value, that.value);
  }
  
  public final int value;
}
