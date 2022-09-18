package spartan.data;

import spartan.errors.DivisionByZero;
import spartan.errors.IntegerOverflow;

public final class Int extends IntBase
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
    return Type.INT;
  }
  
  public String repr()
  {
    return Integer.toString(value);
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
  
  public IntBase neg()
  {
    try {
      return new Int(Math.negateExact(value));
    }
    catch (ArithmeticException ex) {
      return new BigInt(this.value).neg();
    }
  }
  
  public IntBase abs()
  {
    return new Int(Math.abs(value));
  }
  
  public IntBase add(Int other)
  {
    try {
      return new Int(Math.addExact(this.value, other.value));
    }
    catch (ArithmeticException ex) {
      return new BigInt(this.value).add(new BigInt(other.value));
    }
  }
  
  public IntBase sub(Int other)
  {
    try {
      return new Int(Math.subtractExact(this.value, other.value));
    }
    catch (ArithmeticException ex) {
      return new BigInt(this.value).sub(new BigInt(other.value));
    }
  }
  
  public IntBase mul(Int other)
  {
    try {
      return new Int(Math.multiplyExact(this.value, other.value));
    }
    catch (ArithmeticException ex) {
      return new BigInt(this.value).mul(new BigInt(other.value));
    }
  }
  
  /*
  public Real div(Int other)
  {
    return new Real((double)this.value / (double)other.value);
  }
  */
  public Ratio div(Int other)
  {
    return new Ratio(this.value, other.value);
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
