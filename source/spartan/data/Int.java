package spartan.data;

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
    return Type.INT;
  }
  
  public String repr()
  {
    return Integer.toString(value);
  }
    
  public Int neg()
  {
    try {
      return new Int(Math.negateExact(value));
    }
    catch (ArithmeticException ex) {
      return new BigInt(this.value).neg();
    }
  }
  
  public Int abs()
  {
    return new Int(Math.abs(value));
  }
  
  public Int add(Int other)
  {
    try {
      return new Int(Math.addExact(this.value, other.value));
    }
    catch (ArithmeticException ex) {
      return new BigInt(this.value).add(new BigInt(other.value));
    }
  }
  
  public Int sub(Int other)
  {
    try {
      return new Int(Math.subtractExact(this.value, other.value));
    }
    catch (ArithmeticException ex) {
      return new BigInt(this.value).sub(new BigInt(other.value));
    }
  }
  
  public Int mul(Int other)
  {
    try {
      return new Int(Math.multiplyExact(this.value, other.value));
    }
    catch (ArithmeticException ex) {
      return new BigInt(this.value).mul(new BigInt(other.value));
    }
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
