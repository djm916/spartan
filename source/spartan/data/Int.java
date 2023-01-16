package spartan.data;

import java.math.BigInteger;
import spartan.errors.DivisionByZero;
import spartan.errors.IntegerOverflow;

public final class Int implements Datum, INum, IInt, IReal, IEq, IOrd<Int>
{
  public Int(int value)
  {
    this.value = value;
  }
  
  public Int(String value)
  {
    this(Integer.parseInt(value));
  }
  
  @Override // Datum
  public Type type()
  {
    return Type.INT;
  }
  
  @Override // Datum
  public String repr()
  {
    return Integer.toString(value);
  }
  
  @Override // IInt
  public int intValue()
  {
    return value;
  }  
  
  @Override
  public double doubleValue()
  {
    return (double)value;
  }
  
  @Override // Object
  public boolean equals(Object other)
  {
    return (other instanceof Int that) && this.value == that.value;
  }
  
  @Override // Object
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
  
  @Override
  public IInt neg()
  {
    try {
      return new Int(Math.negateExact(value));
    }
    catch (ArithmeticException ex) {
      return toBigInt().neg();
    }
  }
  
  @Override
  public IInt abs()
  {
    try {
      return new Int(Math.absExact(value));
    }
    catch (ArithmeticException ex) {
      return toBigInt().abs();
    }
  }
  
  @Override
  public IInt add(Int rhs)
  {
    try {
      return new Int(Math.addExact(this.value, rhs.value));
    }
    catch (ArithmeticException ex) {
      return add(rhs.toBigInt());
    }
  }
  
  @Override
  public BigInt add(BigInt rhs)
  {
    return toBigInt().add(rhs);
  }
  
  @Override
  public Ratio add(Ratio rhs)
  {
    return toRatio().add(rhs);
  }
  
  @Override
  public Real add(Real rhs)
  {
    return toReal().add(rhs);
  }
  
  @Override
  public Complex add(Complex rhs)
  {
    return toComplex().add(rhs);
  }
  
  @Override
  public IInt sub(Int rhs)
  {
    try {
      return new Int(Math.subtractExact(this.value, rhs.value));
    }
    catch (ArithmeticException ex) {
      return sub(rhs.toBigInt());
    }
  }
  
  @Override
  public BigInt sub(BigInt rhs)
  {
    return toBigInt().sub(rhs);
  }
  
  @Override
  public Ratio sub(Ratio rhs)
  {
    return toRatio().sub(rhs);
  }
  
  @Override
  public Real sub(Real rhs)
  {
    return toReal().sub(rhs);
  }
  
  @Override
  public Complex sub(Complex rhs)
  {
    return toComplex().sub(rhs);
  }
  
  @Override
  public IInt mul(Int rhs)
  {
    try {
      return new Int(Math.multiplyExact(this.value, rhs.value));
    }
    catch (ArithmeticException ex) {
      return mul(rhs.toBigInt());
    }
  }
  
  @Override
  public BigInt mul(BigInt rhs)
  {
    return toBigInt().mul(rhs);
  }
  
  @Override
  public Ratio mul(Ratio rhs)
  {
    return toRatio().mul(rhs);
  }
  
  @Override
  public Real mul(Real rhs)
  {
    return toReal().mul(rhs);
  }
  
  @Override
  public Complex mul(Complex rhs)
  {
    return toComplex().mul(rhs);
  }
  
  @Override
  public Ratio div(Int rhs)
  {
    return new Ratio(this.value, rhs.value);
  }
  
  @Override
  public Ratio div(BigInt rhs)
  {
    return toBigInt().div(rhs);
  }
  
  @Override
  public Ratio div(Ratio rhs)
  {
    return toRatio().div(rhs);
  }
  
  @Override
  public Real div(Real rhs)
  {
    return toReal().div(rhs);
  }
  
  @Override
  public Complex div(Complex rhs)
  {
    return toComplex().div(rhs);
  }
  
  @Override
  public IInt quotient(Int rhs)
  {
    if (rhs.value == 0)
      throw new DivisionByZero();
    if (this.value == Integer.MIN_VALUE && rhs.value == -1)
      return quotient(rhs.toBigInt());
    return new Int(this.value / rhs.value);
  }
  
  @Override
  public BigInt quotient(BigInt rhs)
  {
    return toBigInt().quotient(rhs);
  }
  
  @Override
  public Int remainder(Int that)
  {
    try {
      return new Int(this.value % that.value);
    }
    catch (ArithmeticException ex) {
      throw new DivisionByZero();
    }
  }
  
  @Override
  public BigInt remainder(BigInt rhs)
  {
    return toBigInt().remainder(rhs);
  }
  
  @Override
  public Int floor()
  {
    return this;
  }
  
  @Override
  public Int ceiling()
  {
    return this;
  }
  
  @Override
  public Int round()
  {
    return this;
  }
  
  @Override
  public Real sin()
  {
    return toReal().sin();
  }
  
  @Override
  public Real cos()
  {
    return toReal().cos();
  }
  
  @Override
  public Real tan()
  {
    return toReal().tan();
  }
  
  @Override
  public Real asin()
  {
    return toReal().asin();
  }
  
  @Override
  public Real acos()
  {
    return toReal().acos();
  }
  
  @Override
  public Real atan()
  {
    return toReal().atan();
  }
  
  @Override
  public Real exp(Int rhs)
  {
    return toReal().exp(rhs);
  }
  
  @Override
  public Real exp(BigInt rhs)
  {
    return toReal().exp(rhs);
  }
  
  @Override
  public Real exp(Ratio rhs)
  {
    return toReal().exp(rhs);
  }
  
  @Override
  public Real exp(Real rhs)
  {
    return toReal().exp(rhs);
  }
  
  @Override
  public Complex exp(Complex rhs)
  {
    return toComplex().exp(rhs);
  }
  
  @Override
  public Real log(Int rhs)
  {
    return toReal().log(rhs);
  }
  
  @Override
  public Real log(BigInt rhs)
  {
    return toReal().log(rhs);
  }
  
  @Override
  public Real log(Ratio rhs)
  {
    return toReal().log(rhs);
  }
  
  @Override
  public Real log(Real rhs)
  {
    return toReal().log(rhs);
  }
  
  @Override
  public Complex log(Complex rhs)
  {
    return toComplex().log(rhs);
  }
    
  @Override // IEq
  public boolean isEqual(Int rhs)  
  {
    return this.value == rhs.value;
  }
  
  @Override // IEq
  public boolean isEqual(BigInt rhs)
  {
    return toBigInt().isEqual(rhs);
  }
  
  @Override // IEq
  public boolean isEqual(Ratio rhs)  
  {
    return toRatio().isEqual(rhs);
  }
  
  @Override // IEq
  public boolean isEqual(Real rhs)  
  {
    return toReal().isEqual(rhs);
  }
  
  @Override // IEq
  public boolean isEqual(Complex rhs)  
  {
    return toComplex().isEqual(rhs);
  }
  
  @Override // IOrd
  public int compareTo(Int that)
  {
    return Integer.compare(this.value, that.value);
  }
    
  private final int value;
}
