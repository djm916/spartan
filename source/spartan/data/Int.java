package spartan.data;

import spartan.errors.DivisionByZero;
import spartan.errors.IntegerOverflow;
import spartan.errors.InvalidArgument;

/**
 * Implementation of IInt as 64-bit signed integer (a Java long).
 *
 * Using a long for the representation has a few advantages. First, they can simultaneously represent bytes, array indices,
 * hash codes, file sizes and positions, avoiding an explosion of custom wrappers for each size. The trade-off, of course,
 * is that they probably waste a lot of space most of the time. Converting long to and from double produces sensible results,
 * with minimal loss of precision. This makes it easy to convert Int to Real and vice-versa, which is useful for implementing
 * math operations. 
 */
public final class Int implements Datum, INum, IInt, IRatio, IReal, IComplex, IEq, IOrd
{
  private static final IntCache cache = new IntCache();    
  public static final Int ZERO = valueOf(0);
  
  public static Int valueOf(long value)
  {
    return cache.get(value, Int::new);
  }
  
  public static Int valueOf(String value)
  {
    return valueOf(Long.valueOf(value));
  }
  
  private Int(long value)
  {
    this.value = value;
  }
  
  @Override
  public Type type()
  {
    return Type.FIXNUM;
  }
  
  @Override
  public String repr()
  {
    return Long.toString(value);
  }

  @Override
  public int intValue()
  {
    try {
      return Math.toIntExact(value);
    }
    catch (ArithmeticException ex) {
      throw new IntegerOverflow();
    }
  }  
  
  @Override
  public long longValue()
  {
    return value;
  }
  
  @Override
  public byte byteValue()
  {
    if (!(0x00 <= value && value <= 0xFF))
      throw new IntegerOverflow();
    return (byte)value;
  }
  
  @Override
  public double doubleValue()
  {
    return (double)value;
  }
    
  @Override
  public String formatInt(int base)
  {
    return Long.toString(value, base);
  }
  
  @Override
  public String formatDec(int precision)
  {
    return toFloat64().formatDec(precision);
  }
  
  @Override
  public boolean equals(Object rhs)
  {
    return (rhs instanceof Int that) && this.value == that.value;
  }
  
  @Override
  public int hashCode()
  {
    return Long.hashCode(value);
  }
  
  public BigInt toBigInt()
  {
    return new BigInt(value);
  }
  
  public Ratio toRatio()
  {
    return new Ratio(value, 1);
  }
  
  public Float64 toFloat64()
  {
    return new Float64(doubleValue());
  }
  
  public Complex toComplex()
  {
    return new Complex(doubleValue(), 0.0);
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
  public Float64 add(Float64 rhs)
  {
    return toFloat64().add(rhs);
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
  public Float64 sub(Float64 rhs)
  {
    return toFloat64().sub(rhs);
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
  public Float64 mul(Float64 rhs)
  {
    return toFloat64().mul(rhs);
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
  public Float64 div(Float64 rhs)
  {
    return toFloat64().div(rhs);
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
    if (this.value == Long.MIN_VALUE && rhs.value == -1)
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
  public Ratio over(Int rhs)
  {
    return new Ratio(this.value, rhs.value);
  }
  
  @Override
  public Ratio over(BigInt rhs)
  {
    return toBigInt().over(rhs);
  }
  
  @Override
  public Int numerator()
  {
    return this;
  }
  
  @Override
  public Int denominator()
  {
    return valueOf(1);
  }
  
  @Override // IComplex
  public Float64 realPart()
  {
    return toFloat64();
  }
  
  @Override // IComplex
  public Int imagPart()
  {
    return ZERO;
  }
  
  @Override // IComplex
  public Float64 angle()
  {
    return toComplex().angle();
  }
  
  @Override // IComplex
  public Float64 magnitude()
  {
    return toComplex().magnitude();
  }
    
  @Override
  public boolean isEqual(Int rhs)  
  {
    return this.value == rhs.value;
  }
  
  @Override
  public boolean isEqual(BigInt rhs)
  {
    return toBigInt().isEqual(rhs);
  }
  
  @Override
  public boolean isEqual(Ratio rhs)  
  {
    return toRatio().isEqual(rhs);
  }
  
  @Override
  public boolean isEqual(Float64 rhs)  
  {
    return toFloat64().isEqual(rhs);
  }
  
  @Override
  public boolean isEqual(Complex rhs)  
  {
    return toComplex().isEqual(rhs);
  }

  @Override
  public int compareTo(Int rhs)
  {
    return Long.compare(this.value, rhs.value);
  }
  
  @Override
  public int compareTo(BigInt rhs)
  {
    return toBigInt().compareTo(rhs);
  }
  
  @Override
  public int compareTo(Ratio rhs)
  {
    return toRatio().compareTo(rhs);
  }
  
  @Override
  public int compareTo(Float64 rhs)
  {
    return toFloat64().compareTo(rhs);
  }
  
  @Override // INum
  public boolean isZero()
  {
    return value == 0;
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
  
  @Override // IReal
  public boolean isPositive()
  {
    return value > 0;
  }
  
  @Override // IReal
  public boolean isNegative()
  {
    return value < 0;
  }
    
  @Override // IReal
  public IRatio rationalize()
  {
    return this;
  }
  
  @Override // INum
  public INum toInexact()
  {
    return toFloat64();
  }
  
  @Override // IInt
  public IInt bitNot()
  {
    return valueOf(~value);
  }
  
  @Override // IInt
  public IInt bitAnd(Int rhs)
  {
    return valueOf(this.value & rhs.value);
  }
  
  @Override // IInt
  public IInt bitAnd(BigInt rhs)
  {
    return toBigInt().bitAnd(rhs);
  }
  
  @Override // IInt
  public IInt bitOr(Int rhs)
  {
    return valueOf(this.value | rhs.value);
  }
  
  @Override // IInt
  public IInt bitOr(BigInt rhs)
  {
    return toBigInt().bitOr(rhs);
  }
  
  @Override // IInt
  public IInt bitXor(Int rhs)
  {
    return valueOf(this.value ^ rhs.value);
  }
  
  @Override // IInt
  public IInt bitXor(BigInt rhs)
  {
    return toBigInt().bitXor(rhs);
  }
  
  // Create a bitmask with just the bit at the given index set
  private static long maskBit(int index)
  {
    return 1 << index;
  }
  
  @Override // IInt
  public boolean isBitSet(int index)
  {
    if (index < 0)
      throw new InvalidArgument();
    return (value & maskBit(index)) != 0;
  }
  
  @Override // IInt
  public IInt setBit(int index)
  {
    if (index < 0)
      throw new InvalidArgument();
    return new Int(value & maskBit(index));
  }
  
  @Override // IInt
  public IInt clearBit(int index)
  {
    if (index < 0)
      throw new InvalidArgument();
    return new Int(value & ~maskBit(index));
  }
  
  @Override // IInt
  public IInt flipBit(int index)
  {
    if (index < 0)
      throw new InvalidArgument();
    return new Int(value ^ maskBit(index));
  }

  private final long value;
}
