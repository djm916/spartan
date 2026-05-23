package spartan.util;

public class Box<T>
{
  public Box()
  {
    this(null);
  }
  
  public Box(T value)
  {
    this.value = value;
  }
  
  public T get()
  {
    return value;
  }
  
  public void set(T value)
  {
    this.value = value;
  }
  
  private T value;
}
