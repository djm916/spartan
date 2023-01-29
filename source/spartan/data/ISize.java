package spartan.data;

public interface ISize
{
  int length();
  
  default boolean empty()
  {
    return length() == 0;
  }  
}
