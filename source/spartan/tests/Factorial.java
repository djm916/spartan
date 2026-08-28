package spartan.tests;

import org.junit.*;
import static org.junit.Assert.*;
import spartan.data.Int;
import spartan.data.Symbol;

public class Factorial extends TestBase
{
  @Before
  public void setup()
  {
    spartan.Runtime.enterModule(Symbol.of("testbed"));
  }
  
  @Test
  public void test1()
  {
    var code = """
(defun fact (n)
  (defun loop (n p)
    (if (= 0 n) p
      (loop (- n 1) (* n p))))
  (loop n 1))
(fact 20)
""";
    
    var output = eval(code);
    assertTrue(output instanceof Int);
    assertEquals(output.repr(), "2432902008176640000");
  }
}
