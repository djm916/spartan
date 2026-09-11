package spartan.tests;

import org.junit.*;
import static org.junit.Assert.*;
import spartan.data.Bool;
import spartan.data.Symbol;
import spartan.data.Int;
import spartan.errors.WrongNumberArgs;

public class FunCallTests extends TestBase
{
  @BeforeClass
  public static void setupAll()
  {
    spartan.Runtime.boot();
  }
  
  @Before
  public void setup()
  {
    spartan.Runtime.enterModule(Symbol.of("testbed"));
  }
  
  @After
  public void teardown()
  {
    spartan.Runtime.removeModule(Symbol.of("testbed"));
  }
  
  // 
  // fixedNoArgs
  @Test
  public void test1()
  {
    var code =
"""
(defun f () 42)
(f)
""";
    var output = eval(code);
    assertEquals(output, Int.valueOf(42));
  }
  
  // 
  // fixedNoArgsError
  @Test(expected = WrongNumberArgs.class)
  public void test2()
  {
    var code =
"""
(defun f () 42)
(f 11)
""";
    var output = eval(code);
  }
}
