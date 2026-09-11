package spartan.tests;

import org.junit.*;
import static org.junit.Assert.*;
import spartan.data.Bool;
import spartan.data.Symbol;

public class StreamTests extends TestBase
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
    
  }
  
  // 
  // emptyStreamIsStream
  @Test
  public void test1()
  {
    var output = eval("(stream? *empty-stream*)");
    assertEquals(output, Bool.TRUE);
  }
  
  // 
  // emptyStreamIsEmpty
  @Test
  public void test2()
  {
    var output = eval("(stream-empty? *empty-stream*)");
    assertEquals(output, Bool.TRUE);
  }
  
  // 
  // emptyStreamIsEmpty
  @Test
  public void test3()
  {
    var output = eval("(stream-empty? (stream))");
    assertEquals(output, Bool.TRUE);
  }
  
  // 
  // finiteStreamLength
  @Test
  public void test4()
  {
    var output = eval("""
(def s (stream 1 2 3))
(= 3 (stream-length s))
""");
    assertEquals(output, Bool.TRUE);
  }
}
