package spartan.tests;

import org.junit.*;
import static org.junit.Assert.*;
import spartan.data.Bool;
import spartan.data.Symbol;
import spartan.data.Int;
import spartan.data.List;

public class DefrecordTests extends TestBase
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
    eval("""
(defrecord Point
  point
  point?
  (x point-x point-set-x!)
  (y point-y point-set-y!))
""");
  }
  
  @After
  public void teardown()
  {
    spartan.Runtime.removeAllUserModules();
  }
  
  // Constructor
  // 
  @Test
  public void test1()
  {
    var code = """
(def p (point 0 0))
(type p)
""";
    
    var output = eval(code);
    assertTrue(output instanceof Symbol);
    assertEquals(output, Symbol.of("testbed:Point"));
  }
  
  // Type predicate
  // 
  @Test
  public void test2()
  {
    var code = """
(def p (point 0 0))
(point? p)
""";
    
    var output = eval(code);
    assertEquals(output, Bool.TRUE);
  }
  
  // Accessors
  // 
  @Test
  public void test3()
  {
    var code = """
(def p (point 1 2))
(list (point-x p) (point-y p))
""";
    
    var output = eval(code);
    assertTrue(output instanceof List);
    assertEquals(output, List.of(Int.valueOf(1), Int.valueOf(2)));
  }
  
  // Mutators
  // 
  @Test
  public void test4()
  {
    var code = """
(def p (point 1 2))
(point-set-x! p 2)
(point-set-y! p 1)
(list (point-x p) (point-y p))
""";
    
    var output = eval(code);
    assertTrue(output instanceof List);
    assertEquals(output, List.of(Int.valueOf(2), Int.valueOf(1)));
  }
  
  // Matching
  // 
  @Test
  public void test5()
  {
    var code = """
(def p (point 1 2))
(match p
  ((record Point x y) (list x y)))
""";
    
    var output = eval(code);
    assertTrue(output instanceof List);
    assertEquals(output, List.of(Int.valueOf(1), Int.valueOf(2)));
  }
}
