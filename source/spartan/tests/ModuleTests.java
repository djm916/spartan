package spartan.tests;

import org.junit.*;
import static org.junit.Assert.*;
import spartan.data.Symbol;
import spartan.errors.UnboundSymbol;

public class ModuleTests extends TestBase
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
    spartan.Runtime.removeAllUserModules();
  }
  
  // Can access non-exported variable from within same module
  // CanAccessPublicLocally
  @Test
  public void test1()
  {
    var code = """
(in-module a)
(def x 'pass)
x
""";
    
    var output = eval(code);
    assertTrue(output instanceof Symbol);
    assertEquals(output, Symbol.of("pass"));
  }
  
  // Can access exported variable from within same module
  // CanAccessPrivateLocally
  @Test
  public void test2()
  {
    var code = """
(in-module a)
(export x)
(def x 'pass)
x
""";
    
    var output = eval(code);
    assertTrue(output instanceof Symbol);
    assertEquals(output, Symbol.of("pass"));
  }
  
  // Can access variable exported from another module
  // CanAccessImportedVariable
  @Test
  public void test3()
  {
    var code = """
(in-module a)
(export x)
(def x 'pass)

(in-module b)
(def y a:x)
y
""";
    
    var output = eval(code);
    assertTrue(output instanceof Symbol);
    assertEquals(output, Symbol.of("pass"));
  }
  
  // Cannot access variable not exported from another module
  // CannotAccessPrivateVariable
  @Test(expected = UnboundSymbol.class)
  public void test4()
  {
    var code = """
(in-module a)
(def x 'pass)

(in-module b)
(def y a:x)
y
""";
  
    eval(code);
  }
  
  // Cannot import variable not exported from another module
  // CannotImportPrivateVariable
  @Test(expected = UnboundSymbol.class)
  public void test5()
  {
    var code = """
(in-module a)
(def x 'pass)

(in-module b)
(import a :only (x))
(def y x)
y
""";
  
    eval(code);
  }
  
  // Can access variable imported from another module without qualification
  // canAccessImportedVariable
  @Test
  public void test6()
  {
    var code = """
(in-module a)
(export x)
(def x 'pass)

(in-module b)
(import a :only (x))
(def y x)
y
""";
  
    var output = eval(code);
    assertTrue(output instanceof Symbol);
    assertEquals(output, Symbol.of("pass"));
  }
  
  
}
