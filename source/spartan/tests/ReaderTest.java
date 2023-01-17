package spartan.tests;

import org.junit.Test;
import static org.junit.Assert.*;
import spartan.parsing.Reader;
import spartan.errors.Error;
import spartan.data.Datum;
import java.io.IOException;
import spartan.data.Int;

public class ReaderTest
{   
  @Test
  // Reader returns null on end of input
  public void test1()
  {
    try (Reader r = Reader.forString("")) {
      assertNull(r.read());
    }
    catch (Error ex) {
      fail(ex.getMessage());
    }
  }

  @Test(expected = NullPointerException.class)
  // Reader throws NullPointerException on null input
  public void test2()
  {
    try (Reader r = Reader.forString(null)) {
      r.read();
    }
    catch (Error ex) {
      fail(ex.getMessage());
    }
  }
  
  @Test
  // Reader parses integers
  public void test3()
  {
    try (Reader r = Reader.forString("123")) {
      var output = r.read().datum();
      assertTrue(output instanceof Int);
      assertEquals(output.repr(), "123");
    }
    catch (Error ex) {
      fail(ex.getMessage());
    }
  }
}
