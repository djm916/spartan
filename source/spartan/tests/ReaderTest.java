package spartan.tests;

import org.junit.Test;
import static org.junit.Assert.*;
import spartan.parsing.Reader;
import spartan.errors.Error;
import spartan.data.Datum;
import java.io.IOException;

public class ReaderTest
{   
  @Test
  // Reader returns null on end of input
  public void test1()
  {
    try (Reader r = Reader.forString("")) {
      assertNull(r.read());
    }
    catch (IOException | Error ex) {
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
    catch (IOException | Error ex) {
      fail(ex.getMessage());
    }
  }
  
  @Test
  // Reader parses integers
  public void test3()
  {
    try (Reader r = Reader.forString("123")) {
      Datum output = r.read().datum();
      assertTrue(output.type().isInt());
      assertEquals(output.repr(), "123");
    }
    catch (IOException | Error ex) {
      fail(ex.getMessage());
    }
  }
}
