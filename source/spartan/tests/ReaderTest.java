package spartan.tests;

import org.junit.Test;
import static org.junit.Assert.*;
import spartan.parsing.Reader;
import spartan.errors.Error;
import spartan.data.Datum;
import java.io.IOException;
import java.io.EOFException;
import spartan.data.Int;

public class ReaderTest
{   
  @Test(expected = EOFException.class)
  // Reader throws EOFException on end of input
  public void test1() throws EOFException
  {
    try (Reader r = Reader.forString("")) {
      r.read();
    }
    catch (Error ex) {
      fail(ex.getMessage());
    }
  }

  @Test(expected = NullPointerException.class)
  // Reader throws NullPointerException on null input
  public void test2() throws EOFException
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
    catch (EOFException ex) {
      fail(ex.getMessage());
    }
    catch (Error ex) {
      fail(ex.getMessage());
    }
  }
}
