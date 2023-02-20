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
  public void endOfInputThrowsEOF() throws EOFException
  {
    try (Reader r = Reader.forString("")) {
      r.read();
    }
  }

  @Test(expected = NullPointerException.class)
  public void nullInputThrowsNPE() throws EOFException
  {
    try (Reader r = Reader.forString(null)) {
      r.read();
    }
  }
  
  @Test
  public void parsesIntegers() throws EOFException
  {
    try (Reader r = Reader.forString("1 -1 +1")) {
      var output = r.read().datum();
      assertTrue(output instanceof Int);
      assertEquals(output.repr(), "1");
      
      output = r.read().datum();
      assertTrue(output instanceof Int);
      assertEquals(output.repr(), "-1");
      
      output = r.read().datum();
      assertTrue(output instanceof Int);
      assertEquals(output.repr(), "1");
    }
  }
}
