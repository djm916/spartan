package spartan.tests;

import org.junit.Test;
import static org.junit.Assert.*;
import spartan.parsing.Reader;
import spartan.errors.SyntaxError;
import java.io.IOException;
//import java.io.UnsupportedEncodingException;

public class ReaderTest
{   
   @Test
   public void test1()
   {
     try (Reader r = Reader.forString("")) {
       assertNull(r.read());
     }
     catch (IOException | SyntaxError ex) {
       fail(ex.getMessage());
     }
   }

   @Test
   public void test2()
   {
     try {
       Reader r = Reader.forString(null);
       assertNull(r.read());
     }
     catch (IOException | SyntaxError ex) {
       fail(ex.getMessage());
     }
   }
}
