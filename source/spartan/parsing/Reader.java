package spartan.parsing;

import java.io.*;
import spartan.data.*;
import spartan.errors.Error;

class MutablePosition
{
  int line, column;

  MutablePosition(int line, int column)
  {
    this.line = line;
    this.column = column;
  }
}

public class Reader implements AutoCloseable
{
  public static Reader forFile(String fileName) throws FileNotFoundException, UnsupportedEncodingException
  {
    return new Reader(fileName, new FileInputStream(fileName));
  }

  public static Reader forConsole() throws UnsupportedEncodingException
  {
    return new Reader("interactive input", System.in);
  }

  public static Reader forString(String s) throws UnsupportedEncodingException
  {
    return new Reader("unknown source", new ByteArrayInputStream(s.getBytes()));
  }

  public SourceDatum read() throws IOException
  {
    positionMap.clear();
    skipSpace();
    var result = readDatum();
    if (result == null)
      return null;
    return new SourceDatum(result, positionMap);
  }

  public void close() throws IOException
  {
    input.close();
  }

  private Reader(String source, InputStream input) throws UnsupportedEncodingException
  {
    this.source = source;
    this.input = new PushbackReader(new BufferedReader(new InputStreamReader(input, spartan.Config.DEFAULT_ENCODING)));
  }

  private static boolean isDigit(int ch)
  {
    return ch >= '0' && ch <= '9';
  }

  private static boolean isHexDigit(int ch)
  {
    return isDigit(ch) || (ch >= 'A' && ch <= 'F');
  }

  private static boolean isSpace(int ch)
  {
    return ch == ' ' || ch == '\n' || ch == '\r' || ch == '\t';
  }

  private static boolean isLetter(int ch)
  {
    return (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z');
  }

  private static boolean isSpecial(int ch)
  {
    return ch == '~' || ch == '!' || ch == '@' || ch == '#' || ch == '$' || ch == '%'
        || ch == '^' || ch == '&' || ch == '*' || ch == '-' || ch == '_' || ch == '+'
        || ch == '=' || ch == '|' || ch == '\\' || ch == ':' || ch == '<' || ch == '>'
        || ch == '.' || ch == '?' || ch ==  '/';
  }

  private static boolean isSymbol(int ch)
  {
    return isLetter(ch) || isSpecial(ch) || isDigit(ch);
  }

  private static boolean isStringEnd(int ch)
  {
    return ch == -1 || ch == '\"';
  }

  private static boolean isSign(int ch)
  {
    return ch == '+' || ch == '-';
  }

  private static boolean isExponent(int ch)
  {
    return ch == 'e' || ch == 'E';
  }
  
  private static boolean isRadix(int ch)
  {
    return ch == '.';
  }
  
  private void getChar() throws IOException
  {
    lastChar = input.read();

    if (lastChar == -1)
      return;

    if (lastChar == '\n') {
      ++currentPos.line;
      currentPos.column = 0;
    }
    else {
      ++currentPos.column;
    }
  }

  private int peekChar() throws IOException
  {
    int ch = input.read();
    if (ch != -1)
      input.unread(ch);
    return ch;
  }

  private void skipComment() throws IOException
  {
    do {
      getChar();
    } while (lastChar != -1 && lastChar != '\n');
  }

  private void skipSpace() throws IOException
  {
    do {
      getChar();
      if (lastChar == ';') {
        skipComment();
      }
    } while (isSpace(lastChar));
  }

  private void setTokenPosition()
  {
    tokenStart.line = currentPos.line;
    tokenStart.column = currentPos.column;
  }
  
  private Position getTokenPosition()
  {
    return new Position(source, tokenStart.line, tokenStart.column);
  }
  
  private Error syntaxError(String message)
  {
    discardRemainingInput();    
    return new Error(message, getTokenPosition());
  }

  private void discardRemainingInput()
  {
    try {
      while (input.ready())
        getChar();
    }
    catch (IOException ex) {
      // ignore
    }
  }
  
  private void scanDigits(StringBuilder text) throws IOException
  {
    if (!isDigit(lastChar))
      throw syntaxError("malformed numeric literal");

    text.append((char)lastChar);

    while (isDigit(peekChar())) {
      getChar();
      text.append((char)lastChar);
    }
  }
  
  private void scanFractionAndExponent(StringBuilder text) throws IOException
  {
    // add the "." character
    getChar();
    text.append((char)lastChar);    
    
    // add fraction digits
    getChar();
    scanDigits(text);
    
    // scan optional exponent
    if (isExponent(peekChar()))
      scanExponent(text);
  }
  
  private void scanExponent(StringBuilder text) throws IOException
  {
    // add the "e" or "E" character
    getChar();
    text.append((char)lastChar);
    
    // scan the optional sign
    if (isSign(peekChar())) {
      getChar();
      text.append((char)lastChar);
    }
    
    // add digits of the exponent
    getChar();
    scanDigits(text);
  }
    
  private void scanImaginaryPart(StringBuilder text) throws IOException
  {
    // add the sign character
    getChar();
    text.append((char)lastChar);
    
    // add digits before "."
    getChar();
    scanDigits(text);
    
    if (peekChar() != '.')
      throw syntaxError("malformed numeric literal");
    
    scanFractionAndExponent(text);
    
    if (peekChar() != 'i')
      throw syntaxError("malformed numeric literal");
    
    getChar(); // eat "i"
  }
  
  /* Read a number (integer, rational, real, or complex)
     according to the following grammar:
    
    <number> -> <sign>? (<integer> | <rational> | <real> | <complex>)
    <integer> -> <digit>+
    <rational> -> <integer> "/" <integer>
    <real> -> <integer> "." <integer> <exponent>?
    <complex> -> <real> <sign> <real> "i"
    <exponent> -> ("e" | "E") <sign>? <integer>
    <sign> -> "-" | "+"
    <digit> -> "0" | "1" | ... | "9"
    
  */
    
  private Datum readNumber() throws IOException
  {
    var text = new StringBuilder();

    if (isSign(lastChar)) {
      text.append((char)lastChar);
      getChar();
    }

    scanDigits(text);

    if (peekChar() == '/') {
      var numer = text.toString();
      text = new StringBuilder();
      getChar(); // skip /
      getChar();
      scanDigits(text);
      var denom = text.toString();
      return makeRatio(numer, denom);
    }
    
    if (peekChar() == '.') {      
      scanFractionAndExponent(text);
      
      if (isSign(peekChar())) {
        var real = text.toString();
        text = new StringBuilder();
        scanImaginaryPart(text);
        var imag = text.toString();
        return makeComplex(real, imag);
      }
      
      return makeReal(text.toString());
    }
        
    return makeInt(text.toString());
  }

  private Numeric makeInt(String text)
  {
    try {
      return new Int(text);
    }
    catch (NumberFormatException ex) {
      return makeBigInt(text);
    }
  }
  
  private BigInt makeBigInt(String text)
  {    
    try {
      return new BigInt(text);
    }
    catch (NumberFormatException ex) {
      throw syntaxError("malformed numeric literal");
    }
  }
  
  private Ratio makeRatio(String numer, String denom)
  {
    try {
      return new Ratio(numer, denom);
    }
    catch (NumberFormatException ex) {
      throw syntaxError("malformed numeric literal");
    }
  }
  
  private Real makeReal(String text)
  {
    try {
      return new Real(text);
    }
    catch (NumberFormatException ex) {
      throw syntaxError("malformed numeric literal");
    }
  }
  
  private Complex makeComplex(String real, String imag)
  {
    try {
      return new Complex(real, imag);
    }
    catch (NumberFormatException ex) {
      throw syntaxError("malformed numeric literal");
    }
  }
  
  private Datum readSymbol() throws IOException
  {
    var text = new StringBuilder();
    var position = getTokenPosition();
    
    text.append((char)lastChar);

    while (isSymbol(peekChar())) {
      getChar();
      text.append((char)lastChar);
    }
    
    var result = new Symbol(text.toString());
    positionMap.put(result, position);
    return result;
  }
  
  private Datum readText() throws IOException
  {
    var text = new StringBuilder();

    while (!isStringEnd(peekChar())) {
      getChar();
      if (lastChar == '\\') {
        getChar();
        text.append(interpolateEscapeChar((char)lastChar));
      }
      else {
        text.append((char) lastChar);
      }
    }

    getChar();

    if (lastChar != '\"')
      throw syntaxError("undelimited string literal");

    return new Text(text.toString());
  }

  private char interpolateEscapeChar(char ch)
  {
    return switch (lastChar) {      
      case 'n' -> '\n';
      case 'r' -> '\r';
      case 't' -> '\t';
      case '"' -> '"';
      case '\\' -> '\\';
      default -> ch;
    };
  }
  
  private Datum readList() throws IOException
  {
    var builder = new List.Builder();
    var position = getTokenPosition();

    skipSpace();

    while (lastChar != -1 && lastChar != ')') {
      builder.add(readDatum());
      skipSpace();
    }

    var result = builder.build();
    positionMap.put(result, position);
    return result;
  }

  private Datum readVector() throws IOException
  {
    var result = new Vector();
    var position = getTokenPosition();

    skipSpace();

    while (lastChar != -1 && lastChar != ']') {
      result.append(readDatum());
      skipSpace();
    }

    return result;
  }
  
  private Datum readQuote() throws IOException
  {
    skipSpace();
    return List.of(Symbol.Quote, readDatum());
  }
  
  private Datum readUnquote() throws IOException
  {
    skipSpace();
    return List.of(Symbol.Unquote, readDatum());
  }

  private Datum readUnquoteSplicing() throws IOException
  {
    getChar();
    skipSpace();
    return List.of(Symbol.UnquoteSplicing, readDatum());
  }

  private Datum readQuasiQuote() throws IOException
  {
    skipSpace();
    return List.of(Symbol.Quasiquote, readDatum());
  }
  
  private Datum readDatum() throws IOException
  {
    setTokenPosition();

    if (lastChar == -1)
      return null;
    if (lastChar == '(')
      return readList();
    if (lastChar == '[')
      return readVector();
    if (isSign(lastChar) && isDigit(peekChar()))
      return readNumber();
    if (isDigit(lastChar))
      return readNumber();
    if (isSymbol(lastChar))
      return readSymbol();
    if (lastChar == '\"')
      return readText();
    if (lastChar == '\'')
      return readQuote();
    if (lastChar == ',' && peekChar() == '@')
      return readUnquoteSplicing();
    if (lastChar == ',')
      return readUnquote();
    if (lastChar == '`')
      return readQuasiQuote();

    throw syntaxError("unrecognized character " + (char)lastChar);
  }
    
  private final PushbackReader input;
  private final String source;
  private int lastChar = -1;
  private final MutablePosition tokenStart = new MutablePosition(0, 0);
  private final MutablePosition currentPos = new MutablePosition(1, 0);
  private PositionMap positionMap = new PositionMap();
}
