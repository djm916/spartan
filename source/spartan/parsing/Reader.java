package spartan.parsing;

import java.io.*;
import java.nio.file.Path;
import spartan.data.*;
import spartan.errors.SyntaxError;
import spartan.errors.SourceInfo;
import spartan.Config;

class MutablePosition
{
  int line, column;

  MutablePosition(int line, int column)
  {
    this.line = line;
    this.column = column;
  }
}

/**
 * Provides a stream-like interface for parsing Scheme source code.
 *
 * The input source can be a file, the terminal, or a string.
 */
public class Reader implements AutoCloseable
{
  /**
   * Creates a {@code Reader} whose input source is the file located at the given path.
   *
   * @param path the full file path
   * @return a {@code Reader} for the given file
   * @throws FileNotFoundException if the file doesn't exist
   */
  public static Reader forFile(Path path) throws FileNotFoundException
  {
    var file = path.toString();
    return new Reader(file, new FileInputStream(file));
  }

  /**
   * Creates a {@code Reader} whose input source is the standard input (e.g., {@link System#in}).
   *
   * @return a reader for the standard input
   */
  public static Reader forConsole()
  {
    return new Reader("interactive input", System.in);
  }

  /**
   * Creates a reader whose input source is the given string.
   *
   * @return a {@code Reader} for the given {@code String}
   */
  public static Reader forString(String str)
  {
    return new Reader("unknown source", new ByteArrayInputStream(str.getBytes(Config.DEFAULT_ENCODING)));
  }
    
  /**
   * Reads the next {@code SourceDatum} from the input stream.
   *
   * Parses and consumes a complete Scheme expression from the input source.
   * 
   * @return the next {@code SourceDatum}
   * @throws EOFException if input stream is exhausted (end of file)
   * @throws SyntaxError if the input text is syntactially invalid
   */
  public SourceDatum read() throws EOFException
  {
    positionMap.clear();
    skipSpace();
    if (lastChar == -1)
      throw new EOFException();
    return new SourceDatum(readDatum(), positionMap);
  }
  
  /**
   * Closes this reader and its underlying input source.
   */
  public void close()
  {
    try {
      input.close();
    }
    catch (IOException ex) {
      throw new UncheckedIOException(ex);
    }
  }

  private Reader(String source, InputStream input)
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
    return ch == '~' || ch == '!' || ch == '@' || ch == '$' || ch == '%'
        || ch == '^' || ch == '&' || ch == '*' || ch == '-' || ch == '_' || ch == '+'
        || ch == '=' || ch == '|' || ch == '\\' || ch == '<' || ch == '>' || ch == '?'
        || ch == '.' || ch ==  '/';
  }
  
  private static boolean isSymbolStart(int ch)
  {
    return isLetter(ch) || isSpecial(ch);
  }
  
  private static boolean isSymbolFollow(int ch)
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
  
  // Wrapper over input.read() to convert IOException to UncheckedIOException
  private int _read()
  {
    try {
      return input.read();
    }
    catch (IOException ex) {
      throw new UncheckedIOException(ex);
    }
  }
  
  // Wrapper over input.unread() to convert IOException to UncheckedIOException
  private void _unread(int ch)
  {
    try {
      input.unread(ch);
    }
    catch (IOException ex) {
      throw new UncheckedIOException(ex);
    }
  }
  
  private void getChar()
  {
    lastChar = _read();

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

  private int peekChar()
  {
    int ch = _read();
    if (ch != -1)
      _unread(ch);
    return ch;
  }

  private void skipComment()
  {
    do {
      getChar();
    } while (lastChar != -1 && lastChar != '\n');
  }

  private void skipSpace()
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
    
  private SyntaxError syntaxError(String message)
  {
    discardRemainingInput();
    var position = getTokenPosition();
    return new SyntaxError(message, new SourceInfo(null, position));
  }
  
  private <E extends Datum> E withPosition(E exp, Position pos)
  {
    positionMap.put(exp, pos);
    return exp;
  }
  
  private SyntaxError malformedNumeric()
  {
    return syntaxError("malformed numeric literal");
  }
  
  private SyntaxError unexpectedEOF()
  {
    return syntaxError("unexpected end of input");
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
  
  /* Read a number literal (integer, rational, real, or complex)
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
    
  private INum readNumber()
  {
    var text = new StringBuilder();
    var position = getTokenPosition();
    
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

  private void scanDigits(StringBuilder text)
  {
    if (!isDigit(lastChar))
      throw syntaxError("malformed numeric literal");

    text.append((char)lastChar);

    while (isDigit(peekChar())) {
      getChar();
      text.append((char)lastChar);
    }
  }
  
  private void scanFractionAndExponent(StringBuilder text)
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
  
  private void scanExponent(StringBuilder text)
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
    
  private void scanImaginaryPart(StringBuilder text)
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
  
  private IInt makeInt(String text)
  {
    try {
      return Int.valueOf(text);
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
      throw malformedNumeric();
    }
  }
  
  private Ratio makeRatio(String numer, String denom)
  {
    try {
      return new Ratio(numer, denom);
    }
    catch (NumberFormatException ex) {
      throw malformedNumeric();
    }
  }
  
  private Real makeReal(String text)
  {
    try {
      return new Real(text);
    }
    catch (NumberFormatException ex) {
      throw malformedNumeric();
    }
  }
  
  private Complex makeComplex(String real, String imag)
  {
    try {
      return new Complex(real, imag);
    }
    catch (NumberFormatException ex) {
      throw malformedNumeric();
    }
  }
  
  /** Read a symbol with the following grammar:
   *
   * <symbol> -> <qualified-symbol> | <bare-symbol>
   * <qualified-symbol> => <namespace>:<base-name>
   * <namespace> => <bare-symbol>
   * <base-name> => <bare-symbol>
   * <bare-symbol> -> <symbol-start-char> <symbol-follow-char>*
   *
   */
  private Symbol readSymbol()
  {
    var position = getTokenPosition();
    var text = new StringBuilder();
        
    text.append((char)lastChar);

    while (isSymbolFollow(peekChar())) {
      getChar();
      text.append((char)lastChar);
    }
    
    if (peekChar() == ':') 
      return readQualifiedSymbol(text.toString(), text, position);
    else
      return withPosition(new Symbol(text.toString()), position);
  }
  
  /** Read the base-name part of a qualified symbol
   *
   */
  private QualifiedSymbol readQualifiedSymbol(String nsName, StringBuilder text, Position position)
  {
    getChar();
    text.append((char)lastChar); // the separator ":" is part of the symbol's full name
    // save index where base name starts
    var splitIndex = text.length();
    // ensure the base name is not empty and begins a valid symbol
    if (!isSymbolStart(peekChar()))
      throw syntaxError("malformed symbol");
    getChar();
    text.append((char)lastChar);
    // read the rest of the base name
    while (isSymbolFollow(peekChar())) {
      getChar();
      text.append((char)lastChar);
    }
    var fullName = text.toString();
    var baseName = fullName.substring(splitIndex);
    return withPosition(new QualifiedSymbol(fullName, nsName, baseName), position);
  }
  
  private Symbol readKeyword()
  {
    var position = getTokenPosition();
    var text = new StringBuilder();
    
    text.append((char)lastChar);

    while (isSymbolFollow(peekChar())) {
      getChar();
      text.append((char)lastChar);
    }
    
    return Symbol.of(text.toString());
  }
  
  private Datum readLiteral()
  {
    var position = getTokenPosition();
    var buffer = new StringBuilder();
    
    buffer.append((char)lastChar);

    while (isSymbolFollow(peekChar())) {
      getChar();
      buffer.append((char)lastChar);
    }
    
    var str = buffer.toString();
    
    if (str.equals("#true"))
      return Bool.TRUE;
    else if (str.equals("#false"))
      return Bool.FALSE;
    else if (str.equals("#nil"))
      return Nil.VALUE;
    else
      throw syntaxError("invalid literal " + str);
  }
  
  private Text readText()
  {
    var text = new StringBuilder();

    while (!isStringEnd(peekChar())) {
      getChar();
      if (lastChar == '\\') {
        //getChar();
        readEscapeSequence(text);
        //text.append(interpolateEscapeChar((char)lastChar));
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
  
  private void readEscapeSequence(StringBuilder text)
  {
    getChar();
    if (lastChar == 'n')
      text.append('\n');
    else if (lastChar == '"')
      text.append('"');
    else if (lastChar == '\\')
      text.append('\\');
    else if (lastChar == 'u')
      readUnicodeScalar(text);
    else
      text.append((char)lastChar);
  }
  
  private void readUnicodeScalar(StringBuilder text)
  {
    if (peekChar() != '{')
      throw syntaxError("invalid Unicode escape sequence");
    getChar();
    
    var digits = new StringBuilder(8);
    while (isHexDigit(peekChar()) && digits.length() <= 8) {
      getChar();
      digits.append((char)lastChar);
    }
    
    if (peekChar() != '}')
      throw syntaxError("invalid Unicode escape sequence");
    getChar();
    
    int codePoint = 0;
    try {
      codePoint = Integer.parseUnsignedInt(digits, 0, digits.length(), 16);
      //System.out.println("read codepoint = " + Integer.toHexString(codePoint));
    }
    catch (NumberFormatException ex) {
      throw syntaxError("Unicode scalar value out of range");
    }
    
    try {
      text.appendCodePoint(codePoint);
    }
    catch (IllegalArgumentException ex) {
      throw syntaxError("Unicode scalar value out of range");
    }
  }
  
  private List readList(char delim)
  {
    var position = getTokenPosition();
    var builder = new List.Builder();

    skipSpace();

    while (lastChar != -1 && lastChar != delim) {
      builder.add(readDatum());
      skipSpace();
    }
    
    if (lastChar != delim)
      throw unexpectedEOF();
    
    return withPosition(builder.build(), position);
  }
    
  private List readQuote()
  {
    var pos = getTokenPosition();
    skipSpace();
    return withPosition(List.of(Symbol.QUOTE, readDatum()), pos);
  }
  
  private List readUnquote()
  {
    var pos = getTokenPosition();
    skipSpace();
    return withPosition(List.of(Symbol.UNQUOTE, readDatum()), pos);
  }

  private List readUnquoteSplicing()
  {
    var pos = getTokenPosition();
    getChar();
    skipSpace();
    return withPosition(List.of(Symbol.UNQUOTE_SPLICING, readDatum()), pos);
  }

  private List readQuasiQuote()
  {
    var pos = getTokenPosition();
    skipSpace();
    return withPosition(List.of(Symbol.QUASIQUOTE, readDatum()), pos);
  }
  
  /**
   * Reads a single {@code Datum} from the input stream.
   *
   * @return The next {@code Datum}
   * @throws SyntaxError if the input is syntactially invalid or the input stream is exhausted
   */
  private Datum readDatum()
  {
    setTokenPosition();

    if (lastChar == -1)
      throw unexpectedEOF();
    if (lastChar == '(')
      return readList(')');
    if (lastChar == '[')
      return readList(']');
    if (isSign(lastChar) && isDigit(peekChar()))
      return readNumber();
    if (isDigit(lastChar))
      return readNumber();
    if (lastChar == ':')
      return readKeyword();
    if (isSymbolStart(lastChar))
      return readSymbol();
    if (lastChar == '#')
      return readLiteral();
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
