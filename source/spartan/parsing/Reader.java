package spartan.parsing;

import java.io.*;
import spartan.data.*;
import spartan.errors.SyntaxError;

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

  public SourceDatum read() throws SyntaxError, IOException
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
    this.input = new PushbackReader(new BufferedReader(new InputStreamReader(input, DefaultEncoding)));
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
    return ch == 'E' || ch == 'e';
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

  private void markTokenStart()
  {
    tokenStart.line = currentPos.line;
    tokenStart.column = currentPos.column;
  }

  private SyntaxError error(String message)
  {
    flush();
    return new SyntaxError(message, new Position(source, tokenStart.line, tokenStart.column));
  }

  private void flush()
  {
    try {
      while (input.ready())
        getChar();
    }
    catch (IOException ex) {
      // ignore
    }
  }

  private void readDigits(StringBuilder text) throws IOException, SyntaxError
  {
    if (!isDigit(lastChar))
      throw error("malformed numeric literal");

    text.append((char)lastChar);

    while (isDigit(peekChar())) {
      getChar();
      text.append((char)lastChar);
    }
  }

  private Datum readNumber() throws IOException, SyntaxError
  {
    var text = new StringBuilder();

    if (isSign(lastChar)) {
      text.append((char)lastChar);
      getChar();
    }

    readDigits(text);

    if (peekChar() == '.') {
      getChar();
      text.append((char)lastChar);
      getChar();
      readDigits(text);
      if (isExponent(peekChar())) {
        getChar();
        text.append((char)lastChar);
        if (isSign(peekChar())) {
          getChar();
          text.append((char)lastChar);
        }
        getChar();
        readDigits(text);
      }
      if (isSign(peekChar())) {
        var real = Double.parseDouble(text.toString());
        text = new StringBuilder();
        getChar();
        text.append((char)lastChar);
        getChar();
        readDigits(text);
        if (peekChar() != '.')
          throw error("malformed numeric literal");
        getChar();
        text.append((char)lastChar);
        getChar();
        readDigits(text);
        if (isExponent(peekChar())) {
          getChar();
          text.append((char)lastChar);
          if (isSign(peekChar())) {
            getChar();
            text.append((char)lastChar);
          }
          getChar();
          readDigits(text);
        }
        var imag = Double.parseDouble(text.toString());
        if (peekChar() != 'i')
          throw error("malformed numeric literal");
        getChar();
        return new Complex(real, imag);
      }
      return new Real(Double.parseDouble(text.toString()));
    }
    return new Int(Integer.parseInt(text.toString()));
  }

  private Datum readSymbol() throws IOException
  {
    var text = new StringBuilder();
    var position = new Position(source, tokenStart.line, tokenStart.column);

    text.append((char)lastChar);

    while (isSymbol(peekChar())) {
      getChar();
      text.append((char)lastChar);
    }

    var symbol = Symbol.get(text.toString());
    positionMap.put(symbol, position);
    return symbol;
  }

  private Datum readText() throws SyntaxError, IOException
  {
    var text = new StringBuilder();

    while (!isStringEnd(peekChar())) {
      getChar();
      text.append((char)lastChar);
    }

    getChar();

    if (lastChar != '\"')
      throw error("undelimited string literal");

    return new Text(text.toString());
  }

  private Datum readList() throws SyntaxError, IOException
  {
    var builder = new List.Builder();
    var position = new Position(source, tokenStart.line, tokenStart.column);

    skipSpace();

    while (lastChar != -1 && lastChar != ')') {
      builder.add(readDatum());
      skipSpace();
    }

    var result = builder.build();
    positionMap.put(result, position);
    return result;
  }

  private Datum readVector() throws SyntaxError, IOException
  {
    var builder = new List.Builder();
    var position = new Position(source, tokenStart.line, tokenStart.column);

    skipSpace();

    while (lastChar != -1 && lastChar != ']') {
      builder.add(readDatum());
      skipSpace();
    }

    var result = List.cons(Symbol.get("vector"), builder.build());
    positionMap.put(result, position);
    return result;
  }

  private Datum readRecord() throws SyntaxError, IOException
  {
    var builder = new List.Builder();
    var position = new Position(source, tokenStart.line, tokenStart.column);

    skipSpace();

    while (lastChar != -1 && lastChar != '}') {
      builder.add(readDatum());
      skipSpace();
    }

    var result = List.cons(Symbol.get("record"), builder.build());
    positionMap.put(result, position);
    return result;
  }

  private Datum readQuote() throws SyntaxError, IOException
  {
    skipSpace();
    return List.of(Symbol.get("quote"), readDatum());
  }
  
  private Datum readUnquote() throws SyntaxError, IOException
  {
    skipSpace();
    return List.of(Symbol.get("unquote"), readDatum());
  }

  private Datum readUnquoteSplicing() throws SyntaxError, IOException
  {
    getChar();
    skipSpace();
    return List.of(Symbol.get("unquote-splicing"), readDatum());
  }

  private Datum readQuasiQuote() throws SyntaxError, IOException
  {
    skipSpace();
    return List.of(Symbol.get("quasiquote"), readDatum());
  }
  
  private Datum readDatum() throws SyntaxError, IOException
  {
    markTokenStart();

    if (lastChar == -1)
      return null;
    if (lastChar == '(')
      return readList();
    if (lastChar == '[')
      return readVector();
    if (lastChar == '{')
      return readRecord();
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

    throw error("unrecognized character " + (char)lastChar);
  }

  private static final String DefaultEncoding = "UTF-8";
  private final PushbackReader input;
  private final String source;
  private int lastChar = -1;
  private final MutablePosition tokenStart = new MutablePosition(0, 0);
  private final MutablePosition currentPos = new MutablePosition(1, 0);
  private PositionMap positionMap = new PositionMap();
}
