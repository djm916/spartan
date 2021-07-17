package spartan.parsing;

//import java.io.Reader;
import java.io.PushbackReader;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.FileNotFoundException;
import java.util.Deque;
import java.util.ArrayDeque;
import spartan.data.*;
import spartan.Position;
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

public class Reader
{
  private static final String DefaultEncoding = "UTF-8";
  private final PushbackReader input;
  private int lastChar = -1;
  private final MutablePosition tokenStart = new MutablePosition(0, 0);
  private final MutablePosition currentPos = new MutablePosition(1, 0);
  private final String source;
  private PositionMap positionMap;

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
  
  private void readDigits(StringBuilder text) throws IOException, SyntaxError
  {
    if (!isDigit(lastChar)) {
      throw new SyntaxError("malformed numeric literal",
                            new Position(source, tokenStart.line, tokenStart.column));
    }
    
    text.append((char)lastChar);
    
    while (isDigit(peekChar())) {
      getChar();
      text.append((char)lastChar);
    }
  }

  private Value readNumber() throws IOException, SyntaxError
  {
    StringBuilder text = new StringBuilder();
    
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
      return new Real(Double.parseDouble(text.toString()));
    }

    return new Int(Integer.parseInt(text.toString()));
  }

  private Value readSymbol() throws IOException
  {
    Position savePosition = new Position(source, tokenStart.line, tokenStart.column);
    
    StringBuilder text = new StringBuilder();
    
    text.append((char)lastChar);
    
    while (isSymbol(peekChar())) {
      getChar();
      text.append((char)lastChar);
    }
    
    Symbol symbol = Symbol.get(text.toString());
    
    if (Symbol.get("true") == symbol)
      return Bool.True;
    if (Symbol.get("false") == symbol)
      return Bool.False;
    
    positionMap.put(symbol, savePosition);
    return symbol;
  }

  private Value readText() throws SyntaxError, IOException
  {
    StringBuilder text = new StringBuilder();
        
    while (!isStringEnd(peekChar())) {
      getChar();
      text.append((char)lastChar);
    }
    
    getChar();
    
    if (lastChar != '\"') {
      throw new SyntaxError("undelimited string literal",
                            new Position(source, tokenStart.line, tokenStart.column));
    }
    
    return new Text(text.toString());
  }
  
  private Value readList() throws SyntaxError, IOException
  {
    Position savePosition = new Position(source, tokenStart.line, tokenStart.column);    
    Deque<Value> stack = new ArrayDeque<>();
    
    skipSpace();
    
    while (lastChar != -1 && lastChar != ')') {
      stack.push(readValue());      
      skipSpace();
    }
    
    List result = List.Empty;
    while (!stack.isEmpty())
      result = new List(stack.pop(), result);
    
    positionMap.put(result, savePosition);
    return result;
  }
    
  private Value readQuote() throws SyntaxError, IOException
  {
    skipSpace();
    return new List(Symbol.get("quote"), new List(readValue(), List.Empty));
  }
    
  private Value readValue() throws SyntaxError, IOException
  {
    markTokenStart();
    
    if (lastChar == -1)
      return null;
    if (lastChar == '(')
      return readList();
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

    throw new SyntaxError("unrecognized character",
                          new Position(source, currentPos.line, currentPos.column));
  }
  
  public static Reader readFile(String fileName) throws FileNotFoundException, IOException
  {
    java.io.Reader r = new BufferedReader(new InputStreamReader(new FileInputStream(fileName), DefaultEncoding));
    return new Reader(r, fileName);
  }
  
  public static Reader readStdin() throws IOException
  {
    java.io.Reader r = new BufferedReader(new InputStreamReader(System.in, DefaultEncoding));
    return new Reader(r, "stdin");
  }
  
  public Reader(java.io.Reader r, String source)
  {
    this.input = new PushbackReader(r);
    this.source = source;
  }
  
  public SourceValue read() throws SyntaxError, IOException
  {
    positionMap = new PositionMap();
    skipSpace();
    Value result = readValue();
    if (result == null)
      return null;
    return new SourceValue(result, positionMap);
  }
}
