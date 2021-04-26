package spartan.parsing;

import java.io.Reader;
import java.io.PushbackReader;
import java.io.IOException;
import java.util.Map;
import java.util.HashMap;
import spartan.Position;
import spartan.errors.SyntaxError;

enum TokenType
{
  LftParen, RgtParen,
  LftBrack, RgtBrack,
  LftBrace, RgtBrace,
  Colon, SemiColon, Comma, Period, Exclam, At, Pipe,
  If, Then, Else, Elif, 
  Let, Rec, In,
  Fun, RgtArrow,
  Def, End,
  Ident, True, False, Int, Real, String,
  Neg, Add, Sub, Mul, Div, Mod,
  Eq, Ne, Lt, Gt, Le, Ge,
  And, Or, Not,
  Eof
}

class Token
{
  final TokenType type;
  final String text;
  final Position position;
  
  public Token(TokenType type, String text, Position position)
  {
    this.type = type;
    this.text = text;
    this.position = position;
  }
  
  public String toString()
  {
    return "(" + type + " " + text + " " + position + ")";
  }
}

class MutablePosition
{
  int line, column;
  
  MutablePosition(int line, int column)
  {
    this.line = line;
    this.column = column;
  }
}

class TokenStream
{
  private static final Map<String, TokenType> keywords = new HashMap<>();
  
  static
  {
    keywords.put("def", TokenType.Def);
    keywords.put("if", TokenType.If);
    keywords.put("then", TokenType.Then);
    keywords.put("else", TokenType.Else);
    keywords.put("elif", TokenType.Elif);
    keywords.put("let", TokenType.Let);
    keywords.put("rec", TokenType.Rec);
    keywords.put("in", TokenType.In);
    keywords.put("fun", TokenType.Fun);
    keywords.put("true", TokenType.True);
    keywords.put("false", TokenType.False);
    keywords.put("end", TokenType.End);
  }
  
  private final PushbackReader input;
  private int lastChar = -1;
  private final MutablePosition tokenStart = new MutablePosition(0, 0);
  private final MutablePosition currentPos = new MutablePosition(1, 0);
  private final String source;
  
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
  
  private static boolean isIdentStart(int ch)
  {
    return ch == '_' || isLetter(ch);
  }
  
  private static boolean isIdentFollow(int ch)
  {
    return isIdentStart(ch) || isDigit(ch);
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
      if (lastChar == '#') {
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

  private Token readNumber() throws IOException, SyntaxError
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
      return makeToken(TokenType.Real, text.toString());
    }

    return makeToken(TokenType.Int, text.toString());
  }

  private Token readIdent() throws IOException
  {
    StringBuilder text = new StringBuilder();
    
    text.append((char)lastChar);
    
    while (isIdentFollow(peekChar())) {
      getChar();
      text.append((char)lastChar);
    }
    
    String id = text.toString();
    
    if (keywords.containsKey(id)) {
      return makeToken(keywords.get(id), id);
    }

    return makeToken(TokenType.Ident, id);
  }

  private Token readString() throws SyntaxError, IOException
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
    
    return makeToken(TokenType.String, text.toString());
  }
  
  private Token makeToken(TokenType type, String text)
  {
    return new Token(type, text, new Position(source, tokenStart.line, tokenStart.column));
  }
  
  TokenStream(Reader r, String source)
  {
    this.input = new PushbackReader(r);
    this.source = source;
  }
  
  public Position position()
  {
    return new Position(source, currentPos.line, currentPos.column);
  }
  
  Token next() throws SyntaxError, IOException
  {
    skipSpace();
    
    if (lastChar == -1)
      return makeToken(TokenType.Eof, "");
    
    markTokenStart();
    
    if (lastChar == '(')
      return makeToken(TokenType.LftParen, "(");
    
    if (lastChar == ')')
      return makeToken(TokenType.RgtParen, ")");
    
    if (lastChar == '[')
      return makeToken(TokenType.LftBrack, "[");
    
    if (lastChar == ']')
      return makeToken(TokenType.RgtBrack, "]");
    
    if (lastChar == '{')
      return makeToken(TokenType.LftBrace, "{");
    
    if (lastChar == '}')
      return makeToken(TokenType.RgtBrace, "}");
    
    if (lastChar == ',')
      return makeToken(TokenType.Comma, ",");
    
    if (lastChar == ';')
      return makeToken(TokenType.SemiColon, ";");
    
    if (lastChar == ':')
      return makeToken(TokenType.Colon, ":");
    
    if (lastChar == '.')
      return makeToken(TokenType.Period, ".");
    
    if (lastChar == '@')
      return makeToken(TokenType.At, "@");
        
    if (lastChar == '~')
      return makeToken(TokenType.Neg, "~");
    
    if (lastChar == '!')
      return makeToken(TokenType.Exclam, "!");
    
    if (lastChar == '|')
      return makeToken(TokenType.Pipe, "|");
    
    if (lastChar == '+') {
      if (isDigit(peekChar()))
        return readNumber();
      return makeToken(TokenType.Add, "+");
    }
    
    if (lastChar == '-') {
      if (isDigit(peekChar()))
        return readNumber();
      return makeToken(TokenType.Sub, "-");
    }
    
    if (lastChar == '*') {
      return makeToken(TokenType.Mul, "*");
    }
    
    if (lastChar == '/') {
      if (peekChar() == '=') {
        getChar();
        return makeToken(TokenType.Ne, "/=");
      }
      return makeToken(TokenType.Div, "/");
    }
    
    if (lastChar == '%')
      return makeToken(TokenType.Mod, "%");
    
    if (lastChar == '=') {
      if (peekChar() == '>') {
        getChar();
        return makeToken(TokenType.RgtArrow, "=>");
      }
      return makeToken(TokenType.Eq, "=");
    }
    
    if (lastChar == '<') {
      if (peekChar() == '=') {
        getChar();
        return makeToken(TokenType.Le, "<=");
      }
      return makeToken(TokenType.Lt, "<");
    }
    
    if (lastChar == '>') {
      if (peekChar() == '=') {
        getChar();
        return makeToken(TokenType.Ge, ">=");
      }
      return makeToken(TokenType.Gt, ">");
    }
    
    if (isDigit(lastChar))
      return readNumber();

    if (isIdentStart(lastChar))
      return readIdent();

    if (lastChar == '\"')
      return readString();

    throw new SyntaxError("unrecognized character \"" + lastChar + "\"",
                          new Position(source, currentPos.line, currentPos.column));
  }
}
