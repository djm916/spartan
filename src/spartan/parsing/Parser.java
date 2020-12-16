package spartan.parsing;

import spartan.ast.*;
import spartan.Position;
import spartan.data.*;
import spartan.builtins.Builtins;
import spartan.errors.SyntaxError;
import java.io.Reader;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Optional;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.HashSet;
import java.util.Iterator;

public class Parser
{
  
  public static Program parseFile(String fileName)
  throws FileNotFoundException, IOException, SyntaxError
  {
    Reader r = new BufferedReader(new InputStreamReader(new FileInputStream(fileName), DefaultEncoding));
    Parser p = new Parser(r, fileName);
    return p.parseProgram();
  }
  
  private static final String DefaultEncoding = "UTF-8";
  private static final Map<TokenType, PrimFun> ops = new HashMap<>();
  private static final Set<TokenType> atoms = new HashSet<>();
  private TokenStream tokens;
  private Token lastToken;
  
  static
  {
    atoms.add(TokenType.LftParen);
    atoms.add(TokenType.LftBrack);
    atoms.add(TokenType.LftBrace);
    atoms.add(TokenType.Add);
    atoms.add(TokenType.Sub);
    atoms.add(TokenType.Mul);
    atoms.add(TokenType.Div);
    atoms.add(TokenType.Mod);
    atoms.add(TokenType.Neg);
    atoms.add(TokenType.Not);
    atoms.add(TokenType.And);
    atoms.add(TokenType.Or);
    atoms.add(TokenType.Eq);
    atoms.add(TokenType.Ne);
    atoms.add(TokenType.Lt);
    atoms.add(TokenType.Gt);
    atoms.add(TokenType.Le);
    atoms.add(TokenType.Ge);
    atoms.add(TokenType.If);
    atoms.add(TokenType.Let);
    atoms.add(TokenType.Fun);
    atoms.add(TokenType.Ident);
    atoms.add(TokenType.True);
    atoms.add(TokenType.False);
    atoms.add(TokenType.Int);
    atoms.add(TokenType.Real);
    atoms.add(TokenType.String);
    
    ops.put(TokenType.Add, Builtins.Add);
    ops.put(TokenType.Sub, Builtins.Sub);
    ops.put(TokenType.Mul, Builtins.Mul);
    ops.put(TokenType.Div, Builtins.Div);
    ops.put(TokenType.Mod, Builtins.Mod);
    ops.put(TokenType.Neg, Builtins.Neg);
    ops.put(TokenType.Not, Builtins.Not);
    ops.put(TokenType.And, Builtins.And);
    ops.put(TokenType.Or, Builtins.Or);
    ops.put(TokenType.Eq, Builtins.Eq);
    ops.put(TokenType.Ne, Builtins.Ne);
    ops.put(TokenType.Lt, Builtins.Lt);
    ops.put(TokenType.Gt, Builtins.Gt);
    ops.put(TokenType.Le, Builtins.Le);
    ops.put(TokenType.Ge, Builtins.Ge);
  }
  
  private Parser(Reader inputStream, String fileName)
  {
    this.tokens = new TokenStream(inputStream, fileName);
  }
  
  private Program parseProgram() throws IOException, SyntaxError
  {
    List<Binding> defs = new ArrayList<>();
    lastToken = tokens.next();
    while (lastToken != null) {
      require(TokenType.Def, "\"def\" expected");
      lastToken = tokens.next();
      defs.add(parseBinding());
    }
    return new Program(defs);
  }
  
  private Binding parseBinding() throws IOException, SyntaxError
  {
    require(TokenType.Ident, "identifier expected");
    String id = lastToken.text;
    Position pos = lastToken.position;
    lastToken = tokens.next();
    require(TokenType.Colon, "\":\" expected");
    lastToken = tokens.next();
    Expr init = parseExp0();
    return new Binding(id, init, pos);
  }
  
  // Exp0 ::= Exp1 {";" Exp1}
  
  private Expr parseExp0() throws IOException, SyntaxError
  {
    Expr result = parseExp1();
    Position position = result.position;
    if (lastToken != null && lastToken.type == TokenType.SemiColon) {
      List<Expr> elems = new ArrayList<>();
      elems.add(result);
      do {
        lastToken = tokens.next();
        elems.add(parseExp1());
      }
      while (lastToken != null && lastToken.type == TokenType.SemiColon);
      return new Seq(elems, position);
    }
    return result;
  }
  
  // Exp1 ::= Exp2 {Exp2}
  
  private Expr parseExp1() throws IOException, SyntaxError
  {
    Expr result = parseAtom();
    Position position = result.position;
    while (lastToken != null && atoms.contains(lastToken.type)) {
      result = new Apply(result, parseAtom(), position);
    }
    return result;
  }
  
  private Expr parseAtom() throws IOException, SyntaxError
  {
    if (lastToken == null)
      throw new SyntaxError("unexpected end of input", tokens.position());

    Expr result = null;
    
    if (lastToken.type == TokenType.Int)
      result = new Const(new Int(Integer.parseInt(lastToken.text)), lastToken.position);
    else if (lastToken.type == TokenType.Real)
      result = new Const(new spartan.data.Real(Double.parseDouble(lastToken.text)), lastToken.position);
    else if (lastToken.type == TokenType.True)
      result = new Const(Bool.True, lastToken.position);
    else if (lastToken.type == TokenType.False)
      result = new Const(Bool.False, lastToken.position);
    else if (lastToken.type == TokenType.Ident)
      result = new VarRef(lastToken.text, lastToken.position);
    else if (lastToken.type == TokenType.LftParen)
      result = parseLftParen();
    else if (lastToken.type == TokenType.LftBrack)
      result = parseLftBrack();
    else if (lastToken.type == TokenType.LftBrace)
      result = parseLftBrace();
    else if (lastToken.type == TokenType.If)
      result = parseIf();
    else if (lastToken.type == TokenType.Let)
      result = parseLet();
    else if (lastToken.type == TokenType.Fun)
      result = parseFun();
    else if (ops.containsKey(lastToken.type))
      result = new Op(lastToken.text, ops.get(lastToken.type), lastToken.position);
    else if (lastToken.type == TokenType.At)
      result = parseAt();
    else
      throw new SyntaxError("unexpected token " + lastToken.text, lastToken.position);

    lastToken = tokens.next();
    return result;
  }

  private Expr parseLftParen() throws IOException, SyntaxError
  {
    Position position = lastToken.position;
    lastToken = tokens.next();
    
    if (lastToken != null && lastToken.type == TokenType.RgtParen) {
      return new Const(Unit.Instance, position);
    }

    Expr result = parseExp0();
    
    if (lastToken != null && lastToken.type == TokenType.Comma) {
      List<Expr> elems = new ArrayList<>();
      elems.add(result);
      do {
        lastToken = tokens.next();
        elems.add(parseExp0());
      }
      while (lastToken != null && lastToken.type == TokenType.Comma);
      require(TokenType.RgtParen, "\")\" expected");
      return new spartan.ast.Tuple(elems, position);
    }
    
    require(TokenType.RgtParen, "\")\" expected");
    return result;
  }
  
  private Expr parseLftBrack() throws IOException, SyntaxError
  {
    Position position = lastToken.position;
    lastToken = tokens.next();
    
    if (lastToken != null && lastToken.type == TokenType.RgtBrack) {
      return new Const(spartan.data.List.Empty, position);
    }
    
    List<Expr> elems = new ArrayList<>();
    elems.add(parseExp0());
    
    while (lastToken != null && lastToken.type == TokenType.Comma) {
      lastToken = tokens.next();
      elems.add(parseExp0());
    }
    
    require(TokenType.RgtBrack, "\"]\" expected");
    return new spartan.ast.List(elems, position);
  }
  
  private Expr parseLftBrace() throws IOException, SyntaxError
  {
    Position position = lastToken.position;
    List<Binding> members = new ArrayList<>();
    
    do {
      lastToken = tokens.next();
      members.add(parseBinding());
    }
    while (lastToken != null && lastToken.type == TokenType.Comma);
    
    require(TokenType.RgtBrace, "\"}\" expected");
    return new spartan.ast.Record(members, position);
  }
  
  private Expr parseIf() throws IOException, SyntaxError
  {
    Position position = lastToken.position;
    List<Branch> thenExprs = new ArrayList<>();
    lastToken = tokens.next();
    Expr test = parseExp0();
    require(TokenType.Then, "\"then\" required");
    lastToken = tokens.next();
    Expr body = parseExp0();
    thenExprs.add(new Branch(test, body, test.position));
    while (lastToken != null && lastToken.type == TokenType.Elif) {
      lastToken = tokens.next();
      test = parseExp0();
      require(TokenType.Then, "\"then\" required");
      lastToken = tokens.next();
      body = parseExp0();
      thenExprs.add(new Branch(test, body, test.position));
    }
    if (lastToken != null && lastToken.type == TokenType.Else) {
      lastToken = tokens.next();
      body = parseExp0();
    }
    else {
      body = new Const(Unit.Instance, null);
    }
    require(TokenType.End, "\"end\" required");
    return new If(thenExprs, body, position);
  }
  
  private Expr parseLet() throws IOException, SyntaxError
  {
    Position position = lastToken.position;
    lastToken = tokens.next();
    boolean isRec = false;
    if (lastToken != null && lastToken.type == TokenType.Rec) {
      isRec = true;
      lastToken = tokens.next();
    }
    List<Binding> bindings = new ArrayList<>();
    bindings.add(parseBinding());
    while (lastToken != null && lastToken.type == TokenType.Comma) {
      lastToken = tokens.next();
      bindings.add(parseBinding());
    }
    require(TokenType.In, "\"in\" required");
    lastToken = tokens.next();
    Expr body = parseExp0();
    require(TokenType.End, "\"end\" required");
    return isRec ? new LetRec(bindings, body, position)
                 : new Let(bindings, body, position);
  }
  
  private Expr parseFun() throws IOException, SyntaxError
  {
    Position position = lastToken.position;
    lastToken = tokens.next();
    require(TokenType.Ident, "identifier required");
    List<String> params = new ArrayList<>();
    params.add(lastToken.text);
    lastToken = tokens.next();
    while (lastToken != null && lastToken.type == TokenType.Ident) {
      params.add(lastToken.text);
      lastToken = tokens.next();
    }
    require(TokenType.RgtArrow, "\"=>\" required");
    lastToken = tokens.next();
    Expr body = parseExp0();
    require(TokenType.End, "\"end\" required");
    return curry(params.iterator(), body, position);
  }
  
  private Expr curry(Iterator<String> params, Expr body, Position position)
  {
    String param = params.next();
    if (!params.hasNext())
      return new Fun(param, body, position);
    else
      return new Fun(param, curry(params, body, position), position);
  }

  private Expr parseAt() throws IOException, SyntaxError
  {
    Position position = lastToken.position;
    lastToken = tokens.next();
    if (lastToken.type == TokenType.Int)
      return new spartan.ast.TupleSelector(Integer.parseInt(lastToken.text), position);
    else if (lastToken.type == TokenType.Ident)
      return new spartan.ast.RecordSelector(lastToken.text, position);
    else
      throw new SyntaxError("expected integer or identifier", position);
  }
    
  private void require(TokenType expectedType, String errorMessage)
  throws SyntaxError, IOException
  {
    if (lastToken == null)
      throw new SyntaxError(errorMessage, tokens.position());
    
    if (lastToken.type != expectedType)
      throw new SyntaxError(errorMessage, lastToken.position);
  }
}
