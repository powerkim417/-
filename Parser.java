package MiniC.Parser;


import MiniC.Scanner.Token;
import MiniC.Scanner.SourcePos;
import MiniC.Parser.SyntaxError;
import MiniC.Scanner.Scanner;
import MiniC.ErrorReporter;

public class Parser {

  private Scanner scanner;
  private ErrorReporter errorReporter;
  private Token currentToken;

  public Parser(Scanner lexer, ErrorReporter reporter) {
    scanner = lexer;
    errorReporter = reporter;
  }

  // accept() checks whether the current token matches tokenExpected.
  // If so, it fetches the next token.
  // If not, it reports a syntax error.
  void accept (int tokenExpected) throws SyntaxError {
//    tokenPrint(Thread.currentThread().getStackTrace()[2].getLineNumber());
    if (currentToken.kind == tokenExpected) {
      currentToken = scanner.scan();
    } else {
      syntaxError("\"%\" expected here", Token.spell(tokenExpected));
    }
  }

  // acceptIt() unconditionally accepts the current token
  // and fetches the next token from the scanner.
  void acceptIt() {
//    tokenPrint(Thread.currentThread().getStackTrace()[2].getLineNumber());
    currentToken = scanner.scan();
  }

  void syntaxError(String messageTemplate, String tokenQuoted) throws SyntaxError {
    SourcePos pos = currentToken.GetSourcePos();
    errorReporter.reportError(messageTemplate, tokenQuoted, pos);
    throw(new SyntaxError());
  }

  boolean isTypeSpecifier(int token) {
    if (token == Token.VOID ||
      token == Token.INT  ||
      token == Token.BOOL ||
      token == Token.FLOAT) {
      return true;
    } else {
      return false;
    }
  }

  boolean isRelationalOp(int token) {
    if (token == Token.EQ ||
      token == Token.NOTEQ ||
      token == Token.LESS ||
      token == Token.LESSEQ ||
      token == Token.GREATER ||
      token == Token.GREATEREQ) {
      return true;
    } else {
      return false;
    }
  }

  boolean isStartOfStmt(int token) {
    if (token == Token.LEFTBRACE ||
        token == Token.IF ||
        token == Token.WHILE ||
        token == Token.FOR ||
        token == Token.RETURN ||
        token == Token.ID) {
        return true;
      } else {
        return false;
      }
    }

  void tokenPrint(int line) {
    System.out.println(currentToken.GetLexeme() + " " + currentToken.kind + " " + line);
  }

  ///////////////////////////////////////////////////////////////////////////////
  //
  // toplevel parse() routine:
  //
  ///////////////////////////////////////////////////////////////////////////////

  public void parse() {

    currentToken = scanner.scan(); // get first token from scanner...

    try {
      parseProgram();
      if (currentToken.kind != Token.EOF) {
        syntaxError("\"%\" not expected after end of program",
            currentToken.GetLexeme());
      }
    }
    catch (SyntaxError s) {return; /* to be refined in Assignment 3...*/ }
    return;
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseProgram():
  //
  // program ::= ( (VOID|INT|BOOL|FLOAT) ID ( FunPart | VarPart ) )*
  //
  ///////////////////////////////////////////////////////////////////////////////

  public void parseProgram() throws SyntaxError {
    while (isTypeSpecifier(currentToken.kind)) {
      acceptIt();
      accept(Token.ID);
      if(currentToken.kind == Token.LEFTPAREN) {
        parseFunPart();
      } else {
        parseVarPart();
      }
    }
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseFunPart():
  //
  // FunPart ::= ( "(" ParamsList? ")" CompoundStmt )
  //
  ///////////////////////////////////////////////////////////////////////////////

  public void parseFunPart() throws SyntaxError {
    // We already know that the current token is "(".
    // Otherwise use accept() !
    acceptIt();
    // curToken: after "("
    if (isTypeSpecifier(currentToken.kind)) { // if ParamsList exists, go into this block,
    										// else, currentToken is RIGHTPAREN, so go to accept, and take new token.
      parseParamsList();
    }
    accept(Token.RIGHTPAREN);
    parseCompoundStmt();
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseParamsList():
  //
  // ParamsList ::= ParamsDecl ( "," ParamsDecl ) *
  //
  // ParamsDecl ::= TypeSpecifier Declarator
  // Declarator ::= ID ( "[" INTLITERAL "]" )?
  //
  ///////////////////////////////////////////////////////////////////////////////

  public void parseParamsList() throws SyntaxError {
    // parse ParamsDecl
    parseParamsDecl();
    // parse ( "," ParamsDecl ) *
    while (currentToken.kind == Token.COMMA) {
      acceptIt();
      parseParamsDecl();
    }
  }

  public void parseParamsDecl() throws SyntaxError {
    // current token is typespecifier.
    // parse TypeSpecifier(T)
    acceptIt();
    // parse Declarator(NT)
    parseDeclarator();
  }

  public void parseDeclarator() throws SyntaxError {
    accept(Token.ID);
    if (currentToken.kind == Token.LEFTBRACKET) {
      acceptIt();
      accept(Token.INTLITERAL);
      accept(Token.RIGHTBRACKET);
    }
  }

  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseCompoundStmt():
  //
  // CompoundStmt ::= "{" VariableDefinition* Stmt* "}"
  //
  // VariableDefinition ::= TypeSpecifier InitDecl ("," InitDecl)* ";"
  //
  ///////////////////////////////////////////////////////////////////////////////

  public void parseCompoundStmt() throws SyntaxError {
    accept(Token.LEFTBRACE);
    // VariableDefinition*
    while (isTypeSpecifier(currentToken.kind)) {
      acceptIt();
      parseInitDecl();
      while (currentToken.kind == Token.COMMA) {
        acceptIt();
        parseInitDecl();
      }
      accept(Token.SEMICOLON);
    }
    // Stmt*
    while (isStartOfStmt(currentToken.kind)) {
      parseStmt();
    }
    accept(Token.RIGHTBRACE);
  }

  public void parseStmt() throws SyntaxError {
    switch (currentToken.kind) {
    case Token.LEFTBRACE: // compound-stmt
      parseCompoundStmt();
      break;
    case Token.IF: // if-stmt
      parseIfStmt();
      break;
    case Token.WHILE: // while-stmt
      parseWhileStmt();
      break;
    case Token.FOR: // for-stmt
      parseForStmt();
      break;
    case Token.RETURN: // return-stmt
    acceptIt();
      if (!(currentToken.kind == Token.SEMICOLON))
        parseExpr();
      accept(Token.SEMICOLON);
      break;
    case Token.ID:
    acceptIt();
      if (currentToken.kind == Token.ASSIGN) {
        acceptIt();
        parseExpr();
        accept(Token.SEMICOLON);
      }
      else if (currentToken.kind == Token.LEFTBRACKET) {
        acceptIt();
        parseExpr();
        accept(Token.RIGHTBRACKET);
        accept(Token.ASSIGN);
        parseExpr();
        accept(Token.SEMICOLON);
      }
      else if (currentToken.kind == Token.LEFTPAREN) {
        parseArgList();
        accept(Token.SEMICOLON);
      }
      else {
        syntaxError("\"%\" not expected here", currentToken.spell(currentToken.kind));
      }
      break;
      default:
        syntaxError("\"%\" not a statement", currentToken.GetLexeme());
    }
  }

  public void parseIfStmt() throws SyntaxError {
    accept(Token.IF);
    accept(Token.LEFTPAREN);
    parseExpr();
    accept(Token.RIGHTPAREN);
    parseStmt();
    if (currentToken.kind == Token.ELSE) {
      acceptIt();
      parseStmt();
    }
  }

  public void parseWhileStmt() throws SyntaxError {
    accept(Token.WHILE);
    accept(Token.LEFTPAREN);
    parseExpr();
    accept(Token.RIGHTPAREN);
    parseStmt();
  }

  public void parseForStmt() throws SyntaxError {
    accept(Token.FOR);
    accept(Token.LEFTPAREN);
    if (!(currentToken.kind == Token.SEMICOLON))
      parseAsgnExpr();
    accept(Token.SEMICOLON);
    if (!(currentToken.kind == Token.SEMICOLON))
      parseExpr();
    accept(Token.SEMICOLON);
    if (!(currentToken.kind == Token.RIGHTPAREN))
      parseAsgnExpr();
    accept(Token.RIGHTPAREN);
    parseStmt();
  }

  public void parseAsgnExpr() throws SyntaxError {
    accept(Token.ID);
    accept(Token.ASSIGN);
    parseExpr();
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseVarPart():
  //
  // VarPart ::= ( "[" INTLITERAL "]" )?  ( "=" initializer ) ? ( "," init_decl)* ";"
  //
  //
  ///////////////////////////////////////////////////////////////////////////////

  public void parseVarPart() throws SyntaxError {
    // parse ( "[" INTLITERAL "]" )?
    if (currentToken.kind == Token.LEFTBRACKET) {
      acceptIt();
      accept(Token.INTLITERAL);
      accept(Token.RIGHTBRACKET);
    }
    // parse ( "=" initializer ) ?
    if (currentToken.kind == Token.ASSIGN) {
      acceptIt();
      parseInitializer();
    }
    // parse ( "," init_decl)*
    while (currentToken.kind == Token.COMMA) {
      acceptIt();
      parseInitDecl();
    }
    // parse ";"
    accept(Token.SEMICOLON);
  }

  // init_decl ::= declarator ( "=" initializer ) ?
  public void parseInitDecl() throws SyntaxError {
    parseDeclarator();
    // parse ( "=" initializer ) ?
    if (currentToken.kind == Token.ASSIGN) {
      acceptIt();
      parseInitializer();
    }
  }

  // initializer ::= expr
  //              | "{" expr ( "," expr )* "}"
  public void parseInitializer() throws SyntaxError {
    if (currentToken.kind == Token.LEFTBRACE) {
      acceptIt();
      parseExpr();
      while (currentToken.kind == Token.COMMA) {
        acceptIt();
        parseExpr();
      }
      accept(Token.RIGHTBRACE);
    }
    else {
      parseExpr(); // expr, or error
    }
  }

  public void parseExpr() throws SyntaxError {
    parseOrExpr();
  }

  public void parseOrExpr() throws SyntaxError {
    parseAndExpr();
    while (currentToken.kind == Token.OR) {
      acceptIt();
      parseAndExpr();
    }
  }

  public void parseAndExpr() throws SyntaxError {
    parseRelationalExpr();
    while (currentToken.kind == Token.AND) {
      acceptIt();
      parseRelationalExpr();
    }
  }

  public void parseRelationalExpr() throws SyntaxError {
    parseAddExpr();
    if (isRelationalOp(currentToken.kind)) {
      acceptIt();
      parseAddExpr();
    }
  }

  public void parseAddExpr() throws SyntaxError {
    parseMultExpr();
    while (currentToken.kind == Token.PLUS || currentToken.kind == Token.MINUS) {
      acceptIt();
      parseMultExpr();
    }
  }

  public void parseMultExpr() throws SyntaxError {
    parseUnaryExpr();
    while (currentToken.kind == Token.TIMES || currentToken.kind == Token.DIV) {
      acceptIt();
      parseUnaryExpr();
    }
  }

  public void parseUnaryExpr() throws SyntaxError {
    while (currentToken.kind == Token.PLUS || currentToken.kind == Token.MINUS || currentToken.kind == Token.NOT) {
      acceptIt();
    }
    parsePrimaryExpr();
  }

  public void parsePrimaryExpr() throws SyntaxError {
    switch (currentToken.kind) {
    case Token.INTLITERAL:
    case Token.BOOLLITERAL:
    case Token.FLOATLITERAL:
    case Token.STRINGLITERAL:
      acceptIt();
      break;
    case Token.LEFTPAREN:
      acceptIt();
      parseExpr();
      accept(Token.RIGHTPAREN);
      break;
    case Token.ID:
      acceptIt();
      if (currentToken.kind == Token.LEFTPAREN) { // arglist
        parseArgList();
      }
      else if (currentToken.kind == Token.LEFTBRACKET) {
        acceptIt();
        parseExpr();
        accept(Token.RIGHTBRACKET);
      }
      break;
    default:
      syntaxError("\"%\" not a primary expression", currentToken.GetLexeme());
    }
  }

  public void parseArgList() throws SyntaxError {
    accept(Token.LEFTPAREN);
    if (!(currentToken.kind == Token.RIGHTPAREN)) {
      parseExpr();
      while (currentToken.kind == Token.COMMA) {
        acceptIt();
        parseExpr();
      }
    }
    accept(Token.RIGHTPAREN);
  }
}
