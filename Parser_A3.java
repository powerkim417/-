package MiniC.Parser;

import MiniC.Scanner.Scanner;
import MiniC.Scanner.Token;
import MiniC.Scanner.SourcePos;
import MiniC.Parser.SyntaxError;
import MiniC.ErrorReporter;
import MiniC.AstGen.*;


public class Parser {

  private Scanner scanner;
  private ErrorReporter errorReporter;
  private Token currentToken;
  private SourcePos previousTokenPosition;

  public Parser(Scanner lexer, ErrorReporter reporter) {
    scanner = lexer;
    errorReporter = reporter;
  }

  // accept() checks whether the current token matches tokenExpected.
  // If so, it fetches the next token.
  // If not, it reports a syntax error.
  void accept (int tokenExpected) throws SyntaxError {
//  	tokenPrint(Thread.currentThread().getStackTrace()[2].getLineNumber());
    if (currentToken.kind == tokenExpected) {
      previousTokenPosition = currentToken.GetSourcePos();
      currentToken = scanner.scan();
    } else {
      syntaxError("\"%\" expected here", Token.spell(tokenExpected));
    }
  }

  // acceptIt() unconditionally accepts the current token
  // and fetches the next token from the scanner.
  void acceptIt() {
//  	tokenPrint(Thread.currentThread().getStackTrace()[2].getLineNumber());
    previousTokenPosition = currentToken.GetSourcePos();
    currentToken = scanner.scan();
  }

  // start records the position of the start of a phrase.
  // This is defined to be the position of the first
  // character of the first token of the phrase.
  void start(SourcePos position) {
    position.StartCol = currentToken.GetSourcePos().StartCol;
    position.StartLine = currentToken.GetSourcePos().StartLine;
  }

  // finish records the position of the end of a phrase.
  // This is defined to be the position of the last
  // character of the last token of the phrase.
  void finish(SourcePos position) {
    position.EndCol = previousTokenPosition.EndCol;
    position.EndLine = previousTokenPosition.EndLine;
  }

  void syntaxError(String messageTemplate, String tokenQuoted) throws SyntaxError {
    SourcePos pos = currentToken.GetSourcePos();
    errorReporter.reportError(messageTemplate, tokenQuoted, pos);
    throw(new SyntaxError());
  }

  boolean isTypeSpecifier(int token) {
    if(token == Token.VOID ||
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

  void posPrint() {

  	System.out.println(previousTokenPosition.StartLine
        + "(" + previousTokenPosition.StartCol + ").."
        +  previousTokenPosition.EndLine
        + "(" + previousTokenPosition.EndCol + ")");
  }

  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseArrayIndexDecl (Type T):
  //
  // Take [INTLITERAL] and generate an ArrayType
  //
  ///////////////////////////////////////////////////////////////////////////////

  public ArrayType parseArrayIndexDecl(Type T) throws SyntaxError {
    IntLiteral L;
    IntExpr IE;
    accept(Token.LEFTBRACKET);
    SourcePos pos = currentToken.GetSourcePos();
    L = new IntLiteral(currentToken.GetLexeme(), pos);
    accept(Token.INTLITERAL);
    accept(Token.RIGHTBRACKET);
    IE = new IntExpr (L, pos);
    return new ArrayType (T, IE, previousTokenPosition);
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // toplevel parse() routine:
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Program parse() {

    Program ProgramAST = null;

    previousTokenPosition = new SourcePos();
    previousTokenPosition.StartLine = 0;
    previousTokenPosition.StartCol = 0;
    previousTokenPosition.EndLine = 0;
    previousTokenPosition.EndCol = 0;

    currentToken = scanner.scan(); // get first token from scanner...

    try {
      ProgramAST = parseProgram();
      if (currentToken.kind != Token.EOF) {
        syntaxError("\"%\" not expected after end of program",
            currentToken.GetLexeme());
      }
    }
    catch (SyntaxError s) { return null; }
    return ProgramAST;
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseProgram():
  //
  // program ::= ( (VOID|INT|BOOL|FLOAT) ID ( FunPart | VarPart ) )* ";"
  //
  ///////////////////////////////////////////////////////////////////////////////

  // parseProgDecls: recursive helper function to facilitate AST construction.
  public Decl parseProgDecls () throws SyntaxError {
    if (! isTypeSpecifier(currentToken.kind)) {
      return new EmptyDecl (previousTokenPosition);
    }
    SourcePos pos = new SourcePos();
    start(pos);
    Type T = parseTypeSpecifier();
    ID Ident = parseID();
    if(currentToken.kind == Token.LEFTPAREN) {
      Decl newD = parseFunPart(T, Ident, pos);
      return new DeclSequence (newD, parseProgDecls(), previousTokenPosition);
    } else {
      DeclSequence Vars = parseVarPart(T, Ident);
      DeclSequence VarsTail = Vars.GetRightmostDeclSequenceNode();
      Decl RemainderDecls = parseProgDecls();
      VarsTail.SetRightSubtree (RemainderDecls);
      return Vars;
    }
  }

  public Program parseProgram() throws SyntaxError {
    SourcePos pos = new SourcePos();
    start(pos);
    Decl D = parseProgDecls();
    finish(pos);
    Program P = new Program (D, pos);
    return P;
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseFunPart():
  //
  // FunPart ::= ( "(" ParamsList? ")" CompoundStmt )
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Decl parseFunPart(Type T, ID Ident, SourcePos pos) throws SyntaxError {

    // We already know that the current token is "(".
    // Otherwise use accept() !
    acceptIt();
    Decl PDecl = parseParamsList(); // can also be empty...
    accept(Token.RIGHTPAREN);
    CompoundStmt CStmt = parseCompoundStmt();
    finish(pos);
    return new FunDecl (T, Ident, PDecl, CStmt, pos);
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseParamsList():
  //
  // ParamsList ::= ParameterDecl ( "," ParameterDecl ) *
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Decl parseParamsList() throws SyntaxError {
    if (!isTypeSpecifier(currentToken.kind)) {
      return new EmptyFormalParamDecl(previousTokenPosition);
    }
    Decl PDecl = parseParameterDecl();
    if (currentToken.kind == Token.COMMA) {
      acceptIt();
    }
    return new FormalParamDeclSequence (PDecl,
        parseParamsList(), previousTokenPosition);
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseParameterDecl():
  //
  // ParameterDecl ::= (VOID|INT|BOOL|FLOAT) Declarator
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Decl parseParameterDecl() throws SyntaxError {
    Type T = null;
    Decl D = null;

    SourcePos pos = new SourcePos();
    start(pos);
    if (isTypeSpecifier(currentToken.kind)) {
      T = parseTypeSpecifier();
    } else {
      syntaxError("Type specifier instead of % expected",
          Token.spell(currentToken.kind));
    }
    D = parseDeclarator(T, pos);
    return D;
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseDeclarator():
  //
  // Declarator ::= ID ( "[" INTLITERAL "]" )?
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Decl parseDeclarator(Type T, SourcePos pos) throws SyntaxError {
    ID Ident = parseID();
    if (currentToken.kind == Token.LEFTBRACKET) {
      ArrayType ArrT = parseArrayIndexDecl(T);
      finish(pos);
      return new FormalParamDecl (ArrT, Ident, pos);
    }
    finish(pos);
    return new FormalParamDecl (T, Ident, pos);
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseVarPart():
  //
  // VarPart ::= ( "[" INTLITERAL "]" )?  ( "=" initializer ) ? ( "," init_decl)* ";"
  //
  ///////////////////////////////////////////////////////////////////////////////

  public DeclSequence parseVarPart(Type T, ID Ident) throws SyntaxError {
    Type theType = T;
    Decl D;
    DeclSequence Seq = null;
    Expr E = new EmptyExpr(previousTokenPosition);
    if (currentToken.kind == Token.LEFTBRACKET) {
      theType = parseArrayIndexDecl(T);
    }
    if (currentToken.kind == Token.ASSIGN) {
      acceptIt();
      // You can use the following code after you have implemented
      // parseInitializer():
       E = parseInitializer();
    }
    D = new VarDecl (theType, Ident, E, previousTokenPosition);
    // You can use the following code after implementation of parseInitDecl():

       if (currentToken.kind == Token.COMMA) {
       acceptIt();
       Seq = new DeclSequence (D, parseInitDecl(T), previousTokenPosition);
       } else {
       Seq = new DeclSequence (D, new EmptyDecl (previousTokenPosition),
       previousTokenPosition);
       }

    accept (Token.SEMICOLON);
    return Seq;
  }

  // Initalizer ::= expr
  //						| "{" expr ("," expr)* "}"
  public Expr parseInitializer() throws SyntaxError {
  	Expr retExpr = null;
  	if (currentToken.kind == Token.LEFTBRACE) {
  		acceptIt(); // "{"
  		retExpr = parseExpr();
      if (currentToken.kind == Token.COMMA) { // {expr, ...}
        acceptIt();
        retExpr = new ExprSequence(retExpr, parseInitializer(), previousTokenPosition);
      }
      else { // {expr}, cur token kind = "]".
    		retExpr = new ExprSequence(retExpr, new EmptyExpr(previousTokenPosition), previousTokenPosition);
      }
      accept(Token.RIGHTBRACE);
  	}
  	else {
  		retExpr = parseExpr();
  		// in case of recursion
  		if (currentToken.kind == Token.RIGHTBRACE) { // recursion ends
  			retExpr = new ExprSequence(retExpr, new EmptyExpr(previousTokenPosition), previousTokenPosition);
  		}
  		while (currentToken.kind == Token.COMMA) {
        acceptIt();
        retExpr = new ExprSequence(retExpr, parseInitializer(), previousTokenPosition);
      }
  	}
  	return retExpr;
  }

  // InitDecl ::= ID ( "[" INTLITERAL "]" )? ("=" initializer)?

  public Decl parseInitDecl(Type T) throws SyntaxError {
    SourcePos pos = currentToken.GetSourcePos();
    start(pos);
  	ID id = parseID();
  	Type theType = T;
    Decl D;
    DeclSequence Seq = null;
    Expr E = new EmptyExpr(previousTokenPosition);
    if (currentToken.kind == Token.LEFTBRACKET) {
      theType = parseArrayIndexDecl(T);
    }
    if (currentToken.kind == Token.ASSIGN) {
      acceptIt();
      // You can use the following code after you have implemented
      // parseInitializer():
       E = parseInitializer();
    }
    finish(pos);
    D = new VarDecl (theType, id, E, pos);
    // in case of multiple decls
    if (currentToken.kind == Token.COMMA) {
      acceptIt();
      finish(pos);
      D = new DeclSequence (D, parseInitDecl(T), pos);
    }
    else {
    finish(pos);
      D = new DeclSequence (D, new EmptyDecl (previousTokenPosition),
      pos);
    }
    return D;
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseUnaryExpr():
  //
  // UnaryExpr ::= ("+"|"-"|"!")* PrimaryExpr
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Expr parseUnaryExpr() throws SyntaxError {
    while (currentToken.kind == Token.PLUS ||
        currentToken.kind == Token.MINUS ||
        currentToken.kind == Token.NOT) {
      Operator opAST = new Operator (currentToken.GetLexeme(),
          previousTokenPosition);
      acceptIt();
      return new UnaryExpr (opAST, parseUnaryExpr(), previousTokenPosition);
    }
    return parsePrimaryExpr();
  }

  public Expr parseMultExpr() throws SyntaxError {
  	Expr lE = parseUnaryExpr();
    while (currentToken.kind == Token.TIMES ||
        currentToken.kind == Token.DIV) {
      Operator opAST = new Operator (currentToken.GetLexeme(),
          previousTokenPosition);
      acceptIt();
      Expr rE = parseUnaryExpr();
      lE = new BinaryExpr(lE, opAST, rE, previousTokenPosition);
    }
    return lE;
  }

  public Expr parseAddExpr() throws SyntaxError {
  	Expr lE = parseMultExpr();
    while (currentToken.kind == Token.PLUS ||
        currentToken.kind == Token.MINUS) {
      Operator opAST = new Operator (currentToken.GetLexeme(),
          previousTokenPosition);
      acceptIt();
      Expr rE = parseMultExpr();
      lE = new BinaryExpr(lE, opAST, rE, previousTokenPosition);
    }
    return lE;
  }

  public Expr parseRelationalExpr() throws SyntaxError {
  	Expr lE = parseAddExpr();
    if (currentToken.kind == Token.EQ ||
        currentToken.kind == Token.NOTEQ ||
        currentToken.kind == Token.LESS ||
        currentToken.kind == Token.LESSEQ ||
        currentToken.kind == Token.GREATER ||
        currentToken.kind == Token.GREATEREQ) {
      Operator opAST = new Operator (currentToken.GetLexeme(),
          previousTokenPosition);
      acceptIt();
      Expr rE = parseAddExpr();
      lE = new BinaryExpr(lE, opAST, rE, previousTokenPosition);
    }
    return lE;
  }

  public Expr parseAndExpr() throws SyntaxError {
  	Expr lE = parseRelationalExpr();
    while (currentToken.kind == Token.AND) {
      Operator opAST = new Operator (currentToken.GetLexeme(),
          previousTokenPosition);
      acceptIt();
      Expr rE = parseRelationalExpr();
      lE = new BinaryExpr(lE, opAST, rE, previousTokenPosition);
    }
    return lE;
  }

  public Expr parseOrExpr() throws SyntaxError {
  	Expr lE = parseAndExpr();
    while (currentToken.kind == Token.OR) {
      Operator opAST = new Operator (currentToken.GetLexeme(),
          previousTokenPosition);
      acceptIt();
      Expr rE = parseAndExpr();
      lE = new BinaryExpr(lE, opAST, rE, previousTokenPosition);
    }
    return lE;
  }

  ///////////////////////////////////////////////////////////////////////////////
  //
  // parsePrimaryExpr():
  //
  // PrimaryExpr ::= ID arglist?
  //              |  ID "[" expr "]"
  //              |  "(" expr ")"
  //              |  INTLITERAL | BOOLLITERAL | FLOATLITERAL | STRINGLITERAL
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Expr parsePrimaryExpr() throws SyntaxError {
    Expr retExpr = null;
    SourcePos pos = currentToken.GetSourcePos();
    start(pos);
    switch (currentToken.kind) {
    case Token.INTLITERAL:
      IntLiteral IL = new IntLiteral(currentToken.GetLexeme(), currentToken.GetSourcePos());
    	acceptIt();
      finish(pos);
      retExpr = new IntExpr(IL, pos);
      break;
    case Token.BOOLLITERAL:
      BoolLiteral BL = new BoolLiteral(currentToken.GetLexeme(), currentToken.GetSourcePos());
    	acceptIt();
      finish(pos);
      retExpr = new BoolExpr(BL, pos);
      break;
    case Token.FLOATLITERAL:
      FloatLiteral FL = new FloatLiteral(currentToken.GetLexeme(), currentToken.GetSourcePos());
    	acceptIt();
      finish(pos);
      retExpr = new FloatExpr(FL, pos);
      break;
    case Token.STRINGLITERAL:
      StringLiteral SL = new StringLiteral(currentToken.GetLexeme(), currentToken.GetSourcePos());
    	acceptIt();
      finish(pos);
      retExpr = new StringExpr(SL, previousTokenPosition);
      break;
    case Token.LEFTPAREN:
      acceptIt();
      finish(pos);
      retExpr = parseExpr();
      accept(Token.RIGHTPAREN);
      break;
    case Token.ID:
      ID id = parseID();
      Expr idExpr = new VarExpr(id, previousTokenPosition);
      if (currentToken.kind == Token.LEFTPAREN) { // arglist id(expr, expr)
        finish(pos);
      	retExpr = new CallExpr(id, parseArgList(), pos);
      }
      else if (currentToken.kind == Token.LEFTBRACKET) { // array id[expr]
        acceptIt();
        Expr arrExpr = parseExpr();
        accept(Token.RIGHTBRACKET);
        finish(pos);
        retExpr = new ArrayExpr(idExpr, arrExpr, pos);
      }
      else {
        finish(pos);
      	retExpr = idExpr;
      }
      break;
    default:
      syntaxError("\"%\" not a primary expression", currentToken.GetLexeme());
    }

    return retExpr;
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseCompoundStmt():
  //
  // CompoundStmt ::= "{" VariableDef* Stmt* "}"
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Decl parseCompoundDecls () throws SyntaxError {
    if (!isTypeSpecifier(currentToken.kind)) {
      return new EmptyDecl (previousTokenPosition);
    }
    Type T = parseTypeSpecifier();
    ID Ident = parseID();
    DeclSequence Vars = parseVarPart(T, Ident);
    DeclSequence VarsTail = Vars.GetRightmostDeclSequenceNode();
    Decl RemainderDecls = parseCompoundDecls();
    VarsTail.SetRightSubtree (RemainderDecls);
    return Vars;
  }

  public Stmt parseCompoundStmts () throws SyntaxError {
    if (! (currentToken.kind == Token.LEFTBRACE ||
          currentToken.kind == Token.IF ||
          currentToken.kind == Token.WHILE ||
          currentToken.kind == Token.FOR ||
          currentToken.kind == Token.RETURN ||
          currentToken.kind == Token.ID)
       ) {
      return new EmptyStmt(previousTokenPosition);
    }
    Stmt S = null;
    // You can use the following code after implementation of parseStmt():
    S = parseStmt();
    return new StmtSequence (S, parseCompoundStmts(), previousTokenPosition);
  }

  public CompoundStmt parseCompoundStmt() throws SyntaxError {
    SourcePos pos = new SourcePos();
    start(pos);
    accept(Token.LEFTBRACE);
    Decl D = parseCompoundDecls(); // variable def *
    Stmt S = parseCompoundStmts(); // stmt *
    accept(Token.RIGHTBRACE);
    finish(pos);
    if ( (D.getClass() == EmptyDecl.class) &&
        (S.getClass() == EmptyStmt.class)) {
      return new EmptyCompoundStmt (previousTokenPosition);
    } else {
      return new CompoundStmt (D, S, pos);
    }
  }

  public Stmt parseStmt() throws SyntaxError {
  	Stmt S = new EmptyStmt(previousTokenPosition);
    SourcePos pos = currentToken.GetSourcePos();
    start(pos);
  	switch (currentToken.kind) {
    case Token.LEFTBRACE: // compound-stmt
    	finish(pos);
      S = parseCompoundStmt();
      break;
    case Token.IF: // if-stmt
      acceptIt(); // "if"
      accept(Token.LEFTPAREN);
      Expr ifE = parseExpr();
      accept(Token.RIGHTPAREN);
      Stmt thenS = parseStmt();
      if (currentToken.kind == Token.ELSE) {
        acceptIt(); // else
        Stmt elseS = parseStmt();
      	finish(pos);
        S = new IfStmt(ifE, thenS, elseS, pos);
        break;
      }
    	finish(pos);
      S = new IfStmt(ifE, thenS, pos);
      break;
    case Token.WHILE: // while-stmt
    	acceptIt(); // "while"
      accept(Token.LEFTPAREN);
      Expr whileE = parseExpr();
      accept(Token.RIGHTPAREN);
      Stmt whileS = parseStmt();
    	finish(pos);
      S = new WhileStmt(whileE, whileS, pos);
      break;
    case Token.FOR: // for-stmt
      acceptIt(); // "for"
      accept(Token.LEFTPAREN);
      Expr for1E = new EmptyExpr(previousTokenPosition);
      Expr for2E = new EmptyExpr(previousTokenPosition);
      Expr for3E = new EmptyExpr(previousTokenPosition);
      if (!(currentToken.kind == Token.SEMICOLON)) {
        for1E = parseAsgnExpr();
      }
      accept(Token.SEMICOLON);
      if (!(currentToken.kind == Token.SEMICOLON)) {
      	for2E = parseExpr();
      }
      accept(Token.SEMICOLON);
      if (!(currentToken.kind == Token.RIGHTPAREN)) {
      	for3E = parseAsgnExpr();
      }
      accept(Token.RIGHTPAREN);
      Stmt forS = parseStmt();
    	finish(pos);
      S = new ForStmt(for1E, for2E, for3E, forS, pos);
      break;
    case Token.RETURN: // return-stmt
    	acceptIt(); // "return"
      if (!(currentToken.kind == Token.SEMICOLON)) {
      	finish(pos);
        S = new ReturnStmt(parseExpr(), pos);
      }
      else { // return;
      	finish(pos);
      	S = new ReturnStmt(new EmptyExpr(previousTokenPosition), pos);
      }
    	accept(Token.SEMICOLON);
      break;
    case Token.ID:
      SourcePos ePos = new SourcePos();
      start(ePos);
    	ID id = parseID();
      Expr idExpr = new VarExpr(id, previousTokenPosition);
      if (currentToken.kind == Token.ASSIGN) { // assign stmt
        acceptIt(); // "="
        Expr asgnExpr = parseExpr();
        accept(Token.SEMICOLON);
      	finish(pos);
        S = new AssignStmt(idExpr, asgnExpr, pos);
      }
      else if (currentToken.kind == Token.LEFTBRACKET) { // assign stmt, array expr
        acceptIt(); // "["
        Expr arrExpr = parseExpr();
        accept(Token.RIGHTBRACKET);
        finish(ePos);
        Expr idArrExpr = new ArrayExpr(idExpr, arrExpr, ePos);
        accept(Token.ASSIGN);
        Expr asgnExpr = parseExpr();
        accept(Token.SEMICOLON);
      	finish(pos);
        S = new AssignStmt(idArrExpr, asgnExpr, pos);
      }
      else if (currentToken.kind == Token.LEFTPAREN) { // call stmt
        Expr alExpr = parseArgList();
        finish(ePos);
      	Expr cExpr = new CallExpr(id, alExpr, ePos);
        accept(Token.SEMICOLON);
      	finish(pos);
      	S = new CallStmt(cExpr, pos);
      }
      else {
        syntaxError("\"%\" not expected here", currentToken.spell(currentToken.kind));
      }
      break;
      default:
        syntaxError("\"%\" not a statement", currentToken.GetLexeme());
    }
		return S;

  }

  public Expr parseAsgnExpr() throws SyntaxError {
  	ID id = parseID();
    Expr idExpr = new VarExpr(id, previousTokenPosition);
    accept(Token.ASSIGN);
    Expr asgnExpr = parseExpr();
    return new AssignExpr(idExpr, asgnExpr, previousTokenPosition);
  }

  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseArgList():
  //
  // ArgList ::= "(" ( arg ( "," arg )* )? ")"
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Expr parseArgs() throws SyntaxError {
    if (currentToken.kind == Token.RIGHTPAREN) {
      return new  EmptyActualParam (previousTokenPosition);
    }
    Expr Params = null;
    /*
     * You can use the following code after you have implemented parseExpr() aso.:
     *
     *
     */
     Params = new ActualParam (parseExpr(), previousTokenPosition);
     if (currentToken.kind == Token.COMMA) {
     acceptIt();
     }
    return new ActualParamSequence (Params, parseArgs(), previousTokenPosition);
  }

  public Expr parseArgList() throws SyntaxError {
    accept(Token.LEFTPAREN);
    Expr Params = parseArgs();
    accept(Token.RIGHTPAREN);
    return Params;
  }

  public Expr parseExpr() throws SyntaxError {
  	return parseOrExpr();

  }

  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseID():
  //
  // ID (terminal)
  //
  ///////////////////////////////////////////////////////////////////////////////

  public ID parseID() throws SyntaxError {
    ID Ident = new ID(currentToken.GetLexeme(), currentToken.GetSourcePos());
    accept(Token.ID);
    return Ident;
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseTypeSpecifier():
  //
  // VOID | INT | FLOAT | BOOL (all terminals)
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Type parseTypeSpecifier() throws SyntaxError {
    Type T = null;
    switch (currentToken.kind) {
      case Token.INT:
        T = new IntType(currentToken.GetSourcePos());
        break;
      case Token.FLOAT:
        T = new FloatType(currentToken.GetSourcePos());
        break;
      case Token.BOOL:
        T = new BoolType(currentToken.GetSourcePos());
        break;
      case Token.VOID:
        T = new VoidType(currentToken.GetSourcePos());
        break;
      default:
        syntaxError("Type specifier expected", "");
    }
    acceptIt();
    return T;
  }

}
