package MiniC.Scanner;

import MiniC.Scanner.SourceFile;
import MiniC.Scanner.Token;

public final class Scanner {

  private SourceFile sourceFile;

  private char currentChar;
  private boolean verbose;
  private StringBuffer currentLexeme;
  private boolean currentlyScanningToken;
  private int currentLineNr;
  private int currentColNr;
  ///
  private StringBuffer rescanBuffer = new StringBuffer("");
  public final static int TOKEN_COMMENT = -1;

  private boolean isDigit(char c) {
    return (c >= '0' && c <= '9');
  }

  private boolean isLetter(char c) {
	  return ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c == '_'));
  }


///////////////////////////////////////////////////////////////////////////////

  public Scanner(SourceFile source) {
    sourceFile = source;
    currentChar = sourceFile.readChar();
    verbose = false;
    currentLineNr = 1;
    currentColNr= 0;
  }

  public void enableDebugging() {
    verbose = true;
  }

  // takeIt appends the current character to the current token, and gets
  // the next character from the source program (or the to-be-implemented
  // "untake" buffer in case of look-ahead characters that got 'pushed back'
  // into the input stream).

  private void takeIt() {
    if (currentlyScanningToken)
    {
      currentLexeme.append(currentChar);
    }
    if (currentChar == '\n') {
    	currentLineNr++;
    	currentColNr = 0;
    }
  	else {
      currentColNr++;
    }
    if (rescanBuffer.length() != 0) {
    	currentChar = rescanBuffer.charAt(0);
    	rescanBuffer.deleteCharAt(0);
    }
    else {
    	currentChar = sourceFile.readChar();
    }
  }

  private void storeBuffer() {
  	rescanBuffer.insert(0, currentChar);
  	currentChar = currentLexeme.charAt(currentLexeme.length()-1);
  	currentLexeme.deleteCharAt(currentLexeme.length()-1);
  	currentColNr--;
  }

  private int scanToken() {
    switch (currentChar) {

    case '0':  case '1':  case '2':  case '3':  case '4':
    case '5':  case '6':  case '7':  case '8':  case '9':
      takeIt();
      if (isDigit(currentChar)) {
      	while (isDigit(currentChar)) {
      		takeIt();
      	}
      	return Token.INTLITERAL;
      }
      else if (currentChar == '.') { // .
      	takeIt();
      	if (currentChar == 'e' || currentChar == 'E') { // e|E
        	takeIt();
  	    	if (currentChar == '+' || currentChar == '-') { // +|-
  	    	  takeIt();
  	    	  if (isDigit(currentChar)) { // d+
  	        	while (isDigit(currentChar)) {
  	        		takeIt();
  	        	}
  	        	return Token.FLOATLITERAL; // d+ . e|E +|- d+: float case 7.
  	    	  }
  	    	  else {
  	    	  	while (!(currentChar == 'e' || currentChar == 'E')) { // delete until e. e becomes currentChar
        	  		storeBuffer();
  	    	  	}
  		    		return Token.FLOATLITERAL; // d+ . (e|E +|-) ERROR
  	    	  }
  	    	}
  	    	else if (isDigit(currentChar)) { // d+
  	      	while (isDigit(currentChar)) {
  	      		takeIt();
  	      	}
  	      	return Token.FLOATLITERAL; // d+ . e|E d+: float case 6.
  	    	}
  	    	else {
      	  	while (!(currentChar == 'e' || currentChar == 'E')) { // delete until e. e becomes currentChar
      	  		storeBuffer();
      	  	}
  	    		return Token.FLOATLITERAL; // d+ . (e|E) ERROR
  	    	}
        }
      	else if (isDigit(currentChar)) { // d+
	      	while (isDigit(currentChar)) {
	      		takeIt();
	      	}
      		if (currentChar == 'e' || currentChar == 'E') { // e|E
          	takeIt();
    	    	if (currentChar == '+' || currentChar == '-') { // +|-
    	    	  takeIt();
    	    	  if (isDigit(currentChar)) { // d+
    	        	while (isDigit(currentChar)) {
    	        		takeIt();
    	        	}
    	        	return Token.FLOATLITERAL; // d+ . d+ e|E +|- d+: float case 4.
    	    	  }
    	    	  else {
    	    	  	while (!(currentChar == 'e' || currentChar == 'E')) { // delete until e. e becomes currentChar
          	  		storeBuffer();
    	    	  	}
    		    		return Token.FLOATLITERAL; // d+ . d+ (e|E +|-) ERROR
    	    	  }
    	    	}
    	    	else if (isDigit(currentChar)) { // d+
    	      	while (isDigit(currentChar)) {
    	      		takeIt();
    	      	}
    	      	return Token.FLOATLITERAL; // d+ . d+ e|E d+: float case 3.
    	    	}
    	    	else {
        	  	while (!(currentChar == 'e' || currentChar == 'E')) { // delete until e. e becomes currentChar
        	  		storeBuffer();
        	  	}
    	    		return Token.FLOATLITERAL; // d+ . d+ (e|E) ERROR
    	    	}
          }
      		else {
      			return Token.FLOATLITERAL; // d+ . d+: float case 11
      		}
      	}
      	else {
      		return Token.FLOATLITERAL; // d+ .: float case 5
      	}
      }
      else if (currentChar == 'e' || currentChar == 'E') { // e|E
      	takeIt();
	    	if (currentChar == '+' || currentChar == '-') { // +|-
	    	  takeIt();
	    	  if (isDigit(currentChar)) { // d+
	        	while (isDigit(currentChar)) {
	        		takeIt();
	        	}
	        	return Token.FLOATLITERAL; // d+ e|E +|- d+: float case 9.
	    	  }
	    	  else {
	    	  	while (!(currentChar == 'e' || currentChar == 'E')) { // delete until e. e becomes currentChar
      	  		storeBuffer();
	    	  	}
		    		return Token.INTLITERAL; // d+ (e|E +|-) ERROR
	    	  }
	    	}
	    	else if (isDigit(currentChar)) { // d+
	      	while (isDigit(currentChar)) {
	      		takeIt();
	      	}
	      	return Token.FLOATLITERAL; // d+ e|E d+: float case 8.
	    	}
	    	else {
    	  	while (!(currentChar == 'e' || currentChar == 'E')) { // delete until e. e becomes currentChar
    	  		storeBuffer();
    	  	}
	    		return Token.INTLITERAL; // d+ (e|E) ERROR
	    	}
      }
      return Token.INTLITERAL;
    case '.':
    	takeIt();
    	if (isDigit(currentChar)) { // d+
      	while (isDigit(currentChar)) {
      		takeIt();
      	}
    		if (currentChar == 'e' || currentChar == 'E') { // e|E
        	takeIt();
  	    	if (currentChar == '+' || currentChar == '-') { // +|-
  	    	  takeIt();
  	    	  if (isDigit(currentChar)) { // d+
  	        	while (isDigit(currentChar)) {
  	        		takeIt();
  	        	}
  	        	return Token.FLOATLITERAL; // . d+ e|E +|- d+: float case 2.
  	    	  }
  	    	  else {
  	    	  	while (!(currentChar == 'e' || currentChar == 'E')) { // delete until e. e becomes currentChar
        	  		storeBuffer();
  	    	  	}
  		    		return Token.FLOATLITERAL; // . d+ (e|E +|-) ERROR
  	    	  }
  	    	}
  	    	else if (isDigit(currentChar)) { // d+
  	      	while (isDigit(currentChar)) {
  	      		takeIt();
  	      	}
  	      	return Token.FLOATLITERAL; // . d+ e|E d+: float case 1.
  	    	}
  	    	else {
      	  	while (!(currentChar == 'e' || currentChar == 'E')) { // delete until e. e becomes currentChar
      	  		storeBuffer();
      	  	}
  	    		return Token.FLOATLITERAL; // . d+ (e|E) ERROR
  	    	}
        }
    		else {
    			return Token.FLOATLITERAL; // . d+: float case 10
    		}
    	}
    	else {
    		return Token.ERROR;
    	}
    case '+':
        takeIt();
        return Token.PLUS;
    case '-':
        takeIt();
        return Token.MINUS;
    case '*':
      takeIt();
      return Token.TIMES;
    case '/':
    boolean finishReady = false;
    takeIt();
      if (currentChar == '/'){ // single line comment
        while (currentChar != '\n'){
          takeIt();
        }
        return TOKEN_COMMENT;
      }
      else if (currentChar == '*'){ //multi line comment

          while (currentChar != '\u0000'){
            takeIt();
            if (currentChar == '*'){
              finishReady = true;
              continue;
            }
            if (finishReady && currentChar == '/'){
              takeIt();
              return TOKEN_COMMENT;
            }
            finishReady = false;
          }
          if (currentChar == '\u0000'){
            System.out.println("ERROR: unterminated multi-line comment.");
            return TOKEN_COMMENT;
          }
      }
      else
        return Token.DIV;
    case '\u0000': // sourceFile.eot:
      currentLexeme.append('$');
      currentColNr++;
      return Token.EOF;
    // Add code here for the remaining MiniC tokens...
    case 'A':  case 'B':  case 'C':  case 'D':  case 'E':
    case 'F':  case 'G':  case 'H':  case 'I':  case 'J':
    case 'K':  case 'L':  case 'M':  case 'N':  case 'O':
    case 'P':  case 'Q':  case 'R':  case 'S':  case 'T':
    case 'U':  case 'V':  case 'W':  case 'X':  case 'Y':
    case 'Z':
    case 'a':  case 'b':  case 'c':  case 'd':  case 'e':
    case 'f':  case 'g':  case 'h':  case 'i':  case 'j':
    case 'k':  case 'l':  case 'm':  case 'n':  case 'o':
    case 'p':  case 'q':  case 'r':  case 's':  case 't':
    case 'u':  case 'v':  case 'w':  case 'x':  case 'y':
    case 'z':
    case '_':
      takeIt();
      while (isDigit(currentChar) || isLetter(currentChar)) {
    	   takeIt();
      }
      if (currentLexeme.toString().equals("true") || currentLexeme.toString().equals("false"))
        return Token.BOOLLITERAL;
      return Token.ID;
    case '\"':
      takeIt();
      while (!(currentChar == '\"' || currentChar == '\n' || currentChar == '\u0000')){
        if (currentChar == '\\'){
          takeIt();
          if (currentChar != 'n'){ // if escape sequence except \n occurs, error
            System.out.println("ERROR: illegal escape sequence");
          }
        }
        takeIt();
      }
      if (currentChar == '\"') { // ""
        takeIt();
        currentLexeme.deleteCharAt(0);
        currentLexeme.deleteCharAt(currentLexeme.length()-1);
      }
      else if (currentChar == '\n'){
        System.out.println("ERROR: unterminated string literal");
        currentLexeme.deleteCharAt(0); // delete former "
      }
      else if (currentChar == '\u0000'){
        System.out.println("ERROR: unterminated string literal");
        currentLexeme.deleteCharAt(0); // delete former "
        currentLexeme.deleteCharAt(currentLexeme.length()-1);
      }
      return Token.STRINGLITERAL;
    case '=':
    	takeIt();
    	if (currentChar == '=') {
    		takeIt();
    		return Token.EQ;
    	}
    	return Token.ASSIGN;
    case '|':
    	takeIt();
    	if (currentChar == '|') {
    		takeIt();
    		return Token.OR;
    	}
    	else {
    		return Token.ERROR;
    	}
    case '!':
    	takeIt();
    	if (currentChar == '=') {
    		takeIt();
    		return Token.NOTEQ;
    	}
    	else {
    		return Token.NOT;
    	}
    case '<':
    	takeIt();
    	if (currentChar == '=') {
    		takeIt();
    		return Token.LESSEQ;
    	}
    	else {
    		return Token.LESS;
    	}
    case '>':
    	takeIt();
    	if (currentChar == '=') {
    		takeIt();
    		return Token.GREATEREQ;
    	}
    	else {
    		return Token.GREATER;
    	}
    case '&':
    	takeIt();
    	if (currentChar == '&') {
    		takeIt();
    		return Token.AND;
    	}
    	else {
    		return Token.ERROR;
    	}
    case '{':
    	takeIt();
    	return Token.LEFTBRACE;
    case '}':
    	takeIt();
    	return Token.RIGHTBRACE;
    case '[':
    	takeIt();
    	return Token.LEFTBRACKET;
    case ']':
    	takeIt();
    	return Token.RIGHTBRACKET;
    case '(':
    	takeIt();
    	return Token.LEFTPAREN;
    case ')':
    	takeIt();
    	return Token.RIGHTPAREN;
    case ',':
    	takeIt();
    	return Token.COMMA;
    case ';':
    	takeIt();
    	return Token.SEMICOLON;
    default:
      takeIt();
      return Token.ERROR;
    }
  }

  public Token scan () {
    Token currentToken;
    SourcePos pos;
    int kind = 0;

    currentlyScanningToken = false;
    while (currentChar == ' '
           || currentChar == '\f'
           || currentChar == '\n'
           || currentChar == '\r'
           || currentChar == '\t')
    {
      takeIt();
    }
    currentlyScanningToken = true;
    currentLexeme = new StringBuffer("");
    pos = new SourcePos();
    pos.StartLine = currentLineNr;
    pos.EndLine = currentLineNr;
    pos.StartCol = currentColNr + 1;
    kind = scanToken();
    while (kind == -1){
      currentlyScanningToken = false;
      while (currentChar == ' '
             || currentChar == '\f'
             || currentChar == '\n'
             || currentChar == '\r'
             || currentChar == '\t')
      {
        takeIt();
      }
      currentlyScanningToken = true;
      currentLexeme = new StringBuffer("");
      pos = new SourcePos();
      pos.StartLine = currentLineNr;
      pos.EndLine = currentLineNr;
      pos.StartCol = currentColNr + 1;
      kind = scanToken();
    }
    currentToken = new Token(kind, currentLexeme.toString(), pos);
    pos.EndCol = currentColNr;
    if (verbose)
      currentToken.print();
    return currentToken;
  }

}
