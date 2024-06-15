import TokenType._
class Scanner(source: String):
  private var tokens = List.empty[Token]
  private var start = 0
  private var current = 0
  private var line = 1

  def scanTokens(): List[Token] =
    while (!isAtEnd) {
      start = current
      scanToken()
    }
    tokens :+ Token(EOF, "", null, line)

  private def isAtEnd: Boolean = current >= source.length

  private def scanToken(): Unit =
    val c = advance()
    c match
      case '(' => addToken(LEFT_PAREN)
      case ')' => addToken(RIGHT_PAREN)
      case '{' => addToken(LEFT_BRACE)
      case '}' => addToken(RIGHT_BRACE)
      case ',' => addToken(COMMA)
      case '.' => addToken(DOT)
      case '-' => addToken(MINUS)
      case '+' => addToken(PLUS)
      case ';' => addToken(SEMICOLON)
      case '*' => addToken(STAR)
      case _ =>
        Lox.error(line, "Unexpected character.")

  private def advance(): Char =
    val previous = source(current)
    current += 1
    previous

  private def addToken(tokenType: TokenType, literal: AnyRef = None): Unit =
    val text = source.substring(start, current)
    tokens :+ Token(tokenType, text, Some(literal), line)
