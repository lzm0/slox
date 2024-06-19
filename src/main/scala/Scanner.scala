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
      case '!' => addToken(if next('=') then BANG_EQUAL else BANG)
      case '=' => addToken(if next('=') then EQUAL_EQUAL else EQUAL)
      case '<' => addToken(if next('=') then LESS_EQUAL else LESS)
      case '>' => addToken(if next('=') then GREATER_EQUAL else GREATER)
      case '/' =>
        if next('/') then while peek() != '\n' && !isAtEnd do advance()
        else addToken(SLASH)

      case ' ' | '\r' | '\t' => ()
      case '\n'              => line += 1

      case '"' => string()

      case _ => Lox.error(line, "Unexpected character.")

  private def string(): Unit =
    while (peek() != '"') && !isAtEnd do
      if (peek() eq '\n') line += 1
      advance()
    if (isAtEnd)
      Lox.error(line, "Unterminated string.")
      return

    advance()

    val value = source.substring(start + 1, current - 1)
    addToken(STRING, value)

  private def next(expected: Char): Boolean =
    if isAtEnd then return false
    if source(current) != expected then return false
    current += 1
    true

  private def peek(): Char =
    if isAtEnd then '\u0000'
    else source(current)

  private def advance(): Char =
    val previous = source(current)
    current += 1
    previous

  private def addToken(tokenType: TokenType, literal: AnyRef = None): Unit =
    val text = source.substring(start, current)
    tokens :+ Token(tokenType, text, Some(literal), line)
