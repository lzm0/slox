import Scanner._
import TokenType.*

import scala.collection.mutable.ListBuffer

class Scanner(source: String):
  private val tokens = ListBuffer.empty[Token]
  private var start = 0
  private var current = 0
  private var line = 1

  def scanTokens(): List[Token] =
    while (!isAtEnd) {
      start = current
      scanToken()
    }
    tokens += Token(EOF, "", null, line)
    tokens.toList

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
        if next('/') then while peek != '\n' && !isAtEnd do advance()
        else addToken(SLASH)

      case ' ' | '\r' | '\t' => ()
      case '\n'              => line += 1

      case '"' => string()

      case _ if isDigit(c) => number()
      case _ if isAlpha(c) => identifier()
      case _               => Lox.error(line, "Unexpected character.")

  private def identifier(): Unit =
    while isAlphaNumeric(peek) do advance()
    addToken(keywords.getOrElse(source.substring(start, current), IDENTIFIER))
  private def number(): Unit =
    while isDigit(peek) do advance()
    if (peek == '.') && isDigit(peekNext) then
      advance()
      while isDigit(peek) do advance()
    addToken(NUMBER, source.substring(start, current).toDouble)

  private def string(): Unit =
    while (peek != '"') && !isAtEnd do
      if (peek == '\n') line += 1
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

  private def peek: Char =
    if isAtEnd then '\u0000'
    else source(current)

  private def peekNext: Char =
    if (current + 1 >= source.length) '\u0000'
    else source.charAt(current + 1)

  private def advance(): Char =
    val previous = source(current)
    current += 1
    previous

  private def addToken(tokenType: TokenType, literal: Any = null): Unit =
    val text = source.substring(start, current)
    tokens += Token(tokenType, text, Some(literal), line)

object Scanner:
  private def isAlphaNumeric(c: Char) = isAlpha(c) || isDigit(c)

  private def isDigit(c: Char) = c >= '0' && c <= '9'

  private def isAlpha(c: Char) =
    (c >= 'a' && c <= 'z') ||
      (c >= 'A' && c <= 'Z') ||
      c == '_';

  private val keywords = Map(
    "and" -> AND,
    "class" -> CLASS,
    "else" -> ELSE,
    "false" -> FALSE,
    "for" -> FOR,
    "fun" -> FUN,
    "if" -> IF,
    "nil" -> NIL,
    "or" -> OR,
    "print" -> PRINT,
    "return" -> RETURN,
    "super" -> SUPER,
    "this" -> THIS,
    "true" -> TRUE,
    "var" -> VAR,
    "while" -> WHILE
  )
