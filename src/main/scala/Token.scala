case class Token(
    tokenType: TokenType,
    lexeme: String,
    literal: Option[Any],
    line: Int
):
  override def toString = s"$tokenType $lexeme $literal"
