case class Token(
    tokenType: TokenType,
    lexeme: String,
    literal: Option[AnyRef],
    line: Int
):
  override def toString = s"$tokenType $lexeme $literal"
