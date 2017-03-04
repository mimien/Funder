package lexer

import scala.util.parsing.input.Positional

/**
  * Class description
  *
  * @author emiliocornejo
  *         created on 03/03/17
  */
sealed trait Token extends Positional {

  // KEYWORDS
  case class IDENTIFIER(str: String) extends Token

  case class INT(str: Int) extends Token

  case class FLOAT(str: Float) extends Token

  case class STRING(str: String) extends Token

  case class VAR() extends Token

  case class ARRAY() extends Token

  case class MATRIX() extends Token

  case class FUN() extends Token

  case class IF() extends Token

  case class ELSE() extends Token

  case class AND() extends Token

  case class OR() extends Token
}