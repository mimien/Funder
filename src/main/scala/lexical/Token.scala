package lexical

import scala.util.parsing.input.Positional

/**
  * Class description
  *
  * @author emiliocornejo
  *         created on 03/03/17
  */
sealed trait Token extends Positional

// VALUES
case class IDENTIFIER(str: String) extends Token

case class VAL_INT(str: Int) extends Token

case class VAL_FLOAT(str: Float) extends Token

case class VAL_STRING(str: String) extends Token

case class VAL_BOOL(str: Boolean) extends Token

case class INDENTATION(spaces: Int) extends Token

// TYPES
case class INT() extends Token

case class FLOAT() extends Token

case class BOOL() extends Token

case class STRING() extends Token

// OPERATORS
case class PLUS() extends Token

case class MINUS() extends Token

case class TIMES() extends Token

case class DIVIDES() extends Token

case class MOD() extends Token

case class ASSIGN() extends Token

case class EQUALS() extends Token

case class NOT_EQUALS() extends Token

case class GREATER_THAN() extends Token

case class LESS_THAN() extends Token

case class GREATER_EQUALS() extends Token

case class LESS_EQUALS() extends Token

case class LP() extends Token // Left Parenthesis

case class RP() extends Token // Right Parenthesis

case class LB() extends Token // Left Bracket

case class RB() extends Token // Right Bracket

case class COMMA() extends Token

case class COLON() extends Token

case class INDENT() extends Token

case class DEDENT() extends Token

// KEYWORDS
case class VAR() extends Token

case class ARRAY() extends Token

case class MATRIX() extends Token

case class FUN() extends Token

case class IF() extends Token

case class THEN() extends Token

case class WHILE() extends Token

case class DO() extends Token

case class ELSE() extends Token

case class AND() extends Token

case class OR() extends Token

case class BLACK() extends Token

case class DARK_GRAY() extends Token

case class LIGHT_GRAY() extends Token

case class BLUE() extends Token

case class GREEN() extends Token

case class YELLOW() extends Token

case class RED() extends Token

case class ORANGE() extends Token

case class MAIN() extends Token
case class READ_STRING() extends Token

case class READ_INT() extends Token

case class READ_FLOAT() extends Token

case class WRITE() extends Token

case class DRAW_RECTANGLE() extends Token

case class DRAW_OVAL() extends Token

case class DRAW_ARC() extends Token

case class DRAW_LINE() extends Token

case class RETURN() extends Token

case class END() extends Token