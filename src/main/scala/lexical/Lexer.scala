package lexical

import compiler.{Location, LexerError}

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

/**
  * Class description
  *
  * @author emiliocornejo
  *         created on 04/03/17
  */
object Lexer extends RegexParsers {

  override val whiteSpace: Regex = "[ \t\r\f]+".r

  override def skipWhitespace = true

  /**
    * Set the regular expression for the id token
    *
    * The regex returned by the function r is implicitly converted to a Parser[String] when
    * applying the ^^ function that maps the conversion of string to the token IDENTIFIER
    *
    * @return a parser that recognizes the identifier token
    */
  def identifier: Parser[IDENTIFIER] = positioned {
    "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => IDENTIFIER(str) }
  }

  def valString: Parser[VAL_STRING] = positioned {
    """"[^"]*"""".r ^^ { str =>
      val content = str.substring(1, str.length - 1) // removes the " at the sides
      VAL_STRING(content)
    }
  }

  def valInt: Parser[VAL_INT] = positioned {
    "-?[0-9]+".r ^^ { str =>
      val int = str.toInt
      VAL_INT(int)
    }
  }

  def valFloat: Parser[VAL_FLOAT] = positioned {
    "-?[0-9]+.[0-9]+".r ^^ { str =>
      val float = str.toFloat
      VAL_FLOAT(float)
    }
  }

  def indentation: Parser[INDENTATION] = positioned {
    "\n[ ]*".r ^^ { whitespace =>
      val nSpaces = whitespace.length - 1
      INDENTATION(nSpaces)
    }
  }

  def int: Parser[INT] = positioned { "Int" ^^ (_ => INT()) }

  def float: Parser[FLOAT] = positioned { "Float" ^^ (_ => FLOAT()) }

  def bool: Parser[BOOL] = positioned { "Bool" ^^ (_ => BOOL()) }

  def string: Parser[STRING] = positioned { "String" ^^ (_ => STRING()) }

  def line: Parser[LINE] = positioned { "Line" ^^ (_ => LINE()) }

  def arc: Parser[ARC] = positioned { "Arc" ^^ (_ => ARC()) }

  def oval: Parser[OVAL] = positioned { "Oval" ^^ (_ => OVAL()) }

  def rectangle: Parser[RECTANGLE] = positioned { "Rectangle" ^^ (_ => RECTANGLE()) }

  def plus: Parser[PLUS] = positioned { "+" ^^ (_ => PLUS()) }

  def minus: Parser[MINUS] = positioned { "-" ^^ (_ => MINUS()) }

  def times: Parser[TIMES] = positioned { "*" ^^ (_ => TIMES()) }

  def divides: Parser[DIVIDES] = positioned { "/" ^^ (_ => DIVIDES()) }

  def mod: Parser[MOD] = positioned { "%" ^^ (_ => MOD()) }

  def assign: Parser[ASSIGN] = positioned { "=" ^^ (_ => ASSIGN()) }

  def equals: Parser[EQUALS] = positioned { "==" ^^ (_ => EQUALS()) }

  def notEquals: Parser[NOT_EQUALS] = positioned { "<>" ^^ (_ => NOT_EQUALS()) }

  def greaterThan: Parser[GREATER_THAN] = positioned { ">" ^^ (_ => GREATER_THAN()) }

  def lessThan: Parser[LESS_THAN] = positioned { "<" ^^ (_ => LESS_THAN()) }

  def greaterEquals: Parser[GREATER_EQUALS] = positioned { ">=" ^^ (_ => GREATER_EQUALS()) }

  def lessEquals: Parser[LESS_EQUALS] = positioned { "<=" ^^ (_ => LESS_EQUALS()) }

  def leftParent: Parser[LP] = positioned { "(" ^^ (_ => LP()) }


  def rightParent: Parser[RP] = positioned { ")" ^^ (_ => RP()) }

  def leftBracket: Parser[LB] = positioned { "[" ^^ (_ => LB()) }

  def rightBracket: Parser[RB] = positioned { "]" ^^ (_ => RB()) }

  def comma: Parser[COMMA] = positioned { "," ^^ (_ => COMMA()) }

  def colon: Parser[COLON] = positioned { ":" ^^ (_ => COLON()) }

  def variable: Parser[VAR] = positioned { "var" ^^ (_ => VAR()) }

  def array: Parser[ARRAY] = positioned { "array" ^^ (_ => ARRAY()) }

  def matrix: Parser[MATRIX] = positioned { "matrix" ^^ (_ => MATRIX()) }

  def function: Parser[FUN] = positioned { "fun" ^^ (_ => FUN()) }

  def ifCondition: Parser[IF] = positioned { "if" ^^ (_ => IF()) }

  def then: Parser[THEN] = positioned { "then" ^^ (_ => THEN()) }

  def whileLoop: Parser[WHILE] = positioned { "while" ^^ (_ => WHILE()) }

  def doLoop: Parser[DO] = positioned { "do" ^^ (_ => DO()) }

  def elseCondition: Parser[ELSE] = positioned { "else" ^^ (_ => ELSE()) }

  def and: Parser[AND] = positioned { "and" ^^ (_ => AND()) }

  def or: Parser[OR] = positioned { "or" ^^ (_ => OR()) }

  def black: Parser[BLACK] = positioned { "black" ^^ (_ => BLACK()) }

  def darkGray: Parser[DARK_GRAY] = positioned { "darkGray" ^^ (_ => DARK_GRAY()) }

  def lightGray: Parser[LIGHT_GRAY] = positioned { "lightGray" ^^ (_ => LIGHT_GRAY()) }

  def blue: Parser[BLUE] = positioned { "blue" ^^ (_ => BLUE()) }

  def green: Parser[GREEN] = positioned { "green" ^^ (_ => GREEN()) }

  def yellow: Parser[YELLOW] = positioned { "yellow" ^^ (_ => YELLOW()) }

  def red: Parser[RED] = positioned { "red" ^^ (_ => RED()) }

  def orange: Parser[ORANGE] = positioned { "orange" ^^ (_ => ORANGE()) }

  def main: Parser[MAIN] = positioned { "main" ^^ (_ => MAIN()) }

  def createOval: Parser[CREATE_OVAL] = positioned { "createOval" ^^ (_ => CREATE_OVAL()) }

  def createRectangle: Parser[CREATE_RECTANGLE] = positioned { "createRectangle" ^^ (_ => CREATE_RECTANGLE()) }

  def createArc: Parser[CREATE_ARC] = positioned { "createArc" ^^ (_ => CREATE_ARC()) }

  def createLine: Parser[CREATE_LINE] = positioned { "createLine" ^^ (_ => CREATE_LINE()) }

  def moveX: Parser[MOVE_X] = positioned { "moveX" ^^ (_ => MOVE_X()) }

  def moveY: Parser[MOVE_Y] = positioned { "moveY" ^^ (_ => MOVE_Y()) }

  def setStroke: Parser[SET_STROKE] = positioned { "setStroke" ^^ (_ => SET_STROKE()) }

  def scale: Parser[SCALE] = positioned { "scale" ^^ (_ => SCALE()) }

  def setColor: Parser[SET_COLOR] = positioned { "setColor" ^^ (_ => SET_COLOR()) }

  def read: Parser[READ] = positioned { "read" ^^ (_ => READ()) }

  def write: Parser[WRITE] = positioned { "write" ^^ (_ => WRITE()) }

  def draw: Parser[DRAW] = positioned { "draw" ^^ (_ => DRAW()) }

  def retrn: Parser[RETURN] = positioned { "return" ^^ (_ => RETURN()) }

  def tokens: Parser[List[Token]] = {
    phrase(rep1(int | float | bool | string | line | arc | oval | rectangle | plus | minus | times | divides
      | mod | equals | notEquals | assign | greaterEquals | lessEquals | greaterThan | lessThan | leftParent
      | rightParent | leftBracket | rightBracket | comma | colon | variable | array | matrix | function
      | ifCondition | then | whileLoop | doLoop | elseCondition | and | or | black | darkGray | lightGray
      | blue | green | yellow | red | orange | main | createOval | createRectangle | createArc | createLine
      | moveX | moveY | setStroke | scale | setColor | read | write | draw | retrn | valString | valInt
      | valFloat | indentation | identifier)) ^^ { rawTokens =>
      processIndentations(rawTokens)
    }
  }

  def apply(code: String): Either[LexerError, List[Token]] = {
    parse(tokens, code) match {
      case NoSuccess(msg, next) => Left(LexerError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
    }
  }
  private def processIndentations(tokens: List[Token], indents: List[Int] = List(0)): List[Token] = {
    tokens.headOption match {
      // if there is an increase in indentation level, we push this new level into the stack
      // and produce an INDENT
      case Some(INDENTATION(spaces)) if spaces > indents.head =>
        INDENT() :: processIndentations(tokens.tail, spaces :: indents)

      // if there is a decrease, we pop from the stack until we have matched the new level,
      // producing a DEDENT for each pop
      case Some(INDENTATION(spaces)) if spaces < indents.head =>
        val (dropped, kept) = indents.partition(_ > spaces)
        (dropped map (_ => DEDENT())) ::: processIndentations(tokens.tail, kept)

      // if the indentation level stays unchanged, no tokens are produced
      case Some(INDENTATION(spaces)) if spaces == indents.head =>
        processIndentations(tokens.tail, indents)

      // other tokens are ignored
      case Some(token) => token :: processIndentations(tokens.tail, indents)

      // the final step is to produce a DEDENT for each indentation level still remaining, thus
      // "closing" the remaining open INDENTS
      case None => indents.filter(_ > 0).map(_ => DEDENT())
    }
  }
}
