package parser

import compiler.{Location, ParserError}
import lexer._

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

/**
  * Class description
  *
  * @author emiliocornejo
  *         created on 04/03/17
  */
object Parser extends Parsers {
  override type Elem = Token

  def vars: Parser[Any] =  {
    val arrayParser = ARRAY() ~ LB() ~ expression ~ RB()
    val matrixParser = MATRIX() ~ LB() ~ expression ~ RB() ~ LB() ~ expression ~ RB()

    (VAR() | arrayParser | matrixParser) ~ identifier ~ COLON() ~ dataType
  }

  def dataType: Parser[Any] =  {
    INT() |
      FLOAT() |
      BOOL() |
      STRING() |
      LINE() |
      ARC() |
      OVAL() |
      RECTANGLE()
  }

  def fun: Parser[Any] =  {
    FUN() ~> identifier ~ (LP() ~> rep1sep(vars, COMMA()) <~ RP()) ~ COLON() ~ dataType ~ block
  }

  def block: Parser[Any] =  {
    INDENT() ~> rep(vars) ~ rep1(statement) ~ RETURN() ~ expression <~ INDENT()
  }

  def statement: Parser[Any] =  {
    assignment | ifThen | whileDo | functionCall
  }

  def assignment: Parser[Any] =  {
    identifier ~ ((LB() ~> expression <~ RB()) ~ (LB() ~> expression <~ RB()).?).? ~ ASSIGN() ~ expression
  }

  def ifThen: Parser[Any] =  {
    (IF() ~> expression <~ THEN()) ~ conditionBlock ~ (ELSE() ~> conditionBlock).?
  }

  def conditionBlock: Parser[Any] =  {
    INDENT() ~> rep1(statement) <~ DEDENT()
  }

  def whileDo: Parser[Any] =  {
    (WHILE() ~> expression <~ DO()) ~ conditionBlock
  }

  def functionCall: Parser[Any] =  {
    identifier ~ (LP() ~> rep1sep(expression, COMMA()) <~ RP())
  }

  def expression: Parser[Any] =  {
    comp ~ ((AND() | OR()) ~ comp).?
  }

  def comp: Parser[Any] =  {
    exp ~ ((GREATER_THAN() | GREATER_EQUALS() | LESS_THAN() | LESS_EQUALS() | NOT_EQUALS() | EQUALS()) ~ exp).?
  }

  def exp: Parser[Any] =  {
    term ~ rep((PLUS() | MINUS()) ~ term)
  }

  def term: Parser[Any] =  {
    factor ~ rep((DIVIDES() | TIMES() | MOD()) ~ factor)
  }

  def factor: Parser[Any] = {
    LP() ~> expression <~ RP() | values | funCall
  }

  def values: Parser[Any] =  {
    intValue |
      floatValue |
      stringValue |
      identifier ~ (LB() ~> expression <~ RB()) ~ (LB() ~> expression <~ RB()) |
      identifier ~ (LB() ~> expression <~ RB()) |
      identifier
  }

  def funCall: Parser[Any] =  {
    identifier ~ (LP() ~> rep1sep(expression, COMMA()) <~ RP())
  }

  def program: Parser[Any] =  {
    phrase(rep(vars) ~ rep(fun) ~ MAIN() ~ block)
  }

  def apply(tokens: Seq[Token]): Either[ParserError, Any] = {
    val reader = new TokenReader(tokens)
    program(reader) match {
      case NoSuccess(msg, next) => Left(ParserError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
    }
  }

  private def identifier: Parser[IDENTIFIER] =  {
    accept("identifier", { case id@IDENTIFIER(name) => id })
  }

  private def stringValue: Parser[VAL_STRING] =  {
    accept("string literal", { case str@VAL_STRING(_) => str })
  }

  private def intValue: Parser[VAL_INT] =  {
    accept("constant integer", { case num@VAL_INT(_) => num })
  }

  private def floatValue: Parser[VAL_FLOAT] =  {
    accept("constant float", { case num@VAL_FLOAT(_) => num })
  }
}

class TokenReader(tokens: Seq[Token]) extends Reader[Token] {
  override def first: Token = tokens.head

  override def atEnd: Boolean = tokens.isEmpty

  override def pos: Position = tokens.headOption.map(_.pos).getOrElse(NoPosition)

  override def rest: Reader[Token] = new TokenReader(tokens.tail)
}