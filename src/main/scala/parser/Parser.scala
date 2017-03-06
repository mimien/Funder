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

  def vars: Parser[Vars] = positioned {
    val arrayParser = ARRAY() ~ LB() ~ expression ~ RB()
    val matrixParser = MATRIX() ~ LB() ~ expression ~ RB() ~ LB() ~ expression ~ RB()

    (VAR() | arrayParser | matrixParser) ~ identifier ~ COLON() ~ dataType ^^ {
      case VAR() ~ Id(id) ~ _ ~ typ => Variable(id, typ)
      case ARRAY() ~ _ ~ IntegerN(n) ~ _ ~ Id(id) ~ _ ~ typ => Array(id, typ, n)
      case MATRIX() ~ Id(id) ~ _ ~ typ => Variable(id, typ)
    }
  }

  def dataType: Parser[Type] = positioned {
    INT() ^^ (_ => IntType) |
      FLOAT() ^^ (_ => FloatType) |
      BOOL() ^^ (_ => BoolType) |
      STRING() ^^ (_ => StringType) |
      LINE() ^^ (_ => LineType) |
      ARC() ^^ (_ => ArcType) |
      OVAL() ^^ (_ => OvalType) |
      RECTANGLE() ^^ (_ => RectangleType)
  }

  def fun: Parser[Function] = positioned{
    FUN() ~> identifier ~ (LP() ~> rep1sep(vars, COMMA()) <~ RP()) ~ COLON() ~ dataType ~ block ^^ {
      case Id(id) ~ params ~ _ ~ typ ~ blck => Function(id, params, typ, blck)
    }
  }

  def block: Parser[Block] = positioned {
    INDENT() ~> rep(vars) ~ rep1(statement) ~ RETURN() ~ expression <~ INDENT() ^^ {
      case varss ~ statements ~ _ ~ expr => Block(varss, statements, expr)
    }
  }

  def statement: Parser[Statement] = positioned {
    assignment | ifThen | whileDo | functionCall
  }

  def assignment: Parser[Statement] = positioned {
    identifier ~ (LB() ~> expression <~ RB() ~ (LB() ~> expression <~ RB()).?).? ~ ASSIGN() ~ expression ^^ {
      case Id(id) ~ None ~ None ~ _ ~ expr => Assignment(id, expr)
      case Id(id) ~ Some(IntegerN(size)) ~ None ~ _ ~ expr => AssignArray(id, size, expr)
      case Id(id) ~ Some(IntegerN(rows)) ~ Some(IntegerN(cols)) ~ _ ~ expr => AssignMatrix(id, rows, cols, expr)
    }
  }
  def ifThen: Parser[Statement] = positioned {
    (IF() ~> expression <~ THEN()) ~ conditionBlock ~ (ELSE() ~> conditionBlock).? ^^ {
      case expr ~ cBlock ~ None => IfThen(expr, cBlock)
      case expr ~ cBlock ~ Some(cBlock2) => IfThenElse(expr, cBlock, cBlock2)
    }
  }

  def conditionBlock: Parser[ConditionBlock] = positioned {
    INDENT() ~> rep1(statement) <~ DEDENT() ^^ (ConditionBlock(_))
  }

  def whileDo: Parser[WhileDo] = positioned {
    (WHILE() ~> expression <~ DO()) ~ conditionBlock ^^ {
      case expr ~ cBlock => WhileDo(expr, cBlock)
    }
  }
  def functionCall: Parser[FunctionCall] = positioned {
    identifier ~ (LP() ~> rep1sep(expression, COMMA()) <~ RP()) ^^ {
      case Id(id) ~ expressions => FunctionCall(id, expressions)
    }
  }

  def expression: Parser[Expression] = positioned {
    comp ~ ((AND() | OR()) ~ comp).? ^^ {
      case compr ~ None => compr
      case comp1 ~ Some(AND() ~ comp2) => And(comp1, comp2)
      case comp1 ~ Some(OR() ~ comp2) => Or(comp1, comp2)
    }
  }

  def comp: Parser[Expression] = positioned {
    exp ~ ((GREATER_THAN() | GREATER_EQUALS() | LESS_THAN() | LESS_EQUALS() | NOT_EQUALS() | EQUALS()) ~ exp).? ^^ {
      case expr ~ None => expr
      case expr1 ~ Some(GREATER_THAN() ~ expr2) => GreaterThan(expr1, expr2)
      case expr1 ~ Some(GREATER_EQUALS() ~ expr2) => GreaterEquals(expr1, expr2)
      case expr1 ~ Some(LESS_THAN() ~ expr2) => LessThan(expr1, expr2)
      case expr1 ~ Some(LESS_EQUALS() ~ expr2) => LessEquals(expr1, expr2)
      case expr1 ~ Some(NOT_EQUALS() ~ expr2) => Unequals(expr1, expr2)
      case expr1 ~ Some(EQUALS() ~ expr2) => Equals(expr1, expr2)
    }
  }
  def exp: Parser[Expression] = positioned {
    term ~ rep((PLUS() | MINUS()) ~ term) ^^ {
      case firstTerm ~ ops => ops.reduce((t1, t2) => Sum(firstTerm, t1._2 match {
        case Sum(_, _) => Sum(t1._2, t2._2)
        case Subtract(_, _) => Subtract(t1._2, t2._2)
      }))
    }
  }
  def term: Parser[Expression] = positioned {
    factor ~ rep((DIVIDES() | TIMES() | MOD()) ~ factor) ^^ {
      case firstFactor ~ ops =>
        ops.reduce((t1, t2) => Multiply(firstFactor, t1._2 match {
          case DIVIDES() => Multiply(t1._2, t2._2)
          case TIMES() => Divide(t1._2, t2._2)
          case MOD() => Module(t1._2, t2._2)
        }))
    }
  }

  def factor: Parser[Expression] = positioned {
    expression | values | funCall
  }

  def values: Parser[Expression] = positioned {
    intValue |
      floatValue |
      stringValue |
      identifier |
      identifier ~ (LB() ~> expression <~ RB()) ^^ {
        case Id(id) ~ IntegerN(n) => IdArray(id, n)
      } | identifier ~ (LB() ~> expression <~ RB()) ~ (LB() ~> expression <~ RB()) ^^ {
        case Id(id) ~ IntegerN(rows) ~ IntegerN(cols) => IdMatrix(id, rows, cols)
      }
  }
  def funCall: Parser[Expression] = positioned {
    identifier ~ (LP() ~> rep1sep(expression, COMMA()) <~ RP()) ^^ {
      case Id(id) ~ expressions =>
        FunCall(id, expressions)
    }
  }

  def program: Parser[AST] = positioned {
    phrase(rep(vars) ~ rep(fun) ~ MAIN() ~ block) ^^ {
      case varss ~ funs ~ _ ~ blck => Program(varss, funs, blck)
    }
  }

  def apply(tokens: Seq[Token]): Either[ParserError, AST] = {
    val reader = new TokenReader(tokens)
    program(reader) match {
      case NoSuccess(msg, next) => Left(ParserError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
    }
  }
  private def identifier: Parser[Expression] = positioned {
    accept("identifier", { case IDENTIFIER(name) => Id(name) })
  }

  private def stringValue: Parser[Expression] = positioned {
    accept("string literal", { case lit@Str(_) => lit })
  }

  private def intValue: Parser[Expression] = positioned {
    accept("constant integer", { case VAL_INT(num) => IntegerN(num) })
  }

  private def floatValue: Parser[Expression] = positioned {
    accept("constant float", { case VAL_FLOAT(num) => FloatN(num) })
  }
}

class TokenReader(tokens: Seq[Token]) extends Reader[Token] {
  override def first: Token = tokens.head

  override def atEnd: Boolean = tokens.isEmpty

  override def pos: Position = tokens.headOption.map(_.pos).getOrElse(NoPosition)

  override def rest: Reader[Token] = new TokenReader(tokens.tail)
}