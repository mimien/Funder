package syntax

import compiler.{Location, ParserError}
import lexical._

import scala.collection.immutable.HashSet
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

/**
  * Parsers based on the syntax diagrams from Funder
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
      case ARRAY() ~ _ ~ IntN(n) ~ _ ~ Id(id) ~ _ ~ typ => Array(id, typ, n)
      case MATRIX() ~ Id(id) ~ _ ~ typ => Variable(id, typ)
    }
  }

  def dataType: Parser[Type] = positioned {
    INT()         ^^ (_ => IntType) |
      FLOAT()     ^^ (_ => FloatType) |
      BOOL()      ^^ (_ => BoolType) |
      STRING()    ^^ (_ => StringType) |
      LINE()      ^^ (_ => LineType) |
      ARC()       ^^ (_ => ArcType) |
      OVAL()      ^^ (_ => OvalType) |
      RECTANGLE() ^^ (_ => RectangleType)
  }

  def fun: Parser[Function] = positioned {
    FUN() ~> identifier ~ (LP() ~> repsep(vars, COMMA()) <~ RP()) ~ COLON() ~ dataType ~ block ^^ {
      case Id(id) ~ params ~ _ ~ typ ~ blck => Function(id, params, typ, blck)
    }
  }

  def block: Parser[Block] = positioned {
    INDENT() ~> rep(vars)  ~  rep1(statement) ~ RETURN() ~ expression <~ DEDENT() ^^ {
      case varss ~ statements ~ _ ~ expr => Block(varss, statements, expr)
    }
  }

  def statement: Parser[Statement] = positioned {
    assignment | ifThen | whileDo | functionCall
  }

  def assignment: Parser[Statement] = positioned {
    identifier ~ ((LB() ~> expression <~ RB()) ~ (LB() ~> expression <~ RB()).?).? ~ ASSIGN() ~ expression ^^ {
      case Id(id) ~ None ~ _ ~ expr => Assignment(id, expr)
      case Id(id) ~ Some(size ~ None) ~ _ ~ expr => AssignArray(id, size, expr)
      case Id(id) ~ Some(rows ~ Some(cols)) ~ _ ~ expr => AssignMatrix(id, rows, cols, expr)
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
    identifier ~ (LP() ~> repsep(expression, COMMA()) <~ RP()) ^^ {
      case Id(id) ~ expressions => FunctionCall(id, expressions)
    }
  }

  def expression: Parser[Expression] = positioned {
    comp ~ ((AND() | OR()) ~ comp).? ^^ {
      case compr ~ None => compr
      case comp1 ~ Some(AND() ~ comp2) => And(comp1, comp2)
      case comp1 ~ Some(OR() ~ comp2) => Or(comp1, comp2)
      case comp ~ Some(_) => comp
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
      case expr ~ Some(_) => expr
    }
  }

  // Magic
  def exp: Parser[Expression] = positioned(chainl1(term, PLUS() ^^^ Sum | MINUS() ^^^ Sub))

  // Magic
  def term: Parser[Expression] = positioned {
    chainl1(factor, TIMES() ^^^ Mul | DIVIDES() ^^^ Div | MOD() ^^^ Mod)
  }

  def factor: Parser[Expression] = positioned {
    LP() ~> expression <~ RP() | funCall | values
  }

  def values: Parser[Expression] = positioned {
    intValue |
      floatValue |
      stringValue |
      identifier ~ (LB() ~> intValue <~ RB()) ~ (LB() ~> intValue <~ RB()) ^^ {
        case Id(id) ~ rows ~ cols => IdMatrix(id, rows, cols)
      } |
      identifier ~ (LB() ~> intValue <~ RB()) ^^ {
        case Id(id) ~ expr => IdArray(id, expr)
      } | identifier
  }

  def funCall: Parser[Expression] = positioned {
    identifier ~ (LP() ~> repsep(expression, COMMA()) <~ RP()) ^^ {
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
      case Success(result, _) => Right(result)
    }
  }

  private def identifier: Parser[Id] = positioned {
    accept("identifier", { case IDENTIFIER(name) => Id(name) })
  }

  private def stringValue: Parser[Str] = positioned {
    accept("string literal", { case VAL_STRING(str) => Str(str) })
  }

  private def intValue: Parser[IntN] = positioned {
    accept("constant integer", { case VAL_INT(num) => IntN(num) })
  }

  private def floatValue: Parser[FloatN] = positioned {
    accept("constant float", { case VAL_FLOAT(num) => FloatN(num) })
  }
}

class TokenReader(tokens: Seq[Token]) extends Reader[Token] {
  override def first: Token = tokens.head

  override def atEnd: Boolean = tokens.isEmpty

  override def pos: Position = tokens.headOption.map(_.pos).getOrElse(NoPosition)

  override def rest: Reader[Token] = new TokenReader(tokens.tail)
}