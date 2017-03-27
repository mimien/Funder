package syntax

import scala.collection.immutable.HashSet
import scala.util.parsing.input.Positional

/**
  * Class description
  *
  * @author emiliocornejo
  *         created on 04/03/17
  */
sealed trait AST extends Positional

case class Program(vars: Seq[Vars], functions: Seq[Function], main: Block) extends AST

case class Function(id: String, params: Seq[Vars], returnTyp: Type, block: Block) extends AST

case class Block(vars: Seq[Vars], statements: Seq[Statement], retrn: Expression) extends AST

case class ConditionBlock(statements: Seq[Statement]) extends AST


sealed trait Vars extends Positional

case class Variable(name: String, typ: Type) extends Vars

case class Array(name: String, typ: Type, size: Int) extends Vars

case class Matrix(name: String, typ: Type, numOfRows: Int, numOfColumns: Int) extends Vars


sealed trait Type extends Positional

case object IntType extends Type

case object FloatType extends Type

case object BoolType extends Type

case object StringType extends Type

case object LineType extends Type

case object ArcType extends Type

case object OvalType extends Type

case object RectangleType extends Type


sealed trait Statement extends Positional

case class Assignment(name: String, expr: Expression) extends Statement

case class AssignArray(name: String, size: Expression, expr: Expression) extends Statement

case class AssignMatrix(name: String, numOfRows: Expression, numOfColumns: Expression, expr: Expression) extends Statement

case class IfThen(expr: Expression, block: ConditionBlock) extends Statement

case class IfThenElse(expr: Expression, block1: ConditionBlock, block2: ConditionBlock) extends Statement

case class WhileDo(expr: Expression, block: ConditionBlock) extends Statement

case class FunctionCall(id: String, params: Seq[Expression]) extends Statement

sealed trait Expression extends Positional

// Level 1 <EXPRESSION>
case class And(comparison1: Expression, comparison2: Expression) extends Expression

case class Or(comparison1: Expression, comparison2: Expression) extends Expression

// Level 2 <COMP>
case class Equals(expr1: Expression, expr2: Expression) extends Expression

case class Unequals(expr1: Expression, expr2: Expression) extends Expression

case class GreaterThan(expr1: Expression, expr2: Expression) extends Expression

case class LessThan(expr1: Expression, expr2: Expression) extends Expression

case class GreaterEquals(expr1: Expression, expr2: Expression) extends Expression

case class LessEquals(expr1: Expression, expr2: Expression) extends Expression

// Level 3 <EXPR>
case class Sum(term1: Expression, term2: Expression) extends Expression

case class Sub(term1: Expression, term2: Expression) extends Expression

// Level 4 <TERM>
case class Mul(factor1: Expression, factor2: Expression) extends Expression

case class Div(factor1: Expression, factor2: Expression) extends Expression

case class Mod(factor1: Expression, factor2: Expression) extends Expression

// Level 5 <FACTOR>
// TODO modify the diagram adding read to the expression values
case class Read() extends Expression

trait Value

trait Identifier

case class IntN(num: Int) extends Expression with Value

case class FloatN(num: Float) extends Expression with Value

case class Str(num: String) extends Expression with Value

case class Bool(bool: Boolean) extends Expression with Value

case class Id(name: String) extends Expression with Identifier

case class IdArray(name: String, row: Expression) extends Expression with Identifier

case class IdMatrix(name: String, row: Expression, column: Expression) extends Expression with Identifier

case class FunCall(id: String, params: Seq[Expression]) extends Expression with Identifier