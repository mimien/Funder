package parser

import scala.util.parsing.input.Positional

/**
  * Class description
  *
  * @author emiliocornejo
  *         created on 04/03/17
  */
sealed trait AST extends Positional
case class Program(vars: Seq[Vars], functions: Seq[Function], main: Block) extends AST
case class Function(id: String, params: Seq[Variable], returnTyp: Type, block: Block) extends AST
case class Block(vars: Seq[Vars], statements: Seq[Statement]) extends AST
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
case class Assignment(variable: VarName, expr: Expression) extends Statement
case class IfThen(expr: Expression, block: ConditionBlock) extends Statement
case class IfThenElse(expr: Expression, block1: ConditionBlock, block2: ConditionBlock) extends Statement
case class WhileDo(expr: Expression, block: ConditionBlock) extends Statement

sealed trait VarName extends Positional
case class Var(name: String) extends VarName
case class Arr(name: String, size: Int) extends VarName
case class Mat(name: String, numOfRows: Int, numOfColumns: Int) extends VarName

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
case class Subtract(term1: Expression, term2: Expression) extends Expression
// Level 4 <TERM>
case class Multiply(factor1: Expression, factor2: Expression) extends Expression
case class Divide(factor1: Expression, factor2: Expression) extends Expression
// Level 5 <FACTOR>
case class Negative(constant: Expression) extends Expression
case class Positive(constant: Expression) extends Expression
// Level 6 <VALUES>
case class IntegerN(num: Int) extends Expression
case class FloatN(num: Float) extends Expression
case class Str(num: String) extends Expression
case class Bool(num: Boolean) extends Expression
case class Id(name: String) extends Expression
case class IdArray(name: String, row: Int) extends Expression
case class IdMatrix(name: String, row: Int, column: Int) extends Expression
// Level 7 <FUNCTION_CALL>
case class FunctionCall(id: String, params: Seq[Expression]) extends AST
