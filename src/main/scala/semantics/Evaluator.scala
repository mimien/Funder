package semantics

import lexical.Lexer
import syntax._

import scala.collection.mutable
import scala.io.StdIn

/**
  * Class description
  *
  * @author emiliocornejo
  *         created on 10/03/17
  */
object Evaluator {

  type VarTable = mutable.HashMap[String, Var]
  type FunTable = mutable.HashMap[String, Fun]

  val functionDirectory: FunTable = mutable.HashMap[String, Fun]()
  val quadruples: mutable.Queue[Quad] = mutable.Queue[Quad]()
  var tempIdNum = 0

  def newVarTable: VarTable = mutable.HashMap[String, Var]()

  def apply(program: Program): FunTable = {
    quadruples.enqueue(Quad("", "", "", ""))

    // Add global variables table to the function directory
    functionDirectory("global") = Fun(IntType, createVarTable(newVarTable, program.vars))

    // Add function variables tables to the function directory
    program.functions.foreach(f => addFunction(f.id, f.params, f.returnTyp, f.block))
    quadruples.update(0, Quad("goto", (quadruples.length + 1).toString, "", ""))

    // Add main variables table to the function directory
    addFunction("main", Seq(), IntType, program.main)
    for (i <- quadruples.indices) {
      println(f"${i + 1}%2s${quadruples(i)}")
    }
    functionDirectory
  }

  def addQuads(funVarTable: VarTable, expr: Expression): (String, Type) = {

    def arithmeticQuad(operation: String, leftOp: (String, Type), rightOp: (String, Type), result: String)
    : (String, Type) = {
      quadruples.enqueue(Quad(operation, leftOp._1, rightOp._1, result))
      tempIdNum += 1

      (leftOp._2, rightOp._2) match {
        case (IntType, IntType) => (result, IntType)
        case (IntType, FloatType) => (result, FloatType)
        case (FloatType, IntType) => (result, FloatType)
        case (FloatType, FloatType) => (result, FloatType)
        case (StringType, StringType) if operation == "+" => (result, StringType)
        case _ => sys.error("Error: Arithmetic operation " + operation + " between " + leftOp._2 + " and " + rightOp._2
          + " not permitted")
      }
    }

    def relationalQuad(operation: String, leftOp: (String, Type), rightOp: (String, Type), result: String)
    : (String, Type) = {
      quadruples.enqueue(Quad(operation, leftOp._1, rightOp._1, result))
      tempIdNum += 1
      (leftOp._2, rightOp._2) match {
        case (IntType, IntType) => (result, BoolType)
        case (IntType, FloatType) => (result, BoolType)
        case (FloatType, IntType) => (result, BoolType)
        case (FloatType, FloatType) => (result, BoolType)
        case (StringType, StringType) if operation == "==" || operation == "<>" => (result, BoolType)
        case (BoolType, BoolType) if operation == "==" || operation == "<>" => (result, BoolType)
        case _ => sys.error("Error: Relation operation " + operation + " between " + leftOp._2 + " and " + rightOp._2
          + " not permitted")
      }
    }

    def logicalQuad(operation: String, leftOp: (String, Type), rightOp: (String, Type), result: String)
    : (String, Type) = {
      quadruples.enqueue(Quad(operation, leftOp._1, rightOp._1, result))
      tempIdNum += 1
      (leftOp._2, rightOp._2) match {
        case (BoolType, BoolType) => (result, BoolType)
        case _ => sys.error("Error: Logical operation " + operation + " between " + leftOp._2 + " and " + rightOp._2
          + " not permitted")
      }
    }

    expr match {
      case IntN(num) => (num.toString, IntType)
      case FloatN(num) => (num.toString, FloatType)
      case Str(string) => (string, StringType)
      case Bool(boolean) => (boolean.toString, BoolType)
      case Id(name) =>
        val variable = funVarTable get name
        val globalVariable = functionDirectory("global").varTable get name
        if (variable isDefined) (name, variable.get.typ)
        else if (globalVariable isDefined) (name, globalVariable.get.typ)
        else sys.error(s"Error: The variable $name doesn't exist")

      case IdArray(name, index) => ??? // TODO
      case IdMatrix(name, rowIndex, colIndex) => ??? // TODO
      case FunCall(name, params) => ??? // TODO
      case ReadString() => ("readStr", StringType)
      case ReadInt() => ("readInt", IntType)
      case ReadFloat() => ("readFloat", FloatType)
      case Sum(e1, e2) => arithmeticQuad("+", addQuads(funVarTable, e1), addQuads(funVarTable, e2), "t" + tempIdNum)
      case Sub(e1, e2) => arithmeticQuad("-", addQuads(funVarTable, e1), addQuads(funVarTable, e2), "t" + tempIdNum)
      case Mul(e1, e2) => arithmeticQuad("*", addQuads(funVarTable, e1), addQuads(funVarTable, e2), "t" + tempIdNum)
      case Div(e1, e2) => arithmeticQuad("/", addQuads(funVarTable, e1), addQuads(funVarTable, e2), "t" + tempIdNum)
      case Mod(e1, e2) => arithmeticQuad("%", addQuads(funVarTable, e1), addQuads(funVarTable, e2), "t" + tempIdNum)
      case Equals(e1, e2) => relationalQuad("==", addQuads(funVarTable, e1), addQuads(funVarTable, e2), "t" + tempIdNum)
      case Unequals(e1, e2) => relationalQuad("<>", addQuads(funVarTable, e1), addQuads(funVarTable, e2), "t" + tempIdNum)
      case GreaterThan(e1, e2) => relationalQuad("<", addQuads(funVarTable, e1), addQuads(funVarTable, e2), "t" + tempIdNum)
      case GreaterEquals(e1, e2) => relationalQuad("<=", addQuads(funVarTable, e1), addQuads(funVarTable, e2), "t" + tempIdNum)
      case LessThan(e1, e2) => relationalQuad(">", addQuads(funVarTable, e1), addQuads(funVarTable, e2), "t" + tempIdNum)
      case LessEquals(e1, e2) => relationalQuad(">=", addQuads(funVarTable, e1), addQuads(funVarTable, e2), "t" + tempIdNum)
      case And(e1, e2) => logicalQuad("and", addQuads(funVarTable, e1), addQuads(funVarTable, e2), "t" + tempIdNum)
      case Or(e1, e2) => logicalQuad("or", addQuads(funVarTable, e1), addQuads(funVarTable, e2), "t" + tempIdNum)

      case _ => sys.error("Error: BAD EXPRESSION. DON'T EXPRESS LIKE THAT.")
    }
  }

  def processStatements(varTable: VarTable, statements: Seq[Statement]): Unit = {

    tempIdNum = 1

    def assignVar(name: String, expression: Expression, i: Int = 0, j: Int = 0) = {
      val variable = varTable get name
      val globalVariable = functionDirectory("global").varTable get name
      if (variable isDefined) {
        val evaluatedExpr = addQuads(varTable, expression)
        if (evaluatedExpr._2 == variable.get.typ) quadruples.enqueue(Quad("=", evaluatedExpr._1, "", name))
        else sys.error("Error: Expression assignnment of variable " + name + " doesn't match expected type")
      }
      else if (globalVariable isDefined) {
        val evaluatedExpr = addQuads(varTable, expression)
        if (evaluatedExpr._2 == globalVariable.get.typ) quadruples.enqueue(Quad("=", evaluatedExpr._1, "", name))
        else sys.error("Error: Expression assignnment of variable " + name + "doesn't match expected type")
      }
      else sys.error("Error: Variable " + name + "doesn't exist in this scope")
    }

    statements.foreach {
      case Assignment(name, expr) => assignVar(name, expr)
      case AssignArray(name, IntN(index), expr) => assignVar(name, expr, index)
      case AssignMatrix(name, IntN(row), IntN(col), expr) => assignVar(name, expr, row, col)
      case FunctionCall(name, params) =>
        val function = functionDirectory(name)

      case IfThen(expr, block) =>
        val evaluatedExpr = addQuads(varTable, expr)
        if (evaluatedExpr._2 == BoolType) {
          val gotofLine = quadruples.length
          quadruples.enqueue(Quad("", "", "", ""))
          processStatements(varTable, block.statements)
          quadruples.update(gotofLine, Quad("gotof", evaluatedExpr._1, "", (quadruples.length + 1).toString))
        }
        else sys.error("ERROR: If condition must contain a boolean expression")
      case IfThenElse(expr, blockOne, blockTwo) =>
        val evaluatedExpr = addQuads(varTable, expr)
        if (evaluatedExpr._2 == BoolType) {
          val gotofLine = quadruples.length
          quadruples.enqueue(Quad("", "", "", ""))
          processStatements(varTable, blockOne.statements)
          quadruples.update(gotofLine, Quad("gotof", evaluatedExpr._1, "", (quadruples.length + 2).toString))
          val gotoLine = quadruples.length
          quadruples.enqueue(Quad("", "", "", ""))
          processStatements(varTable, blockTwo.statements)
          quadruples.update(gotoLine, Quad("goto", "", "", (quadruples.length + 1).toString))
        }
        else sys.error("ERROR: If condition must contain a boolean expression")
      case WhileDo(expr, block) =>
        val evaluatedExpr = addQuads(varTable, expr)
        if (evaluatedExpr._2 == BoolType) {
          val gotofLine = quadruples.length
          quadruples.enqueue(Quad("", "", "", ""))
          processStatements(varTable, block.statements)
          quadruples.enqueue(Quad("goto", "", "", (gotofLine + 1).toString))
          quadruples.update(gotofLine, Quad("gotof", evaluatedExpr._1, "", (quadruples.length + 1).toString))
        }
        else sys.error("ERROR: While condition must contain a boolean expression")

      //case FunctionCall(id, params) =>
      case Write(expr) =>
        val evaluatedExpr = addQuads(varTable, expr)
        quadruples.enqueue(Quad("write", evaluatedExpr._1, "", ""))

      case _ => sys.error("ERROR STATEMENT")
    }
  }

  /**
    * Link function data to the mutable function directory and process block statements for making quadruples
    *
    * @param  name    name of the function
    * @param  params  parameters of the function
    * @param  typ     return type of the function
    * @param  block   block of the function
    */
  def addFunction(name: String, params: Seq[Vars], typ: Type, block: Block) {
    val varTable = createVarTable(newVarTable, params)
    block match {
      case Block(vars, statements, retrn) =>
        if (functionDirectory contains name) sys.error(s"Error: Function $name already defined")
        else {
          functionDirectory(name) = Fun(typ, createVarTable(varTable, vars), statements)
          processStatements(functionDirectory(name).varTable, statements)
          val retrnExpr = addQuads(varTable, retrn)
          if (retrnExpr._2 == typ) quadruples.enqueue(Quad("RETURN", retrnExpr._1, "", ""))
          else sys.error("Error: return expression of type " + retrnExpr._2 + " doesn't match function type")        }
    }
  }

  /**
    * Adds variables to the variable table
    *
    * @param  table   The variable table that belongs a function scope
    * @param  vars    variables of type Variable which belongs a function scope
    * @return   The variable table with the variables added
    */
  private def createVarTable(table: VarTable, vars: Seq[Vars]): VarTable = {
    if (vars.isEmpty) table
    else {
      val head = vars.head
      head match {
        case Variable(name, typ) =>
          if (table contains name) sys.error(s"Error: Variable $name already defined")
          else table(name) = Var(typ, 0, 0)
        case Array(name, typ, size) =>
          if (table contains name) sys.error(s"Error: Variable $name already defined")
          else table(name) = Var(typ, size, 0)
        case Matrix(name, typ, row, col) =>
          if (table contains name) sys.error(s"Error: Variable $name already defined")
          else table(name) = Var(typ, row, col)
      }
      createVarTable(table, vars.tail)
    }
  }

  case class Var(typ: Type, row: Int, column: Int)

  case class Expr(expression: Expression, typ: Type)

  case class Quad(operator: String, left: String, right: String, result: String) {
    override def toString: String = f"|$operator%8s|$left%8s|$right%8s|$result%8s|"
  }

  case class Fun(typ: Type, varTable: VarTable, statements: Seq[Statement] = Seq())
}