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

  case class Var(typ: Type, row: Int, column: Int, value: Option[Value])
  case class Fun(typ: Type, varsTable: VarsTable, returnVal: Option[Expression], statements: Seq[Statement] = Seq()) // TODO if the return value is optional maybe you should modify the diagrams

  type VarsTable = mutable.HashMap[String, Var]
  type FunTable = mutable.HashMap[String, Fun]

  val functionDir: FunTable = mutable.HashMap[String, Fun]()


  def defaultVarsTable: VarsTable = mutable.HashMap[String, Var]()

  def apply(program: Program): FunTable = {
    functionDir("global") = Fun(IntType, processVariables(defaultVarsTable, program.vars), None)
    processFunction("main", Seq(), IntType, program.main, functionDir)
    program.functions.foreach(f => processFunction(f.id, f.params, f.returnTyp, f.block, functionDir))
    functionDir
  }

  def evaluateExpression(expr: Expression): Value = {
    // TODO create error and stop program when not compatible operations
    expr match {
      case v: Value => v
      case Read() => evaluateExpression(Str(StdIn.readLine()))
      // INT, INT
      case Sum(IntN(e1), IntN(e2)) => evaluateExpression(IntN(e1 + e2))
      case Sub(IntN(e1), IntN(e2)) => evaluateExpression(IntN(e1 - e2))
      case Mul(IntN(e1), IntN(e2)) => evaluateExpression(IntN(e1 * e2))
      case Div(IntN(e1), IntN(e2)) => evaluateExpression(IntN(e1 / e2))
      case Equals(IntN(e1), IntN(e2)) => evaluateExpression(Bool(e1 == e2))
      case Unequals(IntN(e1), IntN(e2)) => evaluateExpression(Bool(e1 != e2))
      case GreaterThan(IntN(e1), IntN(e2)) => evaluateExpression(Bool(e1 > e2))
      case GreaterEquals(IntN(e1), IntN(e2)) => evaluateExpression(Bool(e1 >= e2))
      case LessThan(IntN(e1), IntN(e2)) => evaluateExpression(Bool(e1 < e2))
      case LessEquals(IntN(e1), IntN(e2)) => evaluateExpression(Bool(e1 <= e2))
      // INT, FLOAT
      case Sum(IntN(e1), FloatN(e2)) => evaluateExpression(FloatN(e1 + e2))
      case Sub(IntN(e1), FloatN(e2)) => evaluateExpression(FloatN(e1 - e2))
      case Mul(IntN(e1), FloatN(e2)) => evaluateExpression(FloatN(e1 * e2))
      case Div(IntN(e1), FloatN(e2)) => evaluateExpression(FloatN(e1 / e2))
      case Equals(IntN(e1), FloatN(e2)) => evaluateExpression(Bool(e1 == e2))
      case Unequals(IntN(e1), FloatN(e2)) => evaluateExpression(Bool(e1 != e2))
      case GreaterThan(IntN(e1), FloatN(e2)) => evaluateExpression(Bool(e1 > e2))
      case GreaterEquals(IntN(e1), FloatN(e2)) => evaluateExpression(Bool(e1 >= e2))
      case LessThan(IntN(e1), FloatN(e2)) => evaluateExpression(Bool(e1 < e2))
      case LessEquals(IntN(e1), FloatN(e2)) => evaluateExpression(Bool(e1 <= e2))
      // FLOAT, INT
      case Sum(FloatN(e1), IntN(e2)) => evaluateExpression(FloatN(e1 + e2))
      case Sub(FloatN(e1), IntN(e2)) => evaluateExpression(FloatN(e1 - e2))
      case Mul(FloatN(e1), IntN(e2)) => evaluateExpression(FloatN(e1 * e2))
      case Div(FloatN(e1), IntN(e2)) => evaluateExpression(FloatN(e1 / e2))
      case Equals(FloatN(e1), IntN(e2)) => evaluateExpression(Bool(e1 == e2))
      case Unequals(FloatN(e1), IntN(e2)) => evaluateExpression(Bool(e1 != e2))
      case GreaterThan(FloatN(e1), IntN(e2)) => evaluateExpression(Bool(e1 > e2))
      case GreaterEquals(FloatN(e1), IntN(e2)) => evaluateExpression(Bool(e1 >= e2))
      case LessThan(FloatN(e1), IntN(e2)) => evaluateExpression(Bool(e1 < e2))
      case LessEquals(FloatN(e1), IntN(e2)) => evaluateExpression(Bool(e1 <= e2))
      // STRING, STRING
      case Sum(Str(s1), Str(s2)) => evaluateExpression(Str(s1 + s2))
      case Equals(Str(s1), Str(s2)) => evaluateExpression(Bool(s1 == s2))
      case Unequals(Str(s1), Str(s2)) => evaluateExpression(Bool(s1 != s2))
      // BOOL, BOOL
      case Equals(Bool(b1), Bool(b2)) => evaluateExpression(Bool(b1 == b2))
      case Unequals(Bool(b1), Bool(b2)) => evaluateExpression(Bool(b1 != b2))
      case And(Bool(b1), Bool(b2)) => evaluateExpression(Bool(b1 && b2))
      case Or(Bool(b1), Bool(b2)) => evaluateExpression(Bool(b1 || b2))

      case _ => IntN(999999999)
    }
  }

  def processStatements(varTable: VarsTable, statements: Seq[Statement]): Unit = {

    def assignVar(name: String, expression: Expression, i: Int = 0, j: Int = 0) = {
      if (varTable contains name) {
        varTable(name) = Var(varTable(name).typ, i, j, Some(evaluateExpression(expression)))
      } else if (functionDir(name).varsTable contains name) {
        functionDir("global").varsTable(name) = Var(varTable(name).typ, i, j, Some(evaluateExpression(expression)))
      } else println("No existe la variable")
    }

    statements.foreach {
      case Assignment(name, expr) => assignVar(name, expr)
      case AssignArray(name, IntN(index), expr) => assignVar(name, expr, index)
      case AssignMatrix(name, IntN(row), IntN(col), expr) => assignVar(name, expr, row, col)
      //      case IfThen(expr, block) => evaluateExpression(expr) match {
      //        case Bool(true) =>
      //        case Bool(false) =>
      //        case _ =>
      //      }
      //      case FunctionCall(id, params) => id match {
      //        case "write" =>
      //      }
      //    }
      case _ => IntN(-999999999)
    }
  }

  /**
    * Add function to the mutable function directory
    *
    * @param name   name of the function
    * @param params parameters of the function
    * @param typ    return type of the function
    * @param block  block of the function
    * @param funDir function directory
    */
  def processFunction(name: String, params: Seq[Vars], typ: Type, block: Block, funDir: FunTable) {
    val varsTable = processVariables(defaultVarsTable, params)
    block match {
      case Block(vars, statements, retrn) =>
        // TODO create error and stop program when names overlap
        if (funDir contains name) println(s"La funcion $name ya existe")
        else funDir(name) = Fun(typ, processVariables(varsTable, vars), Some(retrn), statements)
        processStatements(funDir(name).varsTable, statements)
    }
  }

  /**
    * Adds variables to the variable table
    *
    * @param table The variable table which belongs a function scope
    * @param vars  variables of type Variable which belongs a function scope
    * @return The variable table with the variables added
    */
  def processVariables(table: VarsTable, vars: Seq[Vars]): VarsTable = {
    if (vars.isEmpty) table
    else {
      val head = vars.head
      head match {
        case Variable(name, typ) =>
          if (table contains name) println(s"La variable $name ya existe ")
          else table(name) = Var(typ, 0, 0, None)
        case Array(name, typ, size) =>
          if (table contains name) println(s"La variable $name ya existe")
          else table(name) = Var(typ, size, 0, None)
        case Matrix(name, typ, row, col) =>
          if (table contains name) println(s"La variable $name ya existe")
          else table(name) = Var(typ, row, col, None)
      }
      processVariables(table, vars.tail)
    }
  }
}