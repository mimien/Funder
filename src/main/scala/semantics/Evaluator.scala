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

  type VarsTable = mutable.HashMap[String, Var]
  type FunTable = mutable.HashMap[String, Fun]
  val functionDir: FunTable = mutable.HashMap[String, Fun]()
  val quadruples: mutable.Queue[Quad] = mutable.Queue[Quad]()

  def defaultVarsTable: VarsTable = mutable.HashMap[String, Var]()

  def apply(program: Program): FunTable = {
    // Add global variables table to the function directory
    functionDir("global") = Fun(IntType, processVariables(defaultVarsTable, program.vars), None)

    // Add main variables table to the function directory
    processFunction("main", Seq(), IntType, program.main, functionDir)

    // Add function variables tables to the function directory
    program.functions.foreach(f => processFunction(f.id, f.params, f.returnTyp, f.block, functionDir))
    functionDir
  }

  def genQuad(expr: Expression): Value = {
    def eval(v1: Value, v2: Value, op: (Int, Int) => Int) = {
      (v1, v2) match {
        case (IntN(n1), IntN(n2)) => IntN(op(n1, n2))
        case (IntN(n1), FloatN(n2)) => FloatN(op(n1, n2))
        case (FloatN(n1), IntN(n2)) => FloatN(op(n1, n2))
        case (FloatN(n1), FloatN(n2)) => FloatN(op(n1, n2))
        case (FloatN(n1), FloatN(n2)) => FloatN(op(n1, n2))
        case _ => sys.error("Error: Arithmetic operation must be between Ints and/or floats")
      }
    }

    def evalRel(v1: Value, v2: Value, op: (Value, Value) => Boolean) = {
    }

    expr match {
      case v: Value => v
      case Read() => genQuad(Str(StdIn.readLine()))
      case Sum(e1, e2) => eval(genQuad(e1), genQuad(e2), _ + _)
      case Sub(e1, e2) => eval(genQuad(e1), genQuad(e2), _ - _)
      case Mul(e1, e2) => eval(genQuad(e1), genQuad(e2), _ * _)
      case Div(e1, e2) => eval(genQuad(e1), genQuad(e2), _ / _)
      case Mul(e1, e2) => eval(genQuad(e1), genQuad(e2), _ % _)
      case Equals(e1, e2) => genQuad(Bool(e1 == e2))
      case Unequals(IntN(e1), IntN(e2)) => genQuad(Bool(e1 != e2))
      case GreaterThan(IntN(e1), IntN(e2)) => genQuad(Bool(e1 > e2))
      case GreaterEquals(IntN(e1), IntN(e2)) => genQuad(Bool(e1 >= e2))
      case LessThan(IntN(e1), IntN(e2)) => genQuad(Bool(e1 < e2))
      case LessEquals(IntN(e1), IntN(e2)) => genQuad(Bool(e1 <= e2))
      // STRING, STRING
      case Sum(Str(s1), Str(s2)) => genQuad(Str(s1 + s2))
      case Equals(Str(s1), Str(s2)) => genQuad(Bool(s1 == s2))
      case Unequals(Str(s1), Str(s2)) => genQuad(Bool(s1 != s2))
      // BOOL, BOOL
      case Equals(Bool(b1), Bool(b2)) => genQuad(Bool(b1 == b2))
      case Unequals(Bool(b1), Bool(b2)) => genQuad(Bool(b1 != b2))
      case And(Bool(b1), Bool(b2)) => genQuad(Bool(b1 && b2))
      case Or(Bool(b1), Bool(b2)) => genQuad(Bool(b1 || b2))

      case _ => IntN(999999999)
    }
  }

  def processStatements(varTable: VarsTable, statements: Seq[Statement]): Unit = {

    def assignVar(name: String, expression: Expression, i: Int = 0, j: Int = 0) = {
      if (varTable contains name) {
        varTable(name) = Var(varTable(name).typ, i, j, Some(genQuad(expression)))
      } else if (functionDir(name).varsTable contains name) {
        functionDir("global").varsTable(name) = Var(varTable(name).typ, i, j, Some(genQuad(expression)))
      } else sys.error("Error: No existe la variable " + name)
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
        if (funDir contains name) sys.error(s"Error: La funcion $name ya existe")
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
          if (table contains name) sys.error(s"La variable $name ya existe")
          else table(name) = Var(typ, 0, 0, None)
        case Array(name, typ, size) =>
          if (table contains name) sys.error(s"La variable $name ya existe")
          else table(name) = Var(typ, size, 0, None)
        case Matrix(name, typ, row, col) =>
          if (table contains name) sys.error(s"La variable $name ya existe")
          else table(name) = Var(typ, row, col, None)
      }
      processVariables(table, vars.tail)
    }
  }

  case class Var(typ: Type, row: Int, column: Int, value: Option[Value])

  case class Quad(operator: String, row: Int, column: Int, result: String)

  case class Fun(typ: Type, varsTable: VarsTable, returnVal: Option[Expression], statements: Seq[Statement] = Seq()) // TODO if the return value is optional maybe you should modify the diagrams
}