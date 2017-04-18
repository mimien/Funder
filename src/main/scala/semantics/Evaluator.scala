package semantics

import lexical.Lexer
import semantics.Memory.{FunDirectory, VarTable}
import syntax._

import scala.collection.immutable.HashMap
import scala.collection.mutable
import scala.io.StdIn

/**
  * Class description
  *
  * @author emiliocornejo
  *         created on 10/03/17
  */
object Evaluator {

  val ADR = Memory.Addresses
  val MEM = Memory

  def apply(program: Program): FunDirectory = {
    MEM.quadruples.enqueue((-1, -1, -1, -1))

    // Add global variables table to the function directory
    MEM.functionDirectory("global") = Memory.Fun(IntType, Seq(), createVarTable(MEM.VarTable(), program.vars))

    // Add function variables tables to the function directory
    program.functions.foreach(f => addFunction(f.id, f.params, f.returnTyp, f.block))
    MEM.quadruples.update(0, (ADR.goto, MEM.quadruples.length + 1, -1, -1))

    // Add main variables table to the function directory
    addFunction("main", Seq(), IntType, program.main)

    // Print quadruples
    for (i <- MEM.quadruples.indices) {
      println(f"${i + 1}%2s${MEM.quadruples(i)}")
    }
    MEM.functionDirectory
  }

  def addQuads(funVarTable: VarTable, expr: Expression): (Int, Type) = {

    def arithmeticQuad(operation: Int, leftOp: (Int, Type), rightOp: (Int, Type)) = {
      (leftOp._2, rightOp._2) match {
        case (IntType, IntType) =>
          val result = ADR.addTempInt()
          MEM.quadruples.enqueue((operation, leftOp._1, rightOp._1, result))
          (result, IntType)
        case (IntType, FloatType) | (FloatType, IntType) | (FloatType, FloatType) =>
          val result = ADR.addTempFloat()
          MEM.quadruples.enqueue((operation, leftOp._1, rightOp._1, result))
          (result, FloatType)
        case (StringType, StringType) if operation == ADR.sum =>
          val result = ADR.addTempString()
          MEM.quadruples.enqueue((operation, leftOp._1, rightOp._1, result))
          (result, StringType)
        case _ => sys.error("Error: Arithmetic operation " + operation + " between " + leftOp._2 + " and " + rightOp._2
          + " not permitted")
      }
    }

    def relationalQuad(operation: Int, leftOp: (Int, Type), rightOp: (Int, Type)) = {
      (leftOp._2, rightOp._2) match {
        case (IntType, IntType) | (IntType, FloatType) | (FloatType, IntType) | (FloatType, FloatType) =>
          val result = ADR.addTempBool()
          MEM.quadruples.enqueue((operation, leftOp._1, rightOp._1, result))
          (result, BoolType)
        case (StringType, StringType) | (BoolType, BoolType) if operation == ADR.eq || operation == ADR.ne =>
          val result = ADR.addTempBool()
          MEM.quadruples.enqueue((operation, leftOp._1, rightOp._1, result))
          (result, BoolType)
        case _ => sys.error("Error: Relation operation " + operation + " between " + leftOp._2 + " and " + rightOp._2
          + " not permitted")
      }
    }

    def logicalQuad(operation: Int, leftOp: (Int, Type), rightOp: (Int, Type)) = {
      (leftOp._2, rightOp._2) match {
        case (BoolType, BoolType) =>
          val result = ADR.addTempBool()
          MEM.quadruples.enqueue((operation, leftOp._1, rightOp._1, result))
          (result, BoolType)
        case _ => sys.error("Error: Logical operation " + operation + " between " + leftOp._2 + " and " + rightOp._2
          + " not permitted")
      }
    }

    expr match {
      case IntN(num) => (ADR.addIntVal(num), IntType)
      case FloatN(num) => (ADR.addFltVal(num), FloatType)
      case Str(string) => (ADR.addStrVal(string), StringType)
      case Bool(boolean) => (ADR.addBoolVal(boolean), BoolType)
      case Id(name) =>
        val variable = funVarTable get name
        val globalVariable = MEM.functionDirectory("global").variables get name
        if (variable isDefined) (variable.get.address, variable.get.typ)
        else if (globalVariable isDefined) (globalVariable.get.address, globalVariable.get.typ)
        else sys.error(s"Error: The variable $name doesn't exist")

      case IdArray(name, index) => ??? // TODO
      case IdMatrix(name, rowIndex, colIndex) => ??? // TODO
      case FunCall(name, params) => ??? // TODO
      case ReadString() => (ADR.rdStr, StringType)
      case ReadInt() => (ADR.rdInt, IntType)
      case ReadFloat() => (ADR.rdFlt, FloatType)
      case Sum(e1, e2) => arithmeticQuad(ADR.sum, addQuads(funVarTable, e1), addQuads(funVarTable, e2))
      case Sub(e1, e2) => arithmeticQuad(ADR.sub, addQuads(funVarTable, e1), addQuads(funVarTable, e2))
      case Mul(e1, e2) => arithmeticQuad(ADR.mul, addQuads(funVarTable, e1), addQuads(funVarTable, e2))
      case Div(e1, e2) => arithmeticQuad(ADR.div, addQuads(funVarTable, e1), addQuads(funVarTable, e2))
      case Mod(e1, e2) => arithmeticQuad(ADR.mod, addQuads(funVarTable, e1), addQuads(funVarTable, e2))
      case Equals(e1, e2) => relationalQuad(ADR.eq, addQuads(funVarTable, e1), addQuads(funVarTable, e2))
      case NotEquals(e1, e2) => relationalQuad(ADR.ne, addQuads(funVarTable, e1), addQuads(funVarTable, e2))
      case GreaterThan(e1, e2) => relationalQuad(ADR.gt, addQuads(funVarTable, e1), addQuads(funVarTable, e2))
      case GreaterEquals(e1, e2) => relationalQuad(ADR.ge, addQuads(funVarTable, e1), addQuads(funVarTable, e2))
      case LessThan(e1, e2) => relationalQuad(ADR.lt, addQuads(funVarTable, e1), addQuads(funVarTable, e2))
      case LessEquals(e1, e2) => relationalQuad(ADR.le, addQuads(funVarTable, e1), addQuads(funVarTable, e2))
      case And(e1, e2) => logicalQuad(ADR.and, addQuads(funVarTable, e1), addQuads(funVarTable, e2))
      case Or(e1, e2) => logicalQuad(ADR.or, addQuads(funVarTable, e1), addQuads(funVarTable, e2))

      case _ => sys.error("Error: BAD EXPRESSION. DON'T EXPRESS LIKE THAT.")
    }
  }

  def processStatements(funName: String, statements: Seq[Statement]): Unit = {

    val varTable = MEM.functionDirectory(funName).variables

    def assignVar(name: String, expression: Expression, i: Int = 0, j: Int = 0) = {
      val variable = varTable get name
      val globalVariable = MEM.functionDirectory("global").variables get name
      if (variable isDefined) {
        val evaluatedExpr = addQuads(varTable, expression)
        if (evaluatedExpr._2 == variable.get.typ) {
          MEM.quadruples.enqueue((ADR.asgmt, evaluatedExpr._1, -1, variable.get.address))
        }
        else sys.error("Error: Expression assignnment of variable " + name + " doesn't match expected type")
      }
      else if (globalVariable isDefined) {
        val evaluatedExpr = addQuads(varTable, expression)
        if (evaluatedExpr._2 == globalVariable.get.typ) {
          MEM.quadruples.enqueue((ADR.asgmt, evaluatedExpr._1, -1, globalVariable.get.address))
        }
        else sys.error("Error: Expression assignnment of variable " + name + "doesn't match expected type")
      }
      else sys.error("Error: Variable " + name + " doesn't exist in this scope")
    }

    statements.foreach {
      case Assignment(name, expr) => assignVar(name, expr)
      case AssignArray(name, IntN(index), expr) => assignVar(name, expr, index)
      case AssignMatrix(name, IntN(row), IntN(col), expr) => assignVar(name, expr, row, col)

      case IfThen(expr, block) =>
        val evaluatedExpr = addQuads(varTable, expr)
        if (evaluatedExpr._2 == BoolType) {
          val gotofLine = MEM.quadruples.length
          MEM.quadruples.enqueue((-1, -1, -1, -1))
          processStatements(funName, block.statements)
          MEM.quadruples.update(gotofLine, (ADR.gotof, evaluatedExpr._1, -1, MEM.quadruples.length + 1))
        }
        else sys.error("ERROR: If condition must contain a boolean expression")
      case IfThenElse(expr, blockOne, blockTwo) =>
        val evaluatedExpr = addQuads(varTable, expr)
        if (evaluatedExpr._2 == BoolType) {
          val gotofLine = MEM.quadruples.length
          MEM.quadruples.enqueue((-1, -1, -1, -1))
          processStatements(funName, blockOne.statements)
          MEM.quadruples.update(gotofLine, (ADR.gotof, evaluatedExpr._1, -1, MEM.quadruples.length + 2))
          val gotoLine = MEM.quadruples.length
          MEM.quadruples.enqueue((-1, -1, -1, -1))
          processStatements(funName, blockTwo.statements)
          MEM.quadruples.update(gotoLine, (ADR.gotof, -1, -1, MEM.quadruples.length + 1))
        }
        else sys.error("ERROR: If condition must contain a boolean expression")
      case WhileDo(expr, block) =>
        val evaluatedExpr = addQuads(varTable, expr)
        if (evaluatedExpr._2 == BoolType) {
          val gotofLine = MEM.quadruples.length
          MEM.quadruples.enqueue((-1, -1, -1, -1))
          processStatements(funName, block.statements)
          MEM.quadruples.enqueue((ADR.goto, -1, -1, gotofLine + 1))
          MEM.quadruples.update(gotofLine, (ADR.gotof, evaluatedExpr._1, -1, MEM.quadruples.length + 1))
        }
        else sys.error("ERROR: While condition must contain a boolean expression")
      case FunctionCall(name, params) =>
        if (MEM.functionDirectory contains name) {
          val paramsTypes = MEM.functionDirectory(name).paramsTypes
          if (params.length == paramsTypes.length) {
            MEM.quadruples.enqueue((ADR.era, -1 /*name*/, -1, -1))
            for (i <- params.indices) {
              val evaluatedExpr = addQuads(varTable, params(i))
              if (evaluatedExpr._2 == paramsTypes(i)) {
                MEM.quadruples.enqueue((ADR.param, evaluatedExpr._1, -1, ADR.param))
              }
              else sys.error("ERROR: Type mismatch on function call " + name)
            }
            MEM.quadruples.enqueue((ADR.gosub, -1 /*name*/, -1, -1))
          }
          else sys.error("ERROR: Incorrect number of parameters in function call " + name)
        }
        else sys.error("ERROR: Function " + name + " is not defined")
      case Write(expr) =>
        val evaluatedExpr = addQuads(varTable, expr)
        MEM.quadruples.enqueue((ADR.write, evaluatedExpr._1, -1, -1))

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
    block match {
      case Block(vars, statements, retrn) =>
        if (MEM.functionDirectory contains name) sys.error(s"Error: Function $name already defined")
        else {
          val funParamsTypes = params.map(_.getType)
          val funVarTable = createVarTable(createVarTable(MEM.VarTable(), params), vars)
          MEM.functionDirectory(name) = MEM.Fun(typ, funParamsTypes, funVarTable, statements, MEM.quadruples.length + 1)

          // make quadruples
          processStatements(name, statements)

          // make return quadruple
          val returnExpr = addQuads(funVarTable, retrn)
          if (returnExpr._2 == typ) MEM.quadruples.enqueue((ADR.retrn, returnExpr._1, -1, -1))
          else sys.error("Error: return expression of type " + returnExpr._2 + " doesn't match function type")
        }
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
      vars.head match {
        // TODO Change contains name for isDefined implementation
        case Variable(name, typ) =>
          if (table contains name) sys.error(s"Error: Variable $name already defined")
          else createVarTable(table + (name -> MEM.Var(typ, 0, 0)), vars.tail)
        case Array(name, typ, size) =>
          if (table contains name) sys.error(s"Error: Variable $name already defined")
          else createVarTable(table + (name -> MEM.Var(typ, size, 0)), vars.tail)
        case Matrix(name, typ, row, col) =>
          if (table contains name) sys.error(s"Error: Variable $name already defined")
          else createVarTable(table + (name -> MEM.Var(typ, row, col)), vars.tail)
      }
    }
  }
}