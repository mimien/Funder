package semantics

import semantics.Memory.{EvalExpr, VarTable}
import syntax._

import scala.language.postfixOps

/**
  *
  *         created on 10/03/17
  */
object Evaluator {

  val ADR = Memory.Addresses
  val MEM = Memory

  def apply(program: Program): String = {
    MEM.addQuadruple(-1, -1, -1, -1)

    // Add global variables table to the function directory
    MEM.functionDirectory("global") = Memory.Fun(IntType, Seq(), createVarTable(MEM.VarTable(), program.globalVars))

    // Add function variables tables to the function directory
    program.functions.foreach(f => addFunction(f.id, f.params, f.returnTyp, f.block))
    MEM.updateQuadruple(0, ADR.goto, -1, -1, MEM.numberOfQuadruples)

    // Add main variables table to the function directory
    val mainBlock = Block(program.mainVars, program.mainStatements, IntN(0))
    addFunction("main", Seq(), IntType, mainBlock, isNotMainBlock = false)

    println(MEM.functionDirectory)
    ADR.get + MEM.quadruplesToString
  }

  def addExprQuads(funVarTable: VarTable, expr: Expression): EvalExpr = {

    def elemOfIndex(arr: MEM.Var, index: Expression) = {
      val evalIndxExpr = addExprQuads(funVarTable, index)

      if (evalIndxExpr.typ == IntType) {
        MEM.addQuadruple(ADR.ver, evalIndxExpr.address, -1, arr.rows)
        val tempPos = ADR.addTempInt()
        MEM.addQuadruple(ADR.sum, evalIndxExpr.address, ADR.addIntVal(arr.address), tempPos)
        val arrAdrVal = addTemp(arr.typ)
        MEM.addQuadruple(ADR.adr, tempPos, typeNum(arr.typ), arrAdrVal)
        arrAdrVal
      }
      else sys.error("Error: Index must be integer type")
    }

    // translate type to number for the vm type recognition
    def typeNum(typ: Type) = {
      typ match {
        case IntType => 0
        case FloatType => 1
        case StringType => 2
        case BoolType => 3
        case _ => -1
      }
    }
    // translate type to number for the vm type recognition
    def addTemp(typ: Type) = {
      typ match {
        case IntType => ADR.addTempInt()
        case FloatType => ADR.addTempFloat()
        case StringType => ADR.addTempString()
        case BoolType => ADR.addTempBool()
        case _ => -1
      }
    }

    def elemOfIndices(arr: MEM.Var, row: Expression, col: Expression) = {
      val evalRowIndxExpr = addExprQuads(funVarTable, row)
      val evalColIndxExpr = addExprQuads(funVarTable, col)

      if (evalRowIndxExpr.typ == IntType && evalColIndxExpr.typ == IntType) {
        MEM.addQuadruple(ADR.ver, evalRowIndxExpr.address, -1, arr.rows)
        val tempRowPos = ADR.addTempInt()
        MEM.addQuadruple(ADR.mul, evalRowIndxExpr.address, ADR.addIntVal(arr.columns), tempRowPos)
        val tempPos = ADR.addTempInt()
        MEM.addQuadruple(ADR.sum, tempRowPos, evalColIndxExpr.address, tempPos)
        val matAdr = ADR.addTempInt()
        MEM.addQuadruple(ADR.sum, tempPos, ADR.addIntVal(arr.address), matAdr)
        val matAdrVal = addTemp(arr.typ)
        MEM.addQuadruple(ADR.adr, matAdr, typeNum(arr.typ), matAdrVal)
        matAdrVal
      }
      else sys.error("Error: Indices must be integer type")
    }

    def addArithmeticQuad(operation: Int, leftOp: EvalExpr, rightOp: EvalExpr) = {
      (leftOp.typ, rightOp.typ) match {
        case (IntType, IntType) =>
          val result = ADR.addTempInt()
          MEM.addQuadruple(operation, leftOp.address, rightOp.address, result)
          EvalExpr(result, IntType)
        case (IntType, FloatType) | (FloatType, IntType) | (FloatType, FloatType) =>
          val result = ADR.addTempFloat()
          MEM.addQuadruple(operation, leftOp.address, rightOp.address, result)
          EvalExpr(result, FloatType)
        case (StringType, StringType) if operation == ADR.sum =>
          val result = ADR.addTempString()
          MEM.addQuadruple(operation, leftOp.address, rightOp.address, result)
          EvalExpr(result, StringType)
        case _ => sys.error("Error: Arithmetic operation " + operation + " between " + leftOp.typ + " and " +
          rightOp.typ + " not permitted")
      }
    }

    def addRelationalQuad(operation: Int, leftOp: EvalExpr, rightOp: EvalExpr) = {
      (leftOp.typ, rightOp.typ) match {
        case (IntType, IntType) | (IntType, FloatType) | (FloatType, IntType) | (FloatType, FloatType) =>
          val result = ADR.addTempBool()
          MEM.addQuadruple(operation, leftOp.address, rightOp.address, result)
          EvalExpr(result, BoolType)
        case (StringType, StringType) | (BoolType, BoolType) if operation == ADR.eq || operation == ADR.ne =>
          val result = ADR.addTempBool()
          MEM.addQuadruple(operation, leftOp.address, rightOp.address, result)
          EvalExpr(result, BoolType)
        case _ => sys.error("Error: Relation operation " + operation + " between " + leftOp.typ + " and " + rightOp.typ
          + " not permitted")
      }
    }

    def addLogicalQuad(operation: Int, leftOp: EvalExpr, rightOp: EvalExpr) = {
      (leftOp.typ, rightOp.typ) match {
        case (BoolType, BoolType) =>
          val result = ADR.addTempBool()
          MEM.addQuadruple(operation, leftOp.address, rightOp.address, result)
          EvalExpr(result, BoolType)
        case _ => sys.error("Error: Logical operation " + operation + " between " + leftOp.typ + " and " + rightOp.typ
          + " not permitted")
      }
    }

    expr match {
      case IntN(num) => EvalExpr(ADR.addIntVal(num), IntType)
      case FloatN(num) => EvalExpr(ADR.addFltVal(num), FloatType)
      case Str(string) => EvalExpr(ADR.addStrVal(string), StringType)
      case Bool(boolean) => EvalExpr(ADR.addBoolVal(boolean), BoolType)
      case Id(name) =>
        val variable = funVarTable get name
        val globalVariable = MEM.functionDirectory("global").variables get name
        if (variable isDefined) EvalExpr(variable.get.address, variable.get.typ)
        else if (globalVariable isDefined) EvalExpr(globalVariable.get.address, globalVariable.get.typ)
        else sys.error(s"Error: The variable $name doesn't exist")

      case IdArray(name, index) =>
        val array = funVarTable get name
        val globalArray = MEM.functionDirectory("global").variables get name
        if (array isDefined) EvalExpr(elemOfIndex(array.get, index), array.get.typ)
        else if (globalArray isDefined) EvalExpr(elemOfIndex(array.get, index), globalArray.get.typ)
        else sys.error(s"Error: The variable $name doesn't exist")

      case IdMatrix(name, row, col) =>
        val matrix = funVarTable get name
        val globalMatrix = MEM.functionDirectory("global").variables get name
        if (matrix isDefined) EvalExpr(elemOfIndices(matrix.get, row, col), matrix.get.typ)
        else if (globalMatrix isDefined) EvalExpr(elemOfIndices(matrix.get, row, col), globalMatrix.get.typ)
        else sys.error(s"Error: The variable $name doesn't exist")

      case FunCall(name, params) =>
        val function = MEM.functionDirectory get name
        if (function isDefined) {
          val paramsNames = function.get.paramsNames
          if (params.length == paramsNames.length) {
            for (i <- params.indices) {
              val variable = function.get.variables(paramsNames(i))
              val evaluatedExpr = addExprQuads(funVarTable, params(i))
              if (evaluatedExpr.typ == variable.typ) {
                MEM.addQuadruple(ADR.param, evaluatedExpr.address, -1, variable.address)
              }
              else sys.error("ERROR: Type mismatch on function call " + name)
            }
            MEM.addQuadruple(ADR.gosub, -1, -1, function.get.firstLine)

            // Assign return expression to function address
            val funAdr = function.get.address
            val returnAdr = function.get.returnAddress
            // if function return address is not defined add assignment quadruple to a queue to modify it later
            if (returnAdr == -1) function.get.addIncorrectAdr(MEM.numberOfQuadruples)
            MEM.addQuadruple(ADR.asgmt, returnAdr, -1, funAdr)
            EvalExpr(funAdr, function.get.typ)
          }
          else sys.error("ERROR: Incorrect number of parameters in function call " + name)
        }
        else sys.error("ERROR: Function " + name + " is not defined")

      case ReadString() => EvalExpr(ADR.rdStr, StringType)
      case ReadInt() => EvalExpr(ADR.rdInt, IntType)
      case ReadFloat() => EvalExpr(ADR.rdFlt, FloatType)
      case Sum(e1, e2) => addArithmeticQuad(ADR.sum, addExprQuads(funVarTable, e1), addExprQuads(funVarTable, e2))
      case Sub(e1, e2) => addArithmeticQuad(ADR.sub, addExprQuads(funVarTable, e1), addExprQuads(funVarTable, e2))
      case Mul(e1, e2) => addArithmeticQuad(ADR.mul, addExprQuads(funVarTable, e1), addExprQuads(funVarTable, e2))
      case Div(e1, e2) => addArithmeticQuad(ADR.div, addExprQuads(funVarTable, e1), addExprQuads(funVarTable, e2))
      case Mod(e1, e2) => addArithmeticQuad(ADR.mod, addExprQuads(funVarTable, e1), addExprQuads(funVarTable, e2))
      case Equals(e1, e2) => addRelationalQuad(ADR.eq, addExprQuads(funVarTable, e1), addExprQuads(funVarTable, e2))
      case NotEquals(e1, e2) => addRelationalQuad(ADR.ne, addExprQuads(funVarTable, e1), addExprQuads(funVarTable, e2))
      case GreaterThan(e1, e2) => addRelationalQuad(ADR.gt, addExprQuads(funVarTable, e1), addExprQuads(funVarTable, e2))
      case GreaterEquals(e1, e2) => addRelationalQuad(ADR.ge, addExprQuads(funVarTable, e1), addExprQuads(funVarTable, e2))
      case LessThan(e1, e2) => addRelationalQuad(ADR.lt, addExprQuads(funVarTable, e1), addExprQuads(funVarTable, e2))
      case LessEquals(e1, e2) => addRelationalQuad(ADR.le, addExprQuads(funVarTable, e1), addExprQuads(funVarTable, e2))
      case And(e1, e2) => addLogicalQuad(ADR.and, addExprQuads(funVarTable, e1), addExprQuads(funVarTable, e2))
      case Or(e1, e2) => addLogicalQuad(ADR.or, addExprQuads(funVarTable, e1), addExprQuads(funVarTable, e2))

      case _ => sys.error("Error: BAD EXPRESSION. DON'T EXPRESS LIKE THAT.")
    }
  }

  def addStatementQuads(funName: String, statements: Seq[Statement]): Unit = {

    val varTable = MEM.functionDirectory(funName).variables

    statements.foreach {
      case Assignment(name, expr) =>
        val variable = varTable get name
        val globalVariable = MEM.functionDirectory("global").variables get name
        if (variable isDefined) varAssignment(variable.get)
        else if (globalVariable isDefined) varAssignment(globalVariable.get)
        else sys.error("Error: Variable " + name + " doesn't exist in this scope")

        def varAssignment(variable: MEM.Var) {
          val evaluatedExpr = addExprQuads(varTable, expr)
          if (evaluatedExpr.typ == variable.typ) {
            evaluatedExpr.address match {
              case ADR.rdInt =>
                val tempInt = ADR.addTempInt()
                MEM.addQuadruple(ADR.asgmt, evaluatedExpr.address, tempInt, variable.address)
              case ADR.rdFlt =>
                val tempFloat = ADR.addTempFloat()
                MEM.addQuadruple(ADR.asgmt, evaluatedExpr.address, tempFloat, variable.address)
              case ADR.rdStr =>
                val tempStr = ADR.addTempString()
                MEM.addQuadruple(ADR.asgmt, evaluatedExpr.address, tempStr, variable.address)
              case _ =>
                MEM.addQuadruple(ADR.asgmt, evaluatedExpr.address, -1, variable.address)
            }
          }
          else sys.error("Error: Expression assignnment of variable " + name + "doesn't match expected type")
        }

      case AssignArray(name, index, expr) =>
        val array = varTable get name
        val globalArray = MEM.functionDirectory("global").variables get name
        if (array isDefined) arrayAssignment(array.get)
        else if (globalArray isDefined) arrayAssignment(globalArray.get)
        else sys.error("Error: Variable " + name + " doesn't exist in this scope")

        def arrayAssignment(arr: MEM.Var) {
          val evaluatedExpr = addExprQuads(varTable, expr)
          if (evaluatedExpr.typ == arr.typ) {
            val evaluatedIndex = addExprQuads(varTable, index)
            MEM.addQuadruple(ADR.ver, evaluatedIndex.address, -1, arr.rows)
            val tempElemAdr = ADR.addTempInt()
            MEM.addQuadruple(ADR.sum, evaluatedIndex.address, ADR.addIntVal(arr.address), tempElemAdr)
            MEM.addQuadruple(ADR.asgmt, evaluatedExpr.address, -1, tempElemAdr)
          }
          else sys.error("Error: Expression assignnment of variable " + name + " doesn't match expected type")
        }

      case AssignMatrix(name, row, col, expr) =>
        val matrix = varTable get name
        val globalMatrix = MEM.functionDirectory("global").variables get name
        if (matrix isDefined) matrixAssignment(matrix.get)
        else if (globalMatrix isDefined) matrixAssignment(globalMatrix.get)
        else sys.error("Error: Variable " + name + " doesn't exist in this scope")

        def matrixAssignment(arr: MEM.Var): Unit = {
          val evaluatedExpr = addExprQuads(varTable, expr)
          if (evaluatedExpr.typ == arr.typ) {
            val evaluatedRowIndx = addExprQuads(varTable, row)
            val evaluatedColIndx = addExprQuads(varTable, col)
            MEM.addQuadruple(ADR.ver, evaluatedRowIndx.address, -1, arr.rows) //
            MEM.addQuadruple(ADR.ver, evaluatedColIndx.address, -1, arr.columns)
            val tempAdr = ADR.addTempInt()
            MEM.addQuadruple(ADR.mul, evaluatedRowIndx.address, ADR.addIntVal(arr.columns), tempAdr)
            val tempElemAdr = ADR.addTempInt()
            MEM.addQuadruple(ADR.sum, tempAdr, evaluatedColIndx.address, tempElemAdr)
            val matAdr = ADR.addTempInt()
            MEM.addQuadruple(ADR.sum, tempElemAdr, ADR.addIntVal(arr.address), matAdr)
            MEM.addQuadruple(ADR.asgmt, evaluatedExpr.address, -1, matAdr)
          }
          else sys.error("Error: Expression assignnment of variable " + name + " doesn't match expected type")
        }

      case IfThen(expr, block) =>
        val evaluatedExpr = addExprQuads(varTable, expr)
        if (evaluatedExpr.typ == BoolType) {
          val gotofLine = MEM.numberOfQuadruples // save the next quadruple line
          MEM.addQuadruple(-1, -1, -1, -1) // to update later with gotof jump quad
          addStatementQuads(funName, block.statements)
          MEM.updateQuadruple(gotofLine, ADR.gotof, evaluatedExpr.address, -1, MEM.numberOfQuadruples)
        }
        else sys.error("ERROR: If condition must contain a boolean expression")

      case IfThenElse(expr, blockOne, blockTwo) =>
        val evaluatedExpr = addExprQuads(varTable, expr)
        if (evaluatedExpr.typ == BoolType) {
          val gotofLine = MEM.numberOfQuadruples // save the next quad line
          MEM.addQuadruple(-1, -1, -1, -1) // to update later with gotof jump
          addStatementQuads(funName, blockOne.statements)
          MEM.updateQuadruple(gotofLine, ADR.gotof, evaluatedExpr.address, -1, MEM.numberOfQuadruples + 1)
          val gotoLine = MEM.numberOfQuadruples
          MEM.addQuadruple(-1, -1, -1, -1) // to update later with gotof jump
          addStatementQuads(funName, blockTwo.statements)
          MEM.updateQuadruple(gotoLine, ADR.goto, -1, -1, MEM.numberOfQuadruples)
        }
        else sys.error("ERROR: If condition must contain a boolean expression")

      case WhileDo(expr, block) =>
        val conditionLine = MEM.numberOfQuadruples // save the next quad line
        val evaluatedExpr = addExprQuads(varTable, expr)
        if (evaluatedExpr.typ == BoolType) {
          val gotofLine = MEM.numberOfQuadruples // save the next quad line
          MEM.addQuadruple(-1, -1, -1, -1) // to update later with gotof jump
          addStatementQuads(funName, block.statements)
          MEM.addQuadruple(ADR.goto, -1, -1, conditionLine)
          MEM.updateQuadruple(gotofLine, ADR.gotof, evaluatedExpr.address, -1, MEM.numberOfQuadruples)
        }
        else sys.error("ERROR: While condition must contain a boolean expression")

      case FunctionCall(name, params) =>
        val function = MEM.functionDirectory get name
        if (function isDefined) {
          val paramsNames = function.get.paramsNames
          if (params.length == paramsNames.length) {
            for (i <- params.indices) {
              val variable = function.get.variables(paramsNames(i))
              val evaluatedExpr = addExprQuads(varTable, params(i))
              if (evaluatedExpr.typ == variable.typ) {
                MEM.addQuadruple(ADR.param, evaluatedExpr.address, -1, variable.address)
              }
              else sys.error("ERROR: Type mismatch on function call " + name)
            }
            MEM.addQuadruple(ADR.gosub, -1, -1, function.get.firstLine)
          }
          else sys.error("ERROR: Incorrect number of parameters in function call " + name)
        }
        else sys.error("ERROR: Function " + name + " is not defined")

      case Write(expr) =>
        val evaluatedExpr = addExprQuads(varTable, expr)
        MEM.addQuadruple(ADR.write, evaluatedExpr.address, -1, -1)

      case DrawRectangle(x, y, width, height, color) =>
        val evaluatedExprX = addExprQuads(varTable, x)
        MEM.addQuadruple(ADR.param, ADR.rect, evaluatedExprX.address, -1)
        val evaluatedExprY = addExprQuads(varTable, y)
        MEM.addQuadruple(ADR.param, ADR.rect, evaluatedExprY.address, -1)
        val evaluatedExprWidth = addExprQuads(varTable, width)
        MEM.addQuadruple(ADR.param, ADR.rect, evaluatedExprWidth.address, -1)
        val evaluatedExprHeight = addExprQuads(varTable, height)
        MEM.addQuadruple(ADR.param, ADR.rect, evaluatedExprHeight.address, -1)
        MEM.addQuadruple(ADR.param, ADR.rect, ADR.addIntVal(color.id), -1)

      case DrawOval(x, y, width, height, color) =>
        val evaluatedExprX = addExprQuads(varTable, x)
        MEM.addQuadruple(ADR.param, ADR.oval, evaluatedExprX.address, -1)
        val evaluatedExprY = addExprQuads(varTable, y)
        MEM.addQuadruple(ADR.param, ADR.oval, evaluatedExprY.address, -1)
        val evaluatedExprWidth = addExprQuads(varTable, width)
        MEM.addQuadruple(ADR.param, ADR.oval, evaluatedExprWidth.address, -1)
        val evaluatedExprHeight = addExprQuads(varTable, height)
        MEM.addQuadruple(ADR.param, ADR.oval, evaluatedExprHeight.address, -1)
        MEM.addQuadruple(ADR.param, ADR.oval, ADR.addIntVal(color.id), -1)

      case DrawLine(x, y, width, height, color) =>
        val evaluatedExprX = addExprQuads(varTable, x)
        MEM.addQuadruple(ADR.param, ADR.line, evaluatedExprX.address, -1)
        val evaluatedExprY = addExprQuads(varTable, y)
        MEM.addQuadruple(ADR.param, ADR.line, evaluatedExprY.address, -1)
        val evaluatedExprWidth = addExprQuads(varTable, width)
        MEM.addQuadruple(ADR.param, ADR.line, evaluatedExprWidth.address, -1)
        val evaluatedExprHeight = addExprQuads(varTable, height)
        MEM.addQuadruple(ADR.param, ADR.line, evaluatedExprHeight.address, -1)
        MEM.addQuadruple(ADR.param, ADR.line, ADR.addIntVal(color.id), -1)

      case _ => sys.error("ERROR STATEMENT")
    }
  }

  /**
    * Link function data to the mutable function directory and evaluate block statements for making quadruples
    *
    * @param  name   name of the function
    * @param  params parameters of the function
    * @param  typ    return type of the function
    * @param  block  block of the function
    */
  def addFunction(name: String, params: Seq[Vars], typ: Type, block: Block, isNotMainBlock: Boolean = true) {
    block match { case Block(vars, statements, retrn) =>
        if (MEM.functionDirectory contains name) sys.error(s"Error: Function $name already defined")
        else {
          val funParamsNames = params.map(_.getName)
          val funVarTable = createVarTable(createVarTable(MEM.VarTable(), params), vars)
          MEM.functionDirectory(name) = MEM.Fun(typ, funParamsNames, funVarTable, statements, MEM.numberOfQuadruples)

          // create statement quadruples
          addStatementQuads(name, statements)

          // create function return quadruple if it is not main block and a end quadruple if it is
          if (isNotMainBlock) {
            val function = MEM.functionDirectory(name)
            val returnExpr = addExprQuads(funVarTable, retrn)
            // Define the return address expression for future function calls
            function.returnAddress = returnExpr.address
            /* Now that the function has a return address defined, modify the quads previously defined that used
            this function */
            function.correctRtnAdrs()
            if (returnExpr.typ == typ) MEM.addQuadruple(ADR.retrn, returnExpr.address, -1, -1)
            else sys.error("Error: return expression of type " + returnExpr.typ + " doesn't match function type")
          }
          else MEM.addQuadruple(ADR.end, -1, -1, -1)
        }
    }
  }

  /**
    * Adds variables to the variable table
    *
    * @param  table The variable table that belongs a function scope
    * @param  vars  variables of type Variable which belongs a function scope
    * @return The variable table with the variables added
    */
  private def createVarTable(table: VarTable, vars: Seq[Vars]): VarTable = {
    if (vars.isEmpty) table
    else {
      vars.head match {
        case Variable(name, typ) =>
          if (table contains name) sys.error(s"Error: Variable $name already defined")
          else createVarTable(table + (name -> MEM.Var(typ)), vars.tail)
        case Array(name, typ, size) =>
          if (table contains name) sys.error(s"Error: Variable $name already defined")
          else createVarTable(table + (name -> MEM.Var(typ, size)), vars.tail)
        case Matrix(name, typ, row, col) =>
          if (table contains name) sys.error(s"Error: Variable $name already defined")
          else createVarTable(table + (name -> MEM.Var(typ, row, col)), vars.tail)
      }
    }
  }
}