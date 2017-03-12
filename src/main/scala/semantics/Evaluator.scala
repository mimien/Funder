package semantics

import syntax._

import scala.collection.mutable

/**
  * Class description
  *
  * @author emiliocornejo
  *         created on 10/03/17
  */
object Evaluator {

  type VarsTable = mutable.HashMap[String, (Type, Option[Value])]
  type FunTable = mutable.HashMap[String, (Type, VarsTable, Option[Expression])]
  val functionDir: FunTable = mutable.HashMap[String, (Type, VarsTable, Option[Expression])]()

  def defaultVarsTable: VarsTable = mutable.HashMap[String, (Type, Option[Value])]()

  def apply(program: Program): FunTable = {
    functionDir("global") = (IntType, processVariables(defaultVarsTable, program.vars), None)
    processFunction("main", Seq(), IntType, program.main, functionDir)
    program.functions.foreach(f => processFunction(f.id, f.params, f.returnTyp, f.block, functionDir))
    functionDir
  }

  //  def processStatements(table: VarsTable, statements: Seq[Statement]): Unit = {
  //    statements match {
  //      case Assignment(name, expr) => if (table contains name) table(name) = table(name) = (table(name)._1, Some
  //    }
  //  }

  def processFunction(name: String, params: Seq[Vars], typ: Type, block: Block, table: FunTable): FunTable = {
    val varsTable = processVariables(defaultVarsTable, params)
    block match {
      case Block(vars, statements, retrn) =>
        if (table contains name) println(s"La funcion $name ya existe")
        else table(name) = (typ, processVariables(varsTable, vars), Some(retrn))
    }
    table
  }

  def processVariables(table: VarsTable, vars: Seq[Vars]): VarsTable = {
    if (vars.isEmpty) table
    else {
      val head = vars.head
      head match {
        case Variable(name, typ) =>
          if (table contains name) println(s"La variable $name ya existe")
          else table(name) = (typ, None)
      }
      processVariables(table, vars.tail)
    }
  }
}
