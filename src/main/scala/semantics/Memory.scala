package semantics

import syntax.{Expression, Statement, Type}

import scala.collection.immutable.HashMap
import scala.collection.mutable

/**
  * Class description
  *
  * @author emiliocornejo
  *         created on 08/04/17
  */
object Memory {

  import syntax.{IntType, FloatType, StringType, BoolType}

  type FunDirectory = mutable.HashMap[String, Memory.Fun]
  type VarTable = HashMap[String, Memory.Var]

  val functionDirectory: FunDirectory = mutable.HashMap[String, Fun]()
  val quadruples: mutable.Queue[(Int, Int, Int, Int)] = mutable.Queue[(Int, Int, Int, Int)]()

  def VarTable(): VarTable = HashMap[String, Memory.Var]()

  def FunDirectory(): FunDirectory = mutable.HashMap[String, Memory.Fun]()

/*  def elemOfIndex(arr: Var, index: Expression, varTable: VarTable): Int = {
    val evalIndxExpr = Evaluator.addExprQuads(varTable, index)

    if (evalIndxExpr.typ == IntType) {
      quadruples.enqueue((Addresses.ver, evalIndxExpr.address, -1, arr.rows))
      val arrElemAdrNum = Addresses.addTempInt()
      quadruples.enqueue((Addresses.sum, evalIndxExpr.address, arr.address, arrElemAdrNum))
      val elemAdr = Addresses.addTempInt()
      quadruples.enqueue((Addresses.adr, arrElemAdrNum, -1, elemAdr))
      elemAdr
    }
    else sys.error("Error: Index must be integer type")
  }*/

  case class Var(typ: Type, rows: Int, columns: Int) {
    val address: Int = Addresses.newVariable(typ, rows, columns)
  }

  case class EvalExpr(address: Int, typ: Type)

  case class Fun(typ: Type, paramsTypes: Seq[Type], variables: VarTable, statements: Seq[Statement] = Seq(),
                 firstLine: Int = 0)

  object Addresses {

    val goto  = 1
    val asgmt = 2
    val and   = 3
    val or    = 4
    val eq    = 5
    val ne    = 6
    val gt    = 7
    val lt    = 8
    val ge    = 9
    val le    = 10
    val sum   = 11
    val sub   = 12
    val mul   = 13
    val div   = 14
    val mod   = 14
    val era   = 15
    val ver   = 16
    val adr   = 17
    val gotof = 18
    val gosub = 19
    val param = 20
    val rdInt = 21
    val rdFlt = 22
    val rdStr = 23
    val write = 24
    val retrn = 25

    private val intValInitAdr  = 100
    private val fltValInitAdr  = 2500
    private val strValInitAdr  = 5000
    private val boolValInitAdr = 7500
    private val intVarInitAdr  = 10000
    private val fltVarInitAdr  = 11000
    private val strVarInitAdr  = 12000
    private val boolVarInitAdr = 13000
    private val tmpIntInitAdr  = 14000
    private val tmpFltInitAdr  = 16000
    private val tmpStrInitAdr  = 18000
    private val tmpBoolInitAdr = 20000

    private val intValAdr = Array.ofDim[Int](fltValInitAdr - intValInitAdr)
    private val fltValAdr = Array.ofDim[Float](strValInitAdr - fltValInitAdr)
    private val strValAdr = Array.ofDim[String](boolValInitAdr - strValInitAdr)
    private val boolValAdr = Array.ofDim[Boolean](intVarInitAdr - boolValInitAdr)
    private val intVarAdr = Array.fill[Int](fltVarInitAdr - intVarInitAdr)(-1)
    private val fltVarAdr = Array.fill[Int](strVarInitAdr - fltVarInitAdr)(-1)
    private val strVarAdr = Array.fill[Int](boolVarInitAdr - strVarInitAdr)(-1)
    private val boolVarAdr = Array.fill[Int](22000 - boolVarInitAdr)(-1)

    private var intValInd  = 0
    private var fltValInd  = 0
    private var strValInd  = 0
    private var boolValInd = 0
    private var intVarInd  = 0
    private var fltVarInd  = 0
    private var strVarInd  = 0
    private var boolVarInd = 0
    private var tmpIntAdr  = tmpIntInitAdr
    private var tmpFltAdr  = tmpFltInitAdr
    private var tmpStrAdr  = tmpStrInitAdr
    private var tmpBoolAdr = tmpBoolInitAdr

    def get: String = {
      intValAdr.take(intValInd).mkString(";") + "\n" +
        fltValAdr.take(fltValInd).mkString(";") + "\n" +
        strValAdr.take(strValInd).mkString(";") + "\n" +
        boolValAdr.take(boolValInd).mkString(";") + "\n" +
        intVarAdr.take(intVarInd).mkString(";") + "\n" +
        fltVarAdr.take(fltVarInd).mkString(";") + "\n" +
        strVarAdr.take(strVarInd).mkString(";") + "\n" +
        boolVarAdr.take(boolVarInd).mkString(";") + "\n"
    }

    def newVariable(typ: Type, rows: Int, columns: Int): Int = {
      var address = 0
      typ match {
        case IntType =>
          address = intVarInitAdr + intVarInd
          intVarInd += rows * columns
        case FloatType =>
          address = fltVarInitAdr + fltVarInd
          fltVarInd += rows * columns
        case StringType =>
          address = strVarInitAdr + strVarInd
          strVarInd += rows * columns
        case BoolType =>
          address = boolVarInitAdr + boolVarInd
          boolVarInd += rows * columns
        case _ => sys.error("Type not supported yet")
      }
      address
    }

    def addTempInt(): Int = {
      val address = tmpIntAdr
      tmpIntAdr += 1
      if (tmpIntAdr == tmpFltInitAdr) sys.error("Error: Out of memory for temporal ints")
      else address
    }

    def addTempFloat(): Int = {
      val address = tmpFltAdr
      tmpFltAdr += 1
      if (tmpFltAdr == tmpStrInitAdr) sys.error("Error: Out of memory for temporal floats")
      else address
    }

    def addTempString(): Int = {
      val address = tmpStrAdr
      tmpStrAdr += 1
      if (tmpStrAdr == tmpBoolInitAdr) sys.error("Error: Out of memory for temporal ints")
      else address
    }

    def addTempBool(): Int = {
      val address = tmpBoolAdr
      tmpBoolAdr += 1
      if (tmpBoolAdr == intValInitAdr) sys.error("Error: Out of memory for temporal ints")
      else address
    }

    def addIntVal(n: Int): Int = {
      try intValAdr(intValInd) = n
      catch {
        case e: ArrayIndexOutOfBoundsException => sys.error("Error: Out of memory for ints values")
      }
      val address = intValInitAdr + intValInd
      intValInd += 1
      address
    }

    def addFltVal(n: Float): Int = {
      try fltValAdr(fltValInd) = n
      catch {
        case e: ArrayIndexOutOfBoundsException => sys.error("Error: Out of memory for float values")
      }
      val address = fltValInitAdr + fltValInd
      fltValInd += 1
      address
    }

    def addStrVal(str: String): Int = {
      try strValAdr(strValInd) = str
      catch {
        case e: ArrayIndexOutOfBoundsException => sys.error("Error: Out of memory for string values")
      }
      val address = strValInitAdr + strValInd
      strValInd += 1
      address
    }

    def addBoolVal(bool: Boolean): Int = {
      try boolValAdr(boolValInd) = bool
      catch {
        case e: ArrayIndexOutOfBoundsException => sys.error("Error: Out of memory for string values")
      }
      val address = boolValInitAdr + boolValInd
      boolValInd += 1
      address
    }

    def assignValToVarAdr(expr: EvalExpr, address: Int) {
      expr.typ match {
        case IntType => intVarAdr.update(address - intVarInitAdr,  expr.address)
        case FloatType => fltVarAdr.update(address - fltVarInitAdr, expr.address)
        case StringType => strVarAdr.update(address - strVarInitAdr, expr.address)
        case BoolType => boolVarAdr.update(address - boolVarInitAdr, expr.address)
        case _ => sys.error("Type not supported yet")
      }
    }
  }

}