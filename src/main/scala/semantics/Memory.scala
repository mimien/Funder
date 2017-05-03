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
  private val quadruples: mutable.Queue[Quad] = mutable.Queue[Quad]()

  def addQuadruple(operation: Int, leftOpr: Int, rightOpr: Int, result: Int): Unit = {
    quadruples.enqueue(Quad(operation, leftOpr, rightOpr, result))
  }

  def updateQuadruple(index: Int, operation: Int, leftOpr: Int, rightOpr: Int, result: Int): Unit = {
    quadruples.update(index, Quad(operation, leftOpr, rightOpr, result))
  }

  def quadruplesToString: String = quadruples.mkString("\n")

  def numberOfQuadruples: Int = quadruples.length

  def VarTable(): VarTable = HashMap[String, Memory.Var]()

  def FunDirectory(): FunDirectory = mutable.HashMap[String, Memory.Fun]()

  case class Var(typ: Type, rows: Int = 1, columns: Int = 1) {
    val address: Int = Addresses.newVariable(typ, rows, columns)
  }

  case class EvalExpr(address: Int, typ: Type)

  case class Quad(operation: Int, leftOpr: Int, rightOpr: Int, result: Int) {
    def modify(op: Int = operation, leftOperator: Int = leftOpr, rightOperator: Int = rightOpr, res: Int = result):
    Quad = Quad(op, leftOperator, rightOperator, res)

    override def toString: String = s"$operation;$leftOpr;$rightOpr;$result"
  }

  case class Fun(typ: Type, paramsNames: Seq[String], variables: VarTable, statements: Seq[Statement] = Seq(),
                 firstLine: Int = 0, var returnAddress: Int = -1) {
    private val incorrectRtnAdrs: mutable.Queue[Int] = mutable.Queue[Int]()
    val address: Int = Addresses.newVariable(typ, 1, 1)

    def addIncorrectAdr(line: Int): Unit = incorrectRtnAdrs.enqueue(line)

    def correctRtnAdrs(): Unit = {
      while (incorrectRtnAdrs.nonEmpty) {
        val line = incorrectRtnAdrs.dequeue()
        quadruples.update(line, quadruples(line).modify(rightOperator = line))
      }
    }
  }

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
    val mod   = 15
    val end   = 16
    val ver   = 17
    val gotof = 18
    val gosub = 19
    val param = 20
    val rdInt = 21
    val rdFlt = 22
    val rdStr = 23
    val write = 24
    val retrn = 25

    private val tmpBoolAdrRange = 20000 until 22000
    private val tmpStrAdrRange  = 18000 until tmpBoolAdrRange.start
    private val tmpFltAdrRange  = 16000 until tmpStrAdrRange.start
    private val tmpIntAdrRange  = 14000 until tmpFltAdrRange.start
    private val boolVarAdrRange = 13000 until tmpIntAdrRange.start
    private val strVarAdrRange  = 12000 until boolVarAdrRange.start
    private val fltVarAdrRange  = 11000 until strVarAdrRange.start
    private val intVarAdrRange  = 10000 until fltVarAdrRange.start
    private val boolValAdrRange = 7500 until intVarAdrRange.start
    private val strValAdrRange  = 5000 until boolValAdrRange.start
    private val fltValAdrRange  = 2500 until strValAdrRange.start
    private val intValAdrRange  = 100 until fltValAdrRange.start

    private val intValAdr = Array.ofDim[Int](intValAdrRange.length)
    private val fltValAdr = Array.ofDim[Float](fltValAdrRange.length)
    private val strValAdr = Array.ofDim[String](strValAdrRange.length)
    private val boolValAdr = Array.ofDim[Boolean](boolValAdrRange.length)

    private var intValInd  = 0
    private var fltValInd  = 0
    private var strValInd  = 0
    private var boolValInd = 0
    private var intVarInd  = 0
    private var fltVarInd  = 0
    private var strVarInd  = 0
    private var boolVarInd = 0
    private var tmpIntAdr  = tmpIntAdrRange.start
    private var tmpFltAdr  = tmpFltAdrRange.start
    private var tmpStrAdr  = tmpStrAdrRange.start
    private var tmpBoolAdr = tmpBoolAdrRange.start

    def get: String = {
      intValAdr.take(intValInd).mkString(";") + "\n" +
        fltValAdr.take(fltValInd).mkString(";") + "\n" +
        strValAdr.take(strValInd).mkString(";") + "\n" +
        boolValAdr.take(boolValInd).mkString(";") + "\n" +
        intVarInd + "\n" +
        fltVarInd + "\n" +
        strVarInd + "\n" +
        boolVarInd + "\n"
    }

    def newVariable(typ: Type, rows: Int, columns: Int): Int = {
      var address = 0
      typ match {
        case IntType =>
          address = intVarAdrRange(intVarInd)
          intVarInd += rows * columns
        case FloatType =>
          address = fltVarAdrRange(fltVarInd)
          fltVarInd += rows * columns
        case StringType =>
          address = strVarAdrRange(strVarInd)
          strVarInd += rows * columns
        case BoolType =>
          address = boolVarAdrRange(boolVarInd)
          boolVarInd += rows * columns
        case _ => sys.error("Type not supported yet")
      }
      address
    }

    def addTempInt(): Int = {
      val address = tmpIntAdr
      tmpIntAdr += 1
      if (tmpIntAdr == tmpFltAdrRange.end) sys.error("Error: Out of memory for temporal ints")
      else address
    }

    def addTempFloat(): Int = {
      val address = tmpFltAdr
      tmpFltAdr += 1
      if (tmpFltAdr == tmpStrAdrRange.end) sys.error("Error: Out of memory for temporal floats")
      else address
    }

    def addTempString(): Int = {
      val address = tmpStrAdr
      tmpStrAdr += 1
      if (tmpStrAdr == tmpBoolAdrRange.end) sys.error("Error: Out of memory for temporal ints")
      else address
    }

    def addTempBool(): Int = {
      val address = tmpBoolAdr
      tmpBoolAdr += 1
      if (tmpBoolAdr == intValAdrRange.end) sys.error("Error: Out of memory for temporal ints")
      else address
    }

    def addIntVal(n: Int): Int = {
      try intValAdr(intValInd) = n
      catch {
        case e: ArrayIndexOutOfBoundsException => sys.error("Error: Out of memory for ints values")
      }
      val address = intValAdrRange(intValInd)
      intValInd += 1
      address
    }

    def addFltVal(n: Float): Int = {
      try fltValAdr(fltValInd) = n
      catch {
        case e: ArrayIndexOutOfBoundsException => sys.error("Error: Out of memory for float values")
      }
      val address = fltValAdrRange(fltValInd)
      fltValInd += 1
      address
    }

    def addStrVal(str: String): Int = {
      try strValAdr(strValInd) = str
      catch {
        case e: ArrayIndexOutOfBoundsException => sys.error("Error: Out of memory for string values")
      }
      val address = strValAdrRange(strValInd)
      strValInd += 1
      address
    }

    def addBoolVal(bool: Boolean): Int = {
      try boolValAdr(boolValInd) = bool
      catch {
        case e: ArrayIndexOutOfBoundsException => sys.error("Error: Out of memory for string values")
      }
      val address = boolValAdrRange(boolValInd)
      boolValInd += 1
      address
    }
  }
}