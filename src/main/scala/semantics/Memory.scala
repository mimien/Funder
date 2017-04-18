package semantics

import syntax.{Statement, Type}

import scala.collection.immutable.HashMap
import scala.collection.mutable

/**
  * Class description
  *
  * @author emiliocornejo
  *         created on 08/04/17
  */
object Memory {

  type FunDirectory = mutable.HashMap[String, Memory.Fun]
  type VarTable = HashMap[String, Memory.Var]

  val functionDirectory: FunDirectory = mutable.HashMap[String, Fun]()
  val quadruples: mutable.Queue[(Int, Int, Int, Int)] = mutable.Queue[(Int, Int, Int, Int)]()

  def VarTable(): VarTable = HashMap[String, Memory.Var]()

  def FunDirectory(): FunDirectory = mutable.HashMap[String, Memory.Fun]()

  case class Var(typ: Type, row: Int, column: Int) {
    val address: Int = Addresses.varAddress(typ)
  }

  case class Fun(typ: Type, paramsTypes: Seq[Type], variables: VarTable, statements: Seq[Statement] = Seq(),
                 starts: Int = 0)

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
    val gotof = 15
    val gosub = 16
    val era   = 17
    val param = 18
    val rdInt = 19
    val rdFlt = 20
    val rdStr = 21
    val write = 22
    val retrn = 23

    private val intValInitAdr  = 100
    private val fltValInitAdr  = 2500
    private val strValInitAdr  = 5000
    private val boolValInitAdr = 7500
    private val varIntInitAdr  = 10000
    private val varFltInitAdr  = 11000
    private val varStrInitAdr  = 12000
    private val varBoolInitAdr = 13000
    private val tmpIntInitAdr  = 14000
    private val tmpFltInitAdr  = 16000
    private val tmpStrInitAdr  = 18000
    private val tmpBoolInitAdr = 20000

    private val intValAdr = Array.ofDim[Int](fltValInitAdr - intValInitAdr)
    private val fltValAdr = Array.ofDim[Float](strValInitAdr - fltValInitAdr)
    private val strValAdr = Array.ofDim[String](boolValInitAdr - strValInitAdr)
    private val boolValAdr = Array.ofDim[Boolean](varIntInitAdr - boolValInitAdr)
    private val intVarAdr = Array.ofDim[Int](varFltInitAdr - varIntInitAdr)
    private val fltVarAdr = Array.ofDim[Int](varStrInitAdr - varFltInitAdr)
    private val glbStrAdr = Array.ofDim[Int](varBoolInitAdr - varStrInitAdr)
    private val glbBoolAdr = Array.ofDim[Int](22000 - varBoolInitAdr)

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

    def varAddress(typ: Type): Int = {
      import syntax.{IntType, FloatType, StringType, BoolType}

      var address = 0
      typ match {
        case IntType =>
          address = varIntInitAdr + intVarInd
          intVarInd += 1
        case FloatType =>
          address = varFltInitAdr + fltVarInd
          fltVarInd += 1
        case StringType =>
          address = varStrInitAdr + strVarInd
          strVarInd += 1
        case BoolType =>
          address = varBoolInitAdr + boolVarInd
          boolVarInd += 1
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

    def addIntVar(address: Int, num: Int) {
      intVarAdr(address) = num
    }
  }

}