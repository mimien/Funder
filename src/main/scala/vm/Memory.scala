package vm

import scala.collection.mutable

/**
  * Class description
  *
  * @author emiliocornejo
  *         created on 24/04/17
  */
object Memory {

  object Addresses {
    val tempInt: mutable.Queue[Int] = mutable.Queue[Int]()

    def saveTemp(fl: AnyVal,  tmpAdr: Int): Unit = {

    }

    def saveTempInt(num: Int,  tmpAdr: Int): Unit = {
      tempInt.enqueue(num)
    }

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

    val tmpBoolAdrRange: Range = 20000 until 22000
    val tmpStrAdrRange: Range = 18000 until tmpBoolAdrRange.start
    val tmpFltAdrRange: Range = 16000 until tmpStrAdrRange.start
    val tmpIntAdrRange: Range = 14000 until tmpFltAdrRange.start
    val boolVarAdrRange: Range = 13000 until tmpIntAdrRange.start
    val strVarAdrRange: Range = 12000 until boolVarAdrRange.start
    val fltVarAdrRange: Range = 11000 until strVarAdrRange.start
    val intVarAdrRange: Range = 10000 until fltVarAdrRange.start
    val boolValAdrRange: Range = 7500 until intVarAdrRange.start
    val strValAdrRange: Range = 5000 until boolValAdrRange.start
    val fltValAdrRange: Range = 2500 until strValAdrRange.start
    val intValAdrRange: Range = 100 until fltValAdrRange.start

    private val intValues  = 0
    private val fltValues  = 1
    private val strValues  = 2
    private val boolValues = 3
    private val intVars    = 4
    private val fltVars    = 5
    private val strVars    = 6
    private val boolVars   = 7

    sealed trait DataTypes
    case class IntgrNum(n: Int) extends DataTypes
    case class FloatNum(n: Float) extends DataTypes
    case class Str(d: String) extends DataTypes
    case class Bool(b: Boolean) extends DataTypes

    def value(memory: Vector[Array[Int]], adr: Int): DataTypes = {
      adr match {
        case n if intValAdrRange contains n => IntgrNum(memory(intValues)(intValAdrRange.indexOf(adr)))
        case n if fltValAdrRange contains n => FloatNum(memory(fltValues)(fltValAdrRange.indexOf(adr)))
        case n if strValAdrRange contains n => Str(memory(strValues)(strValAdrRange.indexOf(adr)).toString)
//        case n if boolValAdrRange contains n => Bool(memory(boolValues)(boolValAdrRange.indexOf(adr)))
//        case n if intVarAdrRange contains n => memory(intVars)(intVarAdrRange.indexOf(adr))
//        case n if fltVarAdrRange contains n => memory(fltVars)(fltVarAdrRange.indexOf(adr))
//        case n if strVarAdrRange contains n => memory(strVars)(strVarAdrRange.indexOf(adr))
//        case n if boolVarAdrRange contains n => memory(boolVars)(boolVarAdrRange.indexOf(adr))
        case _ => ???
      }
    }
  }
}