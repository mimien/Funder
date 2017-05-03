import scala.collection.mutable

/**
  * Class description
  *
  * @author emiliocornejo
  *         created on 24/04/17
  */
class Memory(intValues: Vector[Int],
             floatValues: Vector[Float],
             stringValues: Vector[String],
             boolValues: Vector[Boolean],
             varAddresses: Array[Vector[DataType]]) {


  private val tempAddresses = Array[List[DataType]](List[IntN](), List[FltN](), List[StrL](), List[Bool]())
  private val int = 0
  private val flt = 1
  private val str = 2
  private val bool = 3
  private val tmpBoolAdrRange = 20000 until 22000
  private val tmpStrAdrRange = 18000 until tmpBoolAdrRange.start
  private val tmpFltAdrRange = 16000 until tmpStrAdrRange.start
  private val tmpIntAdrRange = 14000 until tmpFltAdrRange.start
  private val boolVarAdrRange = 13000 until tmpIntAdrRange.start
  private val strVarAdrRange = 12000 until boolVarAdrRange.start
  private val fltVarAdrRange = 11000 until strVarAdrRange.start
  private val intVarAdrRange = 10000 until fltVarAdrRange.start
  private val boolValAdrRange = 7500 until intVarAdrRange.start
  private val strValAdrRange = 5000 until boolValAdrRange.start
  private val fltValAdrRange = 2500 until strValAdrRange.start
  private val intValAdrRange = 100 until fltValAdrRange.start

  private var stackTrace = List[Int]()

  def saveCurrentPos(line: Int): Unit = {
    stackTrace = line :: stackTrace
  }

  def popLastPosition(): Int = {
    val head = stackTrace.head
    stackTrace = stackTrace.tail
    head
  }

  def saveTempInt(num: Int): Unit = tempAddresses(int) = IntN(num) :: tempAddresses(int)

  def saveTempFloat(num: Float): Unit = tempAddresses(flt) = FltN(num) :: tempAddresses(flt)

  def saveTempString(lit: String): Unit = tempAddresses(str) = StrL(lit) :: tempAddresses(str)

  def saveTempBool(bln: Boolean): Unit = tempAddresses(bool) = Bool(bln) :: tempAddresses(bool)

  def assign(varAddress: Int, valAddress: Int): Unit = {
    varAddress match {
      case n if intVarAdrRange contains n  => updateVariable(int, varAddress, valAddress)
      case n if fltVarAdrRange contains n  => updateVariable(flt, varAddress, valAddress)
      case n if strVarAdrRange contains n  => updateVariable(str, varAddress, valAddress)
      case n if boolVarAdrRange contains n => updateVariable(bool, varAddress, valAddress)
      case n if tmpIntAdrRange contains n =>
        updateVariable(int, tempAddresses(int)(n).asInstanceOf[IntN].num, valAddress)
        popTemp(int)
    }
  }

  private def updateVariable(typ: Int, varAddress: Int, valAddress: Int) {
    val varAdrRange = typ match {
      case `int`  => intVarAdrRange
      case `flt`  => fltVarAdrRange
      case `str`  => strVarAdrRange
      case `bool` => boolVarAdrRange
    }
    varAddresses(typ) = varAddresses(typ).updated(varAdrRange.indexOf(varAddress), value(valAddress))
  }

  // TODO FIXME
  def value(adr: Int): DataType = {
    adr match {
      case n if intValAdrRange contains n  => IntN(intValues(intValAdrRange.indexOf(adr)))
      case n if fltValAdrRange contains n  => FltN(floatValues(fltValAdrRange.indexOf(adr)))
      case n if strValAdrRange contains n  => StrL(stringValues(strValAdrRange.indexOf(adr)))
      case n if boolValAdrRange contains n => Bool(boolValues(boolValAdrRange.indexOf(adr)))
      case n if intVarAdrRange contains n  => varAddresses(int)(intVarAdrRange.indexOf(adr))
      case n if fltVarAdrRange contains n  => varAddresses(flt)(fltVarAdrRange.indexOf(adr))
      case n if strVarAdrRange contains n  => varAddresses(str)(strVarAdrRange.indexOf(adr))
      case n if boolVarAdrRange contains n => varAddresses(bool)(boolVarAdrRange.indexOf(adr))
      case n if tmpIntAdrRange contains n  => popTemp(int)
      case n if tmpFltAdrRange contains n  => popTemp(flt)
      case n if tmpStrAdrRange contains n  => popTemp(str)
      case n if tmpBoolAdrRange contains n => popTemp(bool)
      case _ => sys.error("Fatal Error: Couldnt find value with adr " + adr)
    }
  }

  def popTemp(typ: Int): DataType = {
    val head = tempAddresses(typ).head
    tempAddresses(typ) = tempAddresses(typ).tail
    head
  }

  override def toString: String = s"Memory:\n\t$intValues\n\t$floatValues\n\t$stringValues\n\t$boolValues\n\t" +
    s"${varAddresses.foreach(println)}"
}