
import scala.io.StdIn

/**
  * Class description
  *
  * @author emiliocornejo
  *         created on 25/04/17
  */
object Operations {
  def readInt(memory: Memory, value: Int, variable: Int)(implicit line: Int): Int = {
    val x = StdIn.readInt()
    memory.saveTempInt(x)
    memory.assign(variable, value)
    line
  }

  def readFloat(memory: Memory, value: Int, variable: Int)(implicit line: Int): Int = {
    val x = StdIn.readFloat()
    memory.saveTempFloat(x)
    memory.assign(variable, value)
    line
  }

  def readString(memory: Memory, value: Int, variable: Int)(implicit line: Int): Int = {
    val s = StdIn.readLine()
    memory.saveTempString(s)
    memory.assign(variable, value)
    line
  }


  def sum(memory: Memory, val1: Int, val2: Int)(implicit line: Int): Int = {
    (memory.value(val1), memory.value(val2)) match {
      case (IntN(x), (IntN(y))) => memory.saveTempInt(x + y)
      case (FltN(x), (IntN(y))) => memory.saveTempFloat(x + y)
      case ((IntN(x)), FltN(y)) => memory.saveTempFloat(x + y)
      case ((FltN(x)), FltN(y)) => memory.saveTempFloat(x + y)
      case ((StrL(x)), StrL(y)) => memory.saveTempString(x + y)
      case _ => sys.error("Wrong format operation")
    }
    line
  }

  def sub (memory: Memory, val1: Int, val2: Int)(implicit line: Int): Int = {
    (memory.value(val1), memory.value(val2)) match {
      case (IntN(x), (IntN(y))) => memory.saveTempInt(x - y)
      case (FltN(x), (IntN(y))) => memory.saveTempFloat(x - y)
      case ((IntN(x)), FltN(y)) => memory.saveTempFloat(x - y)
      case ((FltN(x)), FltN(y)) => memory.saveTempFloat(x - y)
      case _ => sys.error("Wrong format operation")
    }
    line
  }

  def mul (memory: Memory, val1: Int, val2: Int)(implicit line: Int): Int = {
    (memory.value(val1), memory.value(val2)) match {
      case (IntN(x), (IntN(y))) => memory.saveTempInt(x * y)
      case (FltN(x), (IntN(y))) => memory.saveTempFloat(x * y)
      case ((IntN(x)), FltN(y)) => memory.saveTempFloat(x * y)
      case ((FltN(x)), FltN(y)) => memory.saveTempFloat(x * y)
      case _ => sys.error("Wrong format operation")
    }
    line
  }

  def div (memory: Memory, val1: Int, val2: Int)(implicit line: Int): Int = {
    (memory.value(val1), memory.value(val2)) match {
      case (IntN(x), (IntN(y))) => memory.saveTempFloat(x / y)
      case (FltN(x), (IntN(y))) => memory.saveTempFloat(x / y)
      case ((IntN(x)), FltN(y)) => memory.saveTempFloat(x / y)
      case ((FltN(x)), FltN(y)) => memory.saveTempFloat(x / y)
      case _ => sys.error("Wrong format operation")
    }
    line
  }

  def mod (memory: Memory, val1: Int, val2: Int)(implicit line: Int): Int = {
    (memory.value(val1), memory.value(val2)) match {
      case (IntN(x), (IntN(y))) => memory.saveTempInt(x % y)
      case (FltN(x), (IntN(y))) => memory.saveTempFloat(x % y)
      case ((IntN(x)), FltN(y)) => memory.saveTempFloat(x % y)
      case ((FltN(x)), FltN(y)) => memory.saveTempFloat(x % y)
      case _ => sys.error("Wrong format operation")
    }
    line
  }

  def and (memory: Memory, val1: Int, val2: Int)(implicit line: Int): Int = {
    (memory.value(val1), memory.value(val2)) match {
      case (Bool(x), (Bool(y))) => memory.saveTempBool(x && y)
      case _ => sys.error("Wrong format operation")
    }
    line
  }

  def or (memory: Memory, val1: Int, val2: Int)(implicit line: Int): Int = {
    (memory.value(val1), memory.value(val2)) match {
      case (Bool(x), (Bool(y))) => memory.saveTempBool(x || y)
      case _ => sys.error("Wrong format operation")
    }
    line
  }

  def grtThan (memory: Memory, val1: Int, val2: Int)(implicit line: Int): Int = {
    (memory.value(val1), memory.value(val2)) match {
      case (IntN(x), (IntN(y))) => memory.saveTempBool(x > y)
      case (IntN(x), (FltN(y))) => memory.saveTempBool(x > y)
      case (FltN(x), (IntN(y))) => memory.saveTempBool(x > y)
      case (FltN(x), (FltN(y))) => memory.saveTempBool(x > y)
      case _ => sys.error("Wrong format operation")
    }
    line
  }

  def grtEqu (memory: Memory, val1: Int, val2: Int)(implicit line: Int): Int = {
    (memory.value(val1), memory.value(val2)) match {
      case (IntN(x), (IntN(y))) => memory.saveTempBool(x >= y)
      case (IntN(x), (FltN(y))) => memory.saveTempBool(x >= y)
      case (FltN(x), (IntN(y))) => memory.saveTempBool(x >= y)
      case (FltN(x), (FltN(y))) => memory.saveTempBool(x >= y)
      case _ => sys.error("Wrong format operation")
    }
    line
  }

  def lssThan (memory: Memory, val1: Int, val2: Int)(implicit line: Int): Int = {
    (memory.value(val1), memory.value(val2)) match {
      case (IntN(x), (IntN(y))) => memory.saveTempBool(x < y)
      case (IntN(x), (FltN(y))) => memory.saveTempBool(x < y)
      case (FltN(x), (IntN(y))) => memory.saveTempBool(x < y)
      case (FltN(x), (FltN(y))) => memory.saveTempBool(x < y)
      case _ => sys.error("Wrong format operation")
    }
    line
  }

  def lssEqu (memory: Memory, val1: Int, val2: Int)(implicit line: Int): Int = {
    (memory.value(val1), memory.value(val2)) match {
      case (IntN(x), (IntN(y))) => memory.saveTempBool(x <= y)
      case (IntN(x), (FltN(y))) => memory.saveTempBool(x <= y)
      case (FltN(x), (IntN(y))) => memory.saveTempBool(x <= y)
      case (FltN(x), (FltN(y))) => memory.saveTempBool(x <= y)
      case _ => sys.error("Wrong format operation")
    }
    line
  }

  def equ (memory: Memory, val1: Int, val2: Int)(implicit line: Int): Int = {
    (memory.value(val1), memory.value(val2)) match {
      case (IntN(x), (IntN(y))) => memory.saveTempBool(x == y)
      case (IntN(x), (FltN(y))) => memory.saveTempBool(x == y)
      case (FltN(x), (IntN(y))) => memory.saveTempBool(x == y)
      case (FltN(x), (FltN(y))) => memory.saveTempBool(x == y)
      case (StrL(x), (StrL(y))) => memory.saveTempBool(x == y)
      case (Bool(x), (Bool(y))) => memory.saveTempBool(x == y)
      case _ => sys.error("Wrong format operation")
    }
    line
  }

  def notEqu (memory: Memory, val1: Int, val2: Int)(implicit line: Int): Int = {
    (memory.value(val1), memory.value(val2)) match {
      case (IntN(x), (IntN(y))) => memory.saveTempBool(x != y)
      case (IntN(x), (FltN(y))) => memory.saveTempBool(x != y)
      case (FltN(x), (IntN(y))) => memory.saveTempBool(x != y)
      case (FltN(x), (FltN(y))) => memory.saveTempBool(x != y)
      case (StrL(x), (StrL(y))) => memory.saveTempBool(x != y)
      case (Bool(x), (Bool(y))) => memory.saveTempBool(x != y)
      case _ => sys.error("Wrong format operation")
    }
    line
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
    val adr   = 16
    val ver   = 17
    val gotof = 18
    val gosub = 19
    val param = 20
    val rdInt = 21
    val rdFlt = 22
    val rdStr = 23
    val write = 24
    val retrn = 25
    val end   = 26
  }
}
