import java.io.{FileNotFoundException, IOException}

import scala.io.Source
import scala.language.postfixOps
import scala.util.Try

/**
  * Class description
  *
  * @author emiliocornejo
  *         created on 24/04/17
  */
object Run {
  def main(args: Array[String]) {
    val filename = args(0)
    try {
      val lines = Source.fromFile(filename).getLines()
      val data = lines.map(s => s.split(";")).toArray
      val memoryBlocks = {
        val intValues = data.head.toVector map { str => Try(str.toInt).getOrElse(-1) }
        val floatValues = data(1).toVector map { str => Try(str.toFloat).getOrElse(-1.0f) }
        val stringValues = data(2).toVector
        val boolValues = data(3).toVector map { str => Try(str.toBoolean).getOrElse(false) }
        val varAddresses = Array.ofDim[Vector[DataType]](4)
        varAddresses(0) = Vector.fill(data(4).head.toInt)(IntN(0))
        varAddresses(1) = Vector.fill(data(5).head.toInt)(FltN(0.0f))
        varAddresses(2) = Vector.fill(data(6).head.toInt)(StrL(""))
        varAddresses(3) = Vector.fill(data(7).head.toInt)(Bool(false))
        new Memory(intValues, floatValues, stringValues, boolValues, varAddresses)
      }
      val quadruples = data.toVector.drop(8) map (_ map (_ toInt) toList)
      VirtualMachine(memoryBlocks, quadruples)
    }
    catch {
      case _: FileNotFoundException => println("Couldn't find that file.")
      case _: IOException => println("Had an IOException trying to read that file")
    }
  }
}