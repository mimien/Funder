package vm

/**
  * Class description
  *
  * @author emiliocornejo
  *         created on 18/04/17
  */
object VirtualMachine {

  val ADR = Memory.Addresses
  val OPS = Operations

  def apply(memory: Vector[Array[Int]], quadruples: Vector[Array[Int]]): Unit = {

    def runLine(quadruple: Array[Int]) {
      quadruple match {
        case Array(operation, leftOpr, rightOpr, result) =>
          operation match {
            case ADR.goto => runLine(quadruples(result))
          }
      }
    }

    quadruples.foreach(runLine)
  }
}
