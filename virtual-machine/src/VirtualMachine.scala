
/**
  * Class description
  *
  * @author emiliocornejo
  *         created on 18/04/17
  */
object VirtualMachine {

  val ADR = Operations.Addresses
  val OPS = Operations

  def apply(memory: Memory, quadruples: Vector[Seq[Int]]): Unit = run(quadruples, line = 0, memory)

  def run(quadruples: Vector[Seq[Int]], line: Int, memory: Memory) {
    implicit val nextLine = line + 1
    val jumpToLine = quadruples(line) match {
      case Seq(operation, leftOpr, rightOpr, result) =>
        operation match {
          case ADR.goto => result
          case ADR.asgmt => leftOpr match {
            case ADR.rdInt => OPS.readInt(memory, leftOpr, result)
            case ADR.rdFlt => OPS.readFloat(memory, leftOpr, result)
            case ADR.rdStr => OPS.readString(memory, leftOpr, result)
            case _ => memory.assign(result, leftOpr); nextLine
          }
          case ADR.and => OPS.and(memory, leftOpr, rightOpr)
          case ADR.or => OPS.or(memory, leftOpr, rightOpr)
          case ADR.eq => OPS.equ(memory, leftOpr, rightOpr)
          case ADR.ne => OPS.notEqu(memory, leftOpr, rightOpr)
          case ADR.gt => OPS.grtThan(memory, leftOpr, rightOpr)
          case ADR.lt => OPS.lssThan(memory, leftOpr, rightOpr)
          case ADR.ge => OPS.grtEqu(memory, leftOpr, rightOpr)
          case ADR.le => OPS.lssEqu(memory, leftOpr, rightOpr)
          case ADR.sum => OPS.sum(memory, leftOpr, rightOpr)
          case ADR.sub => OPS.sub(memory, leftOpr, rightOpr)
          case ADR.mul => OPS.mul(memory, leftOpr, rightOpr)
          case ADR.div => OPS.div(memory, leftOpr, rightOpr)
          case ADR.mod => OPS.mod(memory, leftOpr, rightOpr)
          case ADR.gotof =>
            if (Bool.unapply(memory.value(leftOpr).asInstanceOf[Bool]).get) nextLine
            else result
          case ADR.gosub => memory.saveCurrentPos(nextLine); result
          case ADR.param => memory.assign(result, leftOpr); nextLine
          case ADR.write => println(memory.value(leftOpr)); nextLine
          case ADR.retrn =>
            memory.popLastPosition()
          case ADR.end => sys.exit(0)
          case _ => nextLine
        }
    }
    run(quadruples, jumpToLine, memory)
  }
}