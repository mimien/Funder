/**
  * Class description
  *
  * @author emiliocornejo
  *         created on 30/04/17
  */
sealed trait DataType {
  def toString: String
}

case class IntN(num: Int) extends DataType {
  override def toString: String = num.toString
}

case class FltN(num: Float) extends DataType {
  override def toString: String = num.toString
}

case class StrL(lit: String) extends DataType {
  override def toString: String = lit.toString
}

case class Bool(bool: Boolean) extends DataType {
  override def toString: String = bool.toString
}