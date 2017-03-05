package compiler

/**
  * Class description
  *
  * @author emiliocornejo
  *         created on 31/01/17
  */
trait CompilationError

case class LexerError(location: Location, msg: String) extends CompilationError

case class ParserError(location: Location, msg: String) extends CompilationError

case class Location(line: Int, column: Int) {
  override def toString = s"$line:$column"
}