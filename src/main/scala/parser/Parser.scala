package parser

import lexer._

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

/**
  * Class description
  *
  * @author emiliocornejo
  *         created on 04/03/17
  */
object Parser extends Parsers {
  override type Elem = Token

}

class TokenReader(tokens: Seq[Token]) extends Reader[Token] {
  override def first: Token = tokens.head

  override def atEnd: Boolean = tokens.isEmpty

  override def pos: Position = tokens.headOption.map(_.pos).getOrElse(NoPosition)

  override def rest: Reader[Token] = new TokenReader(tokens.tail)
}