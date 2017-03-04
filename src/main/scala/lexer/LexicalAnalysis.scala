package lexer

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

/**
  * Class description
  *
  * @author emiliocornejo
  *         created on 04/03/17
  */
object LexicalAnalysis extends RegexParsers {

  override val whiteSpace: Regex = "[ \t\r\f]+".r

  override def skipWhitespace = true

  /**
    * Set the regular expression for the id token
    *
    * The regex returned by the function r is implicitly converted to a Parser[String] when
    * applying the ^^ function that maps the conversion of string to the token IDENTIFIER
    *
    * @return a parser that recognizes the identifier token
    */
  def identifier: Parser[IDENTIFIER] = positioned {
    "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => IDENTIFIER(str) }
  }

  def valString: Parser[VAL_STRING] = positioned {
    """"[^"]*"""".r ^^ { str =>
      val content = str.substring(1, str.length - 1) // removes the " at the sides
      VAL_STRING(content)
    }
  }

  def valInt: Parser[VAL_INT] = positioned {
    "[0-9]+".r ^^ { str =>
      val int = str.toInt
      VAL_INT(int)
    }
  }

  def valFloat: Parser[VAL_FLOAT] = positioned {
    "[0-9]+.[0-9]+".r ^^ { str =>
      val float = str.toFloat
      VAL_FLOAT(float)
    }
  }

  def indentation: Parser[INDENTATION] = positioned {
    "\n[ ]*".r ^^ { whitespace =>
      val nSpaces = whitespace.length - 1
      INDENTATION(nSpaces)
    }
  }

  def assign: Parser[ASSIGN] = positioned { "=" ^^ (_ => ASSIGN()) }

  def colon: Parser[COLON] = positioned { ":" ^^ (_ => COLON()) }

  def comma: Parser[COMMA] = positioned { "," ^^ (_ => COMMA()) }

  def divides: Parser[DIVIDES] = positioned { "/" ^^ (_ => DIVIDES()) }

  def float: Parser[FLOAT] = positioned { "float" ^^ (_ => FLOAT()) }

  def greaterThan: Parser[GREATER_THAN] = positioned { ">" ^^ (_ => GREATER_THAN()) }

  def lessThan: Parser[LESS_THAN] = positioned { "<" ^^ (_ => LESS_THAN()) }

  def plus: Parser[PLUS] = positioned { "+" ^^ (_ => PLUS()) }

  def minus: Parser[MINUS] = positioned { "-" ^^ (_ => MINUS()) }

  def int: Parser[INT] = positioned { "int" ^^ (_ => INT()) }

  def ifCondition: Parser[IF] = positioned { "if" ^^ (_ => IF()) }

  def elseCondition: Parser[ELSE] = positioned { "else" ^^ (_ => ELSE()) }

  def leftParent: Parser[LEFT_PARENT] = positioned { "(" ^^ (_ => LEFT_PARENT()) }

  def rightParent: Parser[RIGHT_PARENT] = positioned { ")" ^^ (_ => RIGHT_PARENT()) }

  def times: Parser[TIMES] = positioned { "*" ^^ (_ => TIMES()) }

  def notEquals: Parser[NOT_EQUALS] = positioned { "<>" ^^ (_ => NOT_EQUALS()) }

  def variable: Parser[VAR] = positioned { "var" ^^ (_ => VAR()) }

  private def processIndentations(tokens: List[Token], indents: List[Int] = List(0)): List[Token] = {
    tokens.headOption match {
      // if there is an increase in indentation level, we push this new level into the stack
      // and produce an INDENT
      case Some(INDENTATION(spaces)) if spaces > indents.head =>
        INDENT() :: processIndentations(tokens.tail, spaces :: indents)

      // if there is a decrease, we pop from the stack until we have matched the new level,
      // producing a DEDENT for each pop
      case Some(INDENTATION(spaces)) if spaces < indents.head =>
        val (dropped, kept) = indents.partition(_ > spaces)
        (dropped map (_ => DEDENT())) ::: processIndentations(tokens.tail, kept)

      // if the indentation level stays unchanged, no tokens are produced
      case Some(INDENTATION(spaces)) if spaces == indents.head =>
        processIndentations(tokens.tail, indents)

      // other tokens are ignored
      case Some(token) => token :: processIndentations(tokens.tail, indents)

      // the final step is to produce a DEDENT for each indentation level still remaining, thus
      // "closing" the remaining open INDENTS
      case None => indents.filter(_ > 0).map(_ => DEDENT())
    }
  }
}
