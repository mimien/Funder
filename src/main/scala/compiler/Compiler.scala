package compiler

import lexer.LexicalAnalysis
import parser.{AST, Parser}

/**
  * Class description
  *
  * @author emiliocornejo
  *         created on 06/03/17
  */
object Compiler {
  def apply(code: String): Either[CompilationError, Any] = {
    for {
      tokens <- LexicalAnalysis(code)
      ast <- Parser(tokens)
    } yield ast
  }
}
