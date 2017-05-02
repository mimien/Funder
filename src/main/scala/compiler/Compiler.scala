package compiler

import lexical.Lexer
import syntax.{AST, Parser}

/**
  * Class description
  *
  * @author emiliocornejo
  *         created on 06/03/17
  */
object Compiler {
  def apply(code: String): Either[CompilationError, AST] = for {
    tokens <- Lexer(code)
    ast <- Parser(tokens)
  } yield ast
}