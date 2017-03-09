package compiler

import lexer.LexicalAnalysis

/**
  * Class description
  *
  * @author emiliocornejo
  *         created on 04/03/17
  */
object Test {
  def main(args: Array[String]): Unit = {
    val code =
      """
        |main
        |  z = 4 - 10 + 4 * 4
        |  id[z + 1] = 10
        |  return 0
        |""".stripMargin
    val result = LexicalAnalysis(code)
    println(result)
    val ast = Compiler(code)
    println(ast)
  }
}
