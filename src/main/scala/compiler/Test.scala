package compiler

import lexical.Lexer
import semantics.Evaluator
import syntax.Program

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
        |var x: Int
        |fun f(var number: Int, var str: String): Int
        |  var x: Float
        |  print(number, str)
        |  return 1 + 1
        |
        |fun f2(): Int
        |  print()
        |  return x
        |
        |main
        |  var z: Float
        |  var y: Float
        |  z = 4 - 10 + 4 * 4
        |  id[z + 1] = 10
        |  return 0
        |""".stripMargin
    val result = Lexer(code)
    println(result)
    val ast = Compiler(code)
    println(ast)
    val eval = ast match {
      case Right(p@Program(_, _, _)) => Evaluator(p)
      case Left(value) => println(value)
    }
    print(eval)
  }
}
