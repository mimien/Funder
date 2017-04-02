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
        |matrix[10][1] arr: Int
        |fun f(var number: Int, var str: String): Int
        |  var z: Bool
        |  z = true
        |  if z then
        |    write("hola")
        |  return 1 + 1
        |
        |fun f2(): String
        |  var s: String
        |  var i: Int
        |  i = 0
        |  while i <> 3 do
        |    s = readString()
        |    s = s + " mundo"
        |    i = i + 1
        |  write("fin")
        |  return s
        |
        |main
        |  var z: Float
        |  z = 4 - 10 + 4 * 4.2
        |  if z > 20 then
        |    z = z + 1
        |    write((z + 2) / 10)
        |  else
        |    write("z " + "mal")
        |    write("z < 20")
        |  return 0
        |""".stripMargin
    val result = Lexer(code)
    println(result)
    val ast = Compiler(code)
    println(ast)
    val eval = ast match {
      case Right(p@Program(_, _, _)) => Evaluator(p)
      case Left(value) => println(value)
      case Right(value) => println(value)
    }
    print(eval)
  }
}
