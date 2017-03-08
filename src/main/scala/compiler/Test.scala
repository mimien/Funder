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
        |var x: Int
        |var y: Float
        |
        |fun mac(array[10] param1: Rectangle, var angel: String): Int
        |  var z: Int
        |  z = x + y
        |  if z == 10 then
        |    writ("hola")
        |  else
        |    writ(param1[1])
        |  return z
        |
        |main
        |  var oval: Oval
        |  var i: Int
        |  oval = createOvl(32,32,32,32, darkGry)
        |  while i >= 10 do
        |    setColr(oval, yellw)
        |  return 0
        |
        |""".stripMargin
    val result = LexicalAnalysis(code)
    println(result)
    val ast = Compiler(code)
    println(ast)
  }
}
