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
        |    write("hola")
        |  else
        |    write(param1[1])
        |
        |main
        |  var oval: Oval
        |  var i: Int
        |  oval = createOval(32,32,32,32, darkGray)
        |  while i <> 10 do
        |    setColor(oval, yellow)
        |
        |""".stripMargin
    val result = LexicalAnalysis(code)
    println(result)

  }
}
