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
//val goto  = 1
//val asgmt = 2
//val and   = 3
//val or    = 4
//val eq    = 5
//val ne    = 6
//val gt    = 7
//val lt    = 8
//val ge    = 9
//val le    = 10
//val sum   = 11
//val sub   = 12
//val mul   = 13
//val div   = 14
//val mod   = 14
//val gotof = 15
//val gosub = 16
//val era   = 17
//val param = 18
//val rdInt = 19
//val rdFlt = 20
//val rdStr = 21
//val write = 22
//val retrn = 23
//
//private val intValInitAdr  = 100
//private val fltValInitAdr  = 2500
//private val strValInitAdr  = 5000
//private val boolValInitAdr = 7500
//private val varIntInitAdr  = 10000
//private val varFltInitAdr  = 11000
//private val varStrInitAdr  = 12000
//private val varBoolInitAdr = 13000
//private val tmpIntInitAdr  = 14000
//private val tmpFltInitAdr  = 16000
//private val tmpStrInitAdr  = 18000
//private val tmpBoolInitAdr = 20000
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
        |  f(1, "mimo")
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
        |    write(z < 20)
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
