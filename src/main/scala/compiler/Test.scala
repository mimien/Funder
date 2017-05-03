package compiler

import java.io.{BufferedWriter, File, FileWriter}

import lexical.Lexer
import semantics.Evaluator
import syntax.Program

/**
  * Class description
  *
  * @author emiliocornejo
  *         created on 04/03/17
  */
//val goto = 1
//val asgmt = 2
//val and = 3
//val or = 4
//val eq = 5
//val ne = 6
//val gt = 7
//val lt = 8
//val ge = 9
//val le = 10
//val sum = 11
//val sub = 12
//val mul = 13
//val div = 14
//val mod = 15
//val end = 16
//val ver = 17
//val gotof = 18
//val gosub = 19
//val param = 20
//val rdInt = 21
//val rdFlt = 22
//val rdStr = 23
//val write = 24
//val retrn = 25
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
        |var m: Int
        |fun f2(var i: Int): Int
        |  if i > 0 then
        |    write(i)
        |    f2(i - 1)
        |  return i
        |
        |main
        |  var n: Int
        |  n = f2(3)
        |  write("la funcion regresa ")
        |  write(n)
        |end
        |""".stripMargin
    val result = Lexer(code)
    println(result)
    val ast = Compiler(code)
    println(ast)
    val eval = ast match {
      case Right(p@Program(_, _, _, _)) =>
        val byteCode = Evaluator(p)
        val file = new File(args(0))
        val bw = new BufferedWriter(new FileWriter(file))
        bw.write(byteCode)
        bw.close()
        byteCode
      case Left(value) => println(value)
      case Right(value) => println(value)
    }
    print(eval)
  }
}