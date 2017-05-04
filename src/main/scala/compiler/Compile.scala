package compiler

import java.io._

import lexical.Lexer
import semantics.Evaluator
import syntax.Program

import scala.io.Source

/***
  *   created on 04/03/17
  */
object Compile {
  def main(args: Array[String]): Unit = {
    val filename = args(0)
    try {
      val code = Source.fromFile(filename).getLines.mkString("\n")
      println(code)
      val result = Lexer(code)
      println(result)
      val ast = Compiler(code)
      println(ast)
      val eval = ast match {
        case Right(p@Program(_, _, _, _)) =>
          val byteCode = Evaluator(p)
          val file = new File(filename.split("\\.").head + ".fun")
          val bw = new BufferedWriter(new FileWriter(file))
          bw.write(byteCode)
          bw.close()
          byteCode
        case Left(value) => println(value)
        case Right(value) => println(value)
      }
      print(eval)
    }
    catch {
      case _: FileNotFoundException => println("Couldn't find that file.")
      case _: IOException => println("Had an IOException trying to read that file")
    }
  }
}