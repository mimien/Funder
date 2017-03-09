//import org.scalatest.Matchers
//
///**
//  * Class description
//  *
//  * @author emiliocornejo
//  *         created on 09/03/17
//  */
//class ParserTest extends FlatSpec with Matchers {
//
//  val validCode =
//    """
//      |read input name, country
//      |switch:
//      |  country == "PT" ->
//      |    call service "A"
//      |    exit
//      |  otherwise ->
//      |    call service "B"
//      |    switch:
//      |      name == "unknown" ->
//      |        exit
//      |      otherwise ->
//      |        call service "C"
//      |        exit
//    """.stripMargin.trim
//
//  val invalidCode =
//    """
//      |read input name, country
//      |switch:
//      |  country == PT ->
//      |    call service "A"
//      |    exit
//      |  otherwise ->
//      |    call service "B"
//      |    switch:
//      |      name == "unknown" ->
//      |        exit
//      |      otherwise ->
//      |        call service "C"
//      |        exit
//    """.stripMargin.
//
//
//  val errorMsg = ParserError(Location(3,14), "string literal expected")
//
//
//
//
//  "Workflow compiler" should "successfully parse a valid workflow" in {
//    Compiler(validCode) shouldBe Right(successfulAST)
//  }
//
//  it should "return an error with an invalid workflow" in {
//    Compiler(invalidCode) shouldBe Left(errorMsg)
//  }
//
//}