package test
import pcf.Interp

object TestInterp extends App {
    test("PCFVertTest1")
    test("PCFVertTest2")
    test("PCFVertTest3")
    test("PCFVertTest4")
    test("PCFVertTest5")
    test("PCFVertTest6")
    test("PCFVertTest7")
    test("PCFVertTest8")
    test("PCFVertTest9")

    test("PCFBleuTest1")
    test("PCFBleuTest2")
    test("PCFBleuTest3")
    test("PCFBleuTest4")
    test("PCFBleuTest5")
    test("PCFBleuTest6")
    test("PCFBleuTest7")
    test("PCFBleuTest8")
    test("PCFBleuTest9")

    test("PCFRougeTest1")
    test("PCFRougeTest2")
    test("PCFRougeTest3")
    test("PCFRougeTest4")
    test("PCFRougeTest5")
    test("PCFRougeTest6")
    test("PCFRougeTest7")
//    test("PCFRougeTest8")

    test("PCFNoirTest1")
    // test("PCFNoirTest2");

    test("PCF0")
    test("PCF1")
    test("PCF2")
    test("PCF3")
    test("PCF4")
    test("PCF5")
    test("PCF6")
    test("PCF7")
    test("PCF8")

    private def test(filename: String): Unit =
        val dir = "testInterp/"
        val args = Array(dir + filename + ".pcf", "-v")
        println(s"**********  ${args(0)}")
        try {
          Interp.main(args)
        } catch
            case ex: Exception => println(s"${ex.getClass} ${ex.getMessage}")

}
