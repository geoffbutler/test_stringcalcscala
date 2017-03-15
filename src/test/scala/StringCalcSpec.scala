package test.scala

import org.scalatest.FunSuite
import main.scala.StringCalc


class StringCalcSpec extends FunSuite {

  test("StringCalc.sum should return 0 for empty string") {
    val calc = new StringCalc()
    assert(calc.sum("") == 0)
  }

  test("StringCalc.sum should return integer of single parameter") {
    val calc = new StringCalc()
    assert(calc.sum("0") == 0)
    assert(calc.sum("1") == 1)
    assert(calc.sum("10") == 10)
  }

  test("StringCalc.sum should return integer sum of two parameters") {
    val calc = new StringCalc()
    assert(calc.sum("0,0") == 0)
    assert(calc.sum("1,2") == 3)
    assert(calc.sum("2,13") == 15)
    assert(calc.sum("12,13") == 25)
  }

  test("StringCalc.sum should return integer sum of multiple parameters") {
    val calc = new StringCalc()
    assert(calc.sum("0,0,0") == 0)
    assert(calc.sum("1,2,3,4,5") == 15)
    assert(calc.sum("1,2,3,4,5,6,7,8,9,10") == 55)
  }

  test("StringCalc.sum should allow use of newline instead of comma for parameter delimiter") {
    val calc = new StringCalc()
    assert(calc.sum("0\n0\n0") == 0)
    assert(calc.sum("1\n2\n3\n4\n5") == 15)
    assert(calc.sum("1\n2\n3\n4\n5\n6\n7\n8\n9\n10") == 55)
  }

  test("StringCalc.sum should allow mix of newline and comma for parameter delimiter") {
    val calc = new StringCalc()
    assert(calc.sum("0,0\n0") == 0)
    assert(calc.sum("1\n2,3") == 6)
    assert(calc.sum("1\n2,3\n4,5") == 15)
    assert(calc.sum("1,2\n3,4,5,6,7\n8\n9\n10") == 55)
  }

  test("StringCalc.sum should allow custom single char delimiter") {
    val calc = new StringCalc()
    assert(calc.sum("//;\n1;2") == 3)
  }

  test("StringCalc.sum should throw when passed negative parameters") {
    val calc = new StringCalc()
    val ex = intercept[java.lang.IllegalArgumentException] {
      calc.sum("//;\n-1;-2")
    }
    assert(ex.getMessage == "negatives not allowed: -1,-2")
  }

  test("StringCalc.sum should ignore paramters greater than 1000") {
    val calc = new StringCalc()
    assert(calc.sum("2,1001") == 2)
    assert(calc.sum("1000,1") == 1001)
    assert(calc.sum("1001,2") == 2)
  }

  test("StringCalc.sum should allow multiple character delimiters of any length") {
    val calc = new StringCalc()
    assert(calc.sum("//[***]\n1***2***3") == 6)
    assert(calc.sum("//[###]\n1###2###3") == 6)
  }

  test("StringCalc.sum allows multiple multiple character delimiters") {
    val calc = new StringCalc()
    assert(calc.sum("//[*][%]\n1*2%3") == 6)
    assert(calc.sum("//[***][%%%]\n1***2%%%3") == 6)
  }
}