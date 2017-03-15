package main.scala

import java.util.regex.Pattern
import scala.collection.mutable.ListBuffer

class StringCalc {
  
  private val defaultDelimsRegex = "(,|\n)".r
  private val customDelimPrefix = "//"
  private val customMultiCharDelimPrefix = "["
  private val customMultiCharDelimSuffix = "]"
  private val customMultiCharDelimRegex = "\\[{1}[^\\]\\[]*\\]{1}".r

  def sum(input : String) : Int = {
    val nums = parseInput(input)
    assertNoNegatives(nums)
    val filteredNums = filterInput(nums)
    filteredNums.sum
  }


  private def parseInput(input : String) : List[Int] = input match {
    case "" => List(0)
    case x if usesCustomSingleDelim(x) => parseWithCustomSingleDelim(x)
    case x if usesCustomMultiDelim(x) => parseWithCustomMultiDelim(x)
    case x if usesDefaultDelims(x) => parseWithDefaultDelims(x)
    case Int(x) => List(x)
    case _ => List(0)
  }


  private def usesCustomSingleDelim(input : String) : Boolean =
    input.startsWith(customDelimPrefix) && // starts with '//'
    !input.startsWith(customDelimPrefix + customMultiCharDelimPrefix) // but does not start with '//['

  private def parseWithCustomSingleDelim(input : String) : List[Int] = {
    val delimStartIdx = 2               // skip '//'
    val delimEndIdx = 3                 // '//' + single char
    val delimPrefixLength = 2 + 1 + 1 // skip '\n' and take rest

    val delim = input.substring(delimStartIdx, delimEndIdx)      
    val numsPart = input.substring(delimPrefixLength)      
    val numStrings = numsPart.split(Pattern.quote(delim))      

    numStrings.map(_.toInt).toList
  }


  private def usesCustomMultiDelim(input : String) : Boolean =
    input.startsWith(customDelimPrefix + customMultiCharDelimPrefix) // starts with '//['

  private def parseWithCustomMultiDelim(input : String) : List[Int] = {
    val delimMatches = customMultiCharDelimRegex.findAllIn(Pattern.quote(input))

    // get delims
    val delims = ListBuffer[String]()
    var delimPrefixLength = 2 // skip '//'    
    for (dm <- delimMatches) {
      val delim = dm.substring(1, dm.indexOf(customMultiCharDelimSuffix)) // between '[' and ']'
      delimPrefixLength += (1 + delim.length + 1) // '[' + delim len + ']'
      delims += delim
    }
    delimPrefixLength += 1 // skip '\n'

    // build pattern // TODO: could use StringBuilder here
    var multiDelimPattern = "("        
    multiDelimPattern += delims.map(Pattern.quote).mkString("|")
    multiDelimPattern += ")"

    // get parts
    val numsPart = input.substring(delimPrefixLength)
    val numStrings = numsPart.split(multiDelimPattern)
    
    numStrings.map(_.toInt).toList
  }


  private def usesDefaultDelims(input : String) : Boolean =
    defaultDelimsRegex.findFirstIn(input).isDefined

  private def parseWithDefaultDelims(input : String) : List[Int] =
    defaultDelimsRegex.split(input).map(_.toInt).toList

  private object Int {
    def unapply(s : String) : Option[Int] = try {
      Some(s.toInt)
    } catch {
      case _ : java.lang.NumberFormatException => None
    }
  }


  private def assertNoNegatives(nums : List[Int]) {
    val ltZeroNums = nums.filter(_ < 0)
    if (ltZeroNums.nonEmpty) {      
      val ltZeroNumList = ltZeroNums.map(_.toString).mkString(",")
      val message = "negatives not allowed: " + ltZeroNumList
      throw new java.lang.IllegalArgumentException(message)
    }
  }


  private def filterInput(nums : List[Int]) : List[Int] =
    nums.filter(_ < 1001)
}