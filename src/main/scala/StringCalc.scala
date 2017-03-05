package main.scala

class StringCalc {
  
  private val defaultDelimsRegex = "(,|\n)".r
  private val customDelimPrefix = "//"

  def Sum(input : String) : Int = {
    val nums = ParseInput(input)
    AssertNoNegatives(nums)
    nums.sum
  }

  private def ParseInput(input : String) : List[Int] = input match {
    case "" => List(0)
    case x if UsesCustomDelim(x) => ParseWithCustomDelim(x)    
    case x if UsesDefaultDelims(x) => ParseWithDefaultDelims(x)
    case Int(x) => List(x)
    case _ => List(0)
  }


  private def UsesCustomDelim(input : String) : Boolean = 
    input.startsWith(customDelimPrefix)

  private def ParseWithCustomDelim(input : String) : List[Int] = {        
    val delim = input.substring(customDelimPrefix.length, 3); // skip '//' and take 1 char    
    val numsPart = input.substring(customDelimPrefix.length + 2); // skip '//' + 1 char + '\n' and take rest
    numsPart.split(delim).map(s => s.toInt).toList
  }    


  private def UsesDefaultDelims(input : String) : Boolean =
    defaultDelimsRegex.findFirstIn(input).isDefined

  private def ParseWithDefaultDelims(input : String) : List[Int] =
    defaultDelimsRegex.split(input).map(s => s.toInt).toList

  private object Int {
    def unapply(s : String) : Option[Int] = try {
      Some(s.toInt)
    } catch {
      case _ : java.lang.NumberFormatException => None
    }
  }


  private def AssertNoNegatives(nums : List[Int]) {    
    val ltZeroNums = nums.filter(n => n < 0)
    if (ltZeroNums.nonEmpty ) {
      println("ltZeroNums len > 0")
      val ltZeroNumList = ltZeroNums.map(n => n.toString).mkString(",")
      val message = "negatives not allowed: " + ltZeroNumList
      throw new java.lang.IllegalArgumentException(message)
    }
  }

}