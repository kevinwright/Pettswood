package org.pettswood

case class ResultSummary(domain: DomainBridge, results: List[Result], children: List[ResultSummary]) {

  val totalTally: Tally = (tally(results) /: children)((aggregator, nextChild) => aggregator.plus(nextChild.totalTally))

  def writeNestedString(out: String => Unit = println, depth: Int = 0): Unit = {
    out(("  " * depth)  + tally(results))  + " [" + domain.name + "]"
    children map { _.writeNestedString(out,depth+1) }
  }

  def tally(someResults: List[Result]): Tally =  {
    var pass, fail, setup, exception = 0
    someResults foreach {
      case x: Pass => pass += 1
      case x: Fail => fail += 1
      case x: Setup => setup += 1
      case x: Exception => exception += 1
    }
    Tally(pass, fail, setup, exception)
  }

  def overallPass = totalTally.overallPass
  override def toString = totalTally.toString
  override def equals(that: Any) = totalTally.toString.equals(that + "")
}

object ResultSummary {
  def apply(domain: DomainBridge, results: List[Result]): ResultSummary = ResultSummary(domain, results, List.empty[ResultSummary])
}

// TODO - CAS - 22/04/2013 - Combine with ResultSummary
case class Tally(pass: Int, fail: Int, setup: Int, exception: Int) {
  override def toString = "Pass: " + pass + " Setup: " + setup + " Fail: " + fail + " Exception: " + exception
  def overallPass = pass >= 0 && (fail + exception) == 0
  def plus(that: Tally): Tally = Tally(pass + that.pass, fail + that.fail, setup + that.setup, exception + that.exception)
}