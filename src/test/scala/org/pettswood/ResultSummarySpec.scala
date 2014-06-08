package org.pettswood

import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.mock._
import org.pettswood.stubs.Results._

class ResultSummarySpec extends SpecificationWithJUnit with Mockito {
  "The result summary" should {
    "be an overall pass if only Pass and Setup results were received" in {
      ResultSummary(null, List(PASS, SETUP)).overallPass must be equalTo true
    }
    "be an overall fail if any Fail or Exception results were received" in {
      ResultSummary(null, List(PASS, FAIL)).overallPass must be equalTo false
      ResultSummary(null, List(PASS, EXCEPTION)).overallPass must be equalTo false
    }
    "provide a summary of results" in {
      val tally = ResultSummary(null, List(PASS, PASS, PASS, FAIL, FAIL, SETUP, EXCEPTION, EXCEPTION)).totalTally

      tally.pass must be equalTo 3
      tally.fail must be equalTo 2
      tally.setup must be equalTo 1
      tally.exception must be equalTo 2
    }
    "swallow other ResultSummary objects and accumulate their results" in {
      val grandChild1 = ResultSummary(null, List(PASS))
      val child1 = ResultSummary(null, List(PASS, FAIL), List(grandChild1))
      val grandChild2 = ResultSummary(null, List(SETUP))
      val child2 = ResultSummary(null, List(SETUP, EXCEPTION), List(grandChild2))

      val actual = ResultSummary(null, List.empty[Result], List(child1, child2))

      actual must be equalTo ResultSummary(null, List(PASS, PASS, FAIL, SETUP, SETUP, EXCEPTION))
    }
  }
}