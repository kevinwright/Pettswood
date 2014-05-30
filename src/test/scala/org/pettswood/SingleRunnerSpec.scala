package org.pettswood

import org.specs2.mutable.Specification
import runners.SingleRunner
import org.specs2.matcher.MatchResult

class SingleRunnerSpec extends Specification {
  "Pettswood.html" in pettswood("src/test/resources/Pettswood.html")
  //"PoorlyFormedXml.html" in pettswood("src/test/resources/PoorlyFormedXml.html")

  def pettswood(path: String): MatchResult[Boolean] = {
    val summary = SingleRunner(path).get
    if(!summary.overallPass) {
      println(summary.results mkString "\n")
    }
    summary.pp.overallPass must beTrue
  }
}