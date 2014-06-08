package org.pettswood

import org.specs2.mutable.Specification
import runners.SingleRunner
import org.specs2.matcher.MatchResult

class SingleRunnerSpec extends Specification {
  //"OverworkedExample.html" in pettswood("src/test/resources/OverworkedExample.html")
  "Pettswood.html" in pettswood("src/test/resources/Pettswood.html")
  //"PoorlyFormedXml.html" in pettswood("src/test/resources/PoorlyFormedXml.html")

  def pettswood(path: String): MatchResult[Boolean] = {
    val summary = SingleRunner(path).get
    if(!summary.overallPass) {
      println("")
      println("=== FAILURE ===")
      println("===============")
      println(summary.results mkString "\n")
    }
    summary.overallPass must beTrue
  }
}