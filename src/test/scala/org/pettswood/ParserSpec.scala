package org.pettswood

import org.pettswood.Xml._
import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.mock._
import parsers.xml.scala.Parser
import xml._

class ParserSpec extends SpecificationWithJUnit with Mockito {

  class Fixture {
    val domain = mock[DomainBridge]
    val summary = mock[ResultSummary]
    domain.summary returns summary
    domain.cell(any[String]) returns Pass("Monkeys")
    domain.table(any[String]) returns Setup()
  }

  "when html does not contains tables, the parser" should {
    "not bother the domain" in {
      val fixture = new Fixture()

      new Parser(fixture.domain).parse(
        <html>
          <tag>value</tag>
          <list>
            <monkey>1</monkey>
            <monkey>2</monkey>
          </list>
        </html>
      )

      there was no(fixture.domain).table(any[String])
      there was no(fixture.domain).row()
      there was no(fixture.domain).cell(any[String])
    }
  }

  "when html contains tables, the parser" should {
    "delegate table and row handling to the domain" in {
      val fixture = new Fixture()
      fixture.domain.row(Seq("Hello","World")) returns Seq(Setup(),Pass("pass"))

      new Parser(fixture.domain).parse(
        <table>
          <tr>
            <th>Hello</th>
            <td>World</td>
          </tr>
        </table>
      )

      there was one(fixture.domain).table("Hello")
      there was one(fixture.domain).row(Seq("Hello","World"))
    }

    "inject pass/fail css classes into the output" in {
      val fixture = new Fixture()
      fixture.domain.row(
        Seq("pass", "fail", "setup", "exception")
      ) returns
        Seq(Pass("pass"), Fail("monkeys"), Setup(), Exception(new RuntimeException("the computer has gone to lunch")))

      val out = new Parser(fixture.domain).parse(
        <tr>
          <td>pass</td>
          <td>fail</td>
          <td>setup</td>
          <td>exception</td>
        </tr>
      )

      val cells = out.child collect { case e: Elem => e }
      (cells(0) \ "@class").text must be equalTo "Pass"
      (cells(1) \ "@class").text must be equalTo "Fail"
      (cells(2) \ "@class").text must be equalTo "Setup"
      (cells(3) \ "@class").text must be equalTo "Exception"
    }

    "display expected vs actual for failure results" in {
      val fixture = new Fixture()
      fixture.domain.row(Seq("sausage")) returns Seq(Fail("potato"))

      new Parser(fixture.domain).parse(<tr><td>sausage</td></tr>) must be equalTo
        <tr><td class="Fail"><span>potato<br></br>but expected:<br></br></span>sausage</td></tr>
      // TODO - CAS - 20/05/2012 - failures really could be displayed better, like this:
//        <td class="Fail"><span class="strikethrough">sausage</span>potato</td>
    }

    "display exception stack traces in cells" in {
      val fixture = new Fixture()
      fixture.domain.row(Seq("sausage")) returns Seq(Exception(new NullPointerException("Your pointy things are all null")))


      // TODO - children of result are NodeSeq and children of expect are ArrayBuffer. WTF?!?
      val result = new Parser(fixture.domain).parse(<tr><td>sausage</td></tr>)

      val linesout = (result \\ "td" \\ "span").text.lines
      linesout.next() must startWith("java.lang.NullPointerException: Your pointy things are all null")
      linesout.next() must startWith("\tat org.pettswood.ParserSpec")
    }

    "respect existing classes" in {
      val fixture = new Fixture()
      fixture.domain.row(Seq("expected")) returns Seq(Pass("actual"))

      new Parser(fixture.domain).parse(<tr><td class="displayElegantly">expected</td></tr>) must be equalTo <tr><td class="displayElegantly Pass">expected</td></tr>
    }

    "recurse into nested tables, wrapping them in a div" in {
      val fixture = new Fixture()
      val nestlingDomain = mock[DomainBridge]
      nestlingDomain.row(any[Seq[String]]) returns Seq(Pass("Monkeys"))
      nestlingDomain.table(any[String]) returns Setup()
      fixture.domain.nestedDomain() returns nestlingDomain

      new Parser(fixture.domain).parse(<td><table><tr><td>Nested Table</td></tr></table></td>) must be equalTo <td><div><table class="Setup"><tr><td class="Pass">Nested Table</td></tr></table></div></td>
      
      there was one(nestlingDomain).table("Nested Table")
    }

    "decorate output files with a results summary at the end of the body" in {
      val fixture = new Fixture()
      fixture.summary.toString returns "High Score: 472"
      fixture.summary.overallPass returns false

      val actual = new Parser(fixture.domain).decorate( <body><p/></body> )

      actual must be equalTo <body><p/><div class="container"><div class="row"><table class="table"><tr><th>Results:</th><td class="Fail">High Score: 472</td></tr></table></div></div></body>
    }

    "not decorate output files that have no body" in {
      val fixture = new Fixture()

      val actual = new Parser(fixture.domain).decorate( <div><p/></div> )

      actual must be equalTo <div><p/></div>
    }

    "Script tags retain their closing tag, so that browsers don't ignore them" in {
      val fixture = new Fixture()

      val before =
        <html>
          <head>
            <script type="text/javascript" src="javascript/jquery-1.7.2.min.js"></script>
          </head>
          <body/>
        </html>

      val after = new Parser(fixture.domain).parse(before)

      after must equalExactly (before)
    }
  }
}

//class NodeSeqWrapper(nodeSeq: NodeSeq) {
//  def \@(selector: String): NodeSeq = {
//    val bits = selector.split("=")
//    nodeSeq.filter(node => ((node \\ ("@" + bits(0))).text) contains bits(1))
//  }
//
//  def myFilter(elem: NodeSeq): Boolean = ((elem \\ "@class").text) contains "result"
//}

//object NodeSeqWrapper {
//  implicit def ToNodeSeqWrapper(someNodeSeq: NodeSeq): NodeSeqWrapper = new NodeSeqWrapper(someNodeSeq)
//}