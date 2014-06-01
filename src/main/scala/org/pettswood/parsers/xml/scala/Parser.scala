package org.pettswood.parsers.xml.scala

import scala.xml._
import org.pettswood._
import java.io._
import org.pettswood.Exception
import org.pettswood.Fail

class Parser(domain: DomainBridge) {

  def parse(node: Node): Node = new TestParser().traverse(node)

  class TestParser extends TraverseCopy {
    def traverse(node: Node) = node match {
      case elem: Elem => elem.label match {
        case "table" => traverseTable(elem)
        case "tr" => traverseRow(elem)
        case "td" if (elem \\ "table").nonEmpty => traverseNestedTable(elem)
        case "td" | "th" => ???
        case _ => parseCopy(elem)
      }
      case any => any
    }

    def traverseTable(elem: Elem): Elem = {
      val result = domain.table(firstCell(elem).text)
      parseCopy(elem, cssAdder(result.name), describeTableFailures(elem.text, result))
    }

    def traverseNestedTable(elem: Elem): Elem = {
      domain.cell("Nested Table")
      <td>{
        new Parser(domain.nestedDomain()).parse(
          <div>{NodeSeq.fromSeq(elem.child)}</div>
        )
      }</td>
    }

    def traverseRow(elem: Elem): Elem = {
      val kids = elem.child.zipWithIndex.map(_.swap)
      val cells = kids.collect{ case (idx, e: Elem) if e.label == "td" || e.label == "tr" => (idx,e) }
      val cellindices = cells map {_._1}
      val noncells = kids filter ( x => !cellindices.contains(x._1) )
      val inputs = cells map {_._2.text}
      val nakedresults = domain.row(inputs)
      val results = cells zip nakedresults map { case ((idx,elem), result) => idx -> (elem->result) }

      val preOutput: Seq[(Int, Any)] = (results ++ noncells) sortBy (_._1)
      val output = preOutput map {
        case (idx, (elem: Elem, result: Result)) => parseCopy(elem, cssAdder(result.name), describeCellFailures(elem.text, result))
        case (idx, elem: Elem) => parseCopy(elem)
        case (idx, node: Node) => node
      }

      elem.copy(
        scope = TopScope,
        minimizeEmpty = elem.label != "script",
        child = output)
    }

  }

  def cssAdder(className: String): (Elem) => MetaData = {
    (elem: Elem) => {
      val classes = (elem \ "@class").text match {
        case "" => className
        case x => x + " " + className
      }
      new UnprefixedAttribute("class", classes, Null)
    }
  }

  def describeCellFailures(expectedText: String, result: Result) = {
    result match {
      case Fail(actual) => <span>{actual}<br></br>but expected:<br></br></span>
      // TODO - CAS - 15/04/2014 - Make this a link to a separate doc, to make files more readable and smaller
      case Exception(t: Throwable) => <span>{exceptionTrace(t)}<br></br>Expected:<br></br></span>
      case _ => NodeSeq.Empty
    }
  }

  def exceptionTrace(t: Throwable) = {
    val writer = new StringWriter()
    t.printStackTrace(new PrintWriter(writer))
    writer.toString
  }

  def describeTableFailures(expectedText: String, result: Result) = {
    val failureMarker = describeCellFailures(expectedText, result)
    if (failureMarker.isEmpty) failureMarker else <tr><td>{failureMarker}</td></tr>
  }

  def firstCell(nodeSeq: NodeSeq): Elem = {
    val tableCells = nodeSeq flatMap (_.descendant) filter (elem => elem.label == "th" || elem.label == "td")
    tableCells.head.asInstanceOf[Elem]
  }

  // TODO - replace overall with calls to summary
  def overall(summary: ResultSummary) = if(summary.overallPass) "Pass" else "Fail"
  def summary = domain.summary

  def decorate(node: Node): Node = new TestDecorator().traverse(node)

  class TestDecorator extends TraverseCopy {
    def traverse(node: Node) = node match {
      case elem: Elem => elem match {
        case <body>{contents@_*}</body> => parseCopy(elem, _.attributes,
          <div class="container"><div class="row"><table class="table"><tr><th>Results:</th><td class={overall(domain.summary)}>{domain.summary.toString}</td></tr></table></div></div>)
        case anyElem => parseCopy(elem)
      }
      case other => other
    }

    override def appendExtras(extraContent: NodeSeq, kids: NodeSeq) = kids :+ extraContent.head
  }
}