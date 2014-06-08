package org.pettswood.specification.concepts

import org.pettswood._
import files.FileSystem
import java.io.File
import parsers.xml.scala.Parser
import runners.DisposableRunner

class Pettswood extends Concept with MultiRow {

  var nestedDomain: DomainBridge = null

  def columns = {
    case "Test File" => PettswoodRunner
    case "Output File" => FileExists
    case "Results" => DoNothing
  }

  val results = () => {
    nestedDomain.log("yielding results: " + nestedDomain.summary)
    new TimedResults(nestedDomain.summary)
  }
  override def nestedConcepts() =
    Map( "Results" -> results )

  case class PettswoodRunner(filePath: String) extends Doer {
    nestedDomain = new DomainBridge
    new DisposableRunner(new Parser(nestedDomain), new FileSystem).run(filePath)
    println("nested run completed with: " + nestedDomain.summary.totalTally)
  }

  case class FileExists(filePath: String) extends Digger {
    val actual = if (!new File(filePath).exists()) "File not found" else filePath
  }
}