package org.pettswood

import files._
import Xml._
import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.mock._
import io.Source._
import java.io.File
import org.specs2.specification.AfterExample
import util.Properties._
import scala.xml.NodeSeq

class FileSystemSpec extends SpecificationWithJUnit with Mockito with AfterExample {

  args(sequential = true)

  val BASE_PATH = userDir + File.separator
  val TARGET_DIR = "./target/pettswood/unitTests/"

  def after = {
    new FileSystem in TARGET_DIR find ".*" foreach (new File(_).delete())
    "Why does this procedure have to return Any(thing)?"
  }

  "My little css publishing fiddle" should {
    "copy CSS and JavaScript from test/resources to main/resources" in {
      val fileSystem = new FileSystem

      fileSystem.copy("src/test/resources/css/pettswood.css", "src/main/resources/css/pettswood.css")
      fileSystem.copy("src/test/resources/javascript/pettswood.js", "src/main/resources/javascript/pettswood.js")
      fileSystem.copy("src/test/resources/javascript/jquery-1.7.2.min.js", "src/main/resources/javascript/jquery-1.7.2.min.js")

      fromFile("src/main/resources/css/pettswood.css").mkString must be equalTo fromFile("src/test/resources/css/pettswood.css").mkString
    }
  }

  "The FileSystem proxy" should {

    "write output files" in {
      val fileSystem = new FileSystem

      fileSystem save ("some data") to (TARGET_DIR + "some.file")

      fromFile(TARGET_DIR + "some.file").mkString must be equalTo "some data"
    }

    "make sure that the target folder exists when writing a file" in {
      val fileSystem = new FileSystem

      fileSystem save ("some data") to (TARGET_DIR + "a/very/nested/directory/structure/some.file")

      fromFile(TARGET_DIR + "a/very/nested/directory/structure/some.file").mkString must be equalTo "some data"
    }

    "finds absolutely no files in folders that don't exist" in {
      val fileSystem = new FileSystem

      fileSystem in TARGET_DIR + "some/silly/folder" find "Monkeys.html" must be equalTo List.empty[String]
    }

    "know how to find files by name regex" in {
      val fileSystem = new FileSystem

      def p(s: String) = s.replace('/', File.separatorChar)

      fileSystem in p("src/test") find ".*.html" must contain (
        BASE_PATH + p("src/test/resources/WritingTestsAndFixture.html")
      )
      
      fileSystem in p("src/main/scala/") find "R.*.scala" must be equalTo List(
        BASE_PATH + p("src/main/scala/org/pettswood/MultiRow.scala"),
        BASE_PATH + p("src/main/scala/org/pettswood/Result.scala"),
        BASE_PATH + p("src/main/scala/org/pettswood/ResultSummary.scala"),
        BASE_PATH + p("src/main/scala/org/pettswood/runners/DisposableRunner.scala")
      )
    }

    "Convert relative paths to absolute" in {
      val fileSystem = new FileSystem

      fileSystem in "src/test" must be equalTo Finder(BASE_PATH + "src/test")

      fileSystem save "some data" to TARGET_DIR + "a.file"
      fromFile(BASE_PATH + TARGET_DIR + "a.file").mkString must be equalTo "some data"
    }

    "Copy files" in {
      val fileSystem = new FileSystem
      fileSystem.save("some monkeys").to(TARGET_DIR + "tmp1/monkeys.file")

      fileSystem.copy(TARGET_DIR + "tmp1/monkeys.file", TARGET_DIR + "tmp2/moreMonkeys.file")
      
      fromFile(TARGET_DIR + "tmp2/moreMonkeys.file").mkString must be equalTo "some monkeys"
    }

    "allow html with doctypes" in {
      load("<!DOCTYPE HTML><html><body>monkeys</body></html>") must equalStructurally(<html><head></head> <body>monkeys</body></html>)
    }

    "allow HTML docs with no doctype" in {
      load("<html><body>monkeys</body></html>") must equalStructurally(<html><head></head> <body>monkeys</body></html>)
    }
  }

  def load(structure: String): NodeSeq = {
    val someFile = TARGET_DIR + "whatever.xml"
    val fileSystem = new FileSystem
    fileSystem.save(structure) to someFile
    fileSystem.loadXml(someFile)
  }
}