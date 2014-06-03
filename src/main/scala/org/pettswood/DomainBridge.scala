package org.pettswood

class DomainBridge {

  var concepts = Map.empty[String, () => Concept]
  var currentConcept: Concept = NoConceptDefined
  var results: List[Result] = Nil
  var nestlings = List.empty[DomainBridge]

  learn("mixins", new Mixins(this))
  learn("ignore", Ignore)

  def table(firstCellText: String): Result = tryThis {
    //println("New table using concept: " + firstCellText)
    currentConcept = conceptFor(firstCellText)
    //println("concept class: " + currentConcept.getClass.getName)
    Setup()
  }

  def row(cells: Seq[String] = Nil): Seq[Result] = {
    //println("row: " + cells.mkString(", "))
    currentConcept.row()
    cells map { x => cell(x) }
  }

  def cell(text: String): Result =  tryThis {
    val suffix = if(currentConcept.firstCell) " (FIRST)" else ""
    //println("cell: " + text + suffix)
    registerResult(currentConcept.anyCell(text))
  }

  private def tryThis(f: => Result): Result = try {f} catch { case e: Throwable => registerResult(Exception(e)) }

  def registerResult(result: Result): Result = {
    results = result :: results
    result
  }

  def nestedDomain() = {
    val nestling = new DomainBridge
    nestlings = nestling :: nestlings
    currentConcept.nestedConcepts().foreach { x => nestling.learn(x._1, x._2()) }
    nestling
  }

  // TODO - make learn() accept a varargs of (name, conceptoriser): _*
  def learn(name: String, conceptoriser: => Concept) =  {
    //println("learning: " + name)
    concepts += ((name.toLowerCase) -> (() => conceptoriser))
    this
  }
  def summary: ResultSummary = ResultSummary(results, nestlings.map(_.summary))

  def conceptFor(conceptName: String): Concept = concepts.get(conceptName.toLowerCase) match {
    case Some(conceptoriser) => conceptoriser()
    case None => throw new RuntimeException("Unknown concept: \"" + conceptName + "\". Known concepts: [" + concepts.keys.mkString(", ") + "]")
  }
}