package countercaching

import BooleanStructure.*
import scala.io.Source

/**
  * Incredibly fragile interface for DIMACS format for CNF Boolean formula
  */
object DIMACSInterface {
  // parse file into list of clause strings
  private def parseFile(name: String): Seq[Seq[Int]] =
    val lines = Source.fromFile(name).getLines()
    // split lines by whitespace
    val clauseLines = lines.map(_.split("\\s+")).filterNot(_.length == 0).filterNot(s => s.head == "p" || s.head == "c")
    // convert to ints and throw the last zero away
    clauseLines.map(_.map(_.toInt).toSeq.init).toSeq
  
  private def intToClause(is: Seq[Int]): Clause = 
    val ls = is.map {
      i => 
        if i < 0 then
          Literal(Variable(s"x:${math.abs(i)}"), Negative)
        else
          Literal(Variable(s"x:$i"), Positive)
    }
    Clause(ls)

  def getCNFFromFile(name: String): CNF = 
    CNF(parseFile(name).map(intToClause(_)))

}