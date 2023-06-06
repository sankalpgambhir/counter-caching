package countercaching

import countercaching.BooleanStructure.Formula

class VerifierTests {
  def gen(numVars: Int, depth: Int, count: Int): Seq[Formula] =
    val gen = FormulaGeneration(numVars)
    // generate random formula stream
    (1 to count).map(i => gen.generateRandomFormula(depth))

  def testVerifier(v: Verifier, numVars: Int, depth: Int, count: Int): CacheStatistics =
    val gen = FormulaGeneration(numVars)
    // generate random formula stream
    val forms = (1 to count).map(i => gen.generateRandomFormula(depth))
    forms.foreach(v.verify(_))
    v.statistics

  def testStatic(v: Verifier, numVars: Int, depth: Int, count: Int): CacheStatistics = 
    testVerifier(v, numVars, depth, count) // build up a cache
    testVerifier(v, numVars, depth, count) // actual run

  def testOnline(v: Verifier, numVars: Int, depth: Int, count: Int): CacheStatistics = 
    testVerifier(v, numVars, depth, count)

  def statsToCSV(s: CacheStatistics): String = 
    val stats = (s.getStats.zipWithIndex zip s.getTime).map{case ((c, i), t) => s"$i, $c, $t"}
    "No, Event, Time\n" + stats.reduce(_ + "\n" + _)

}
