package countercaching

import BooleanStructure.* 
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.duration.DurationInt
import java.util.concurrent.TimeoutException
import java.text.Normalizer.Form
import scala.util.Random

/**
  * The Painless Verification Project
  * 
  * The world's best and most powerful verifier
  */
class PainlessPath extends Verifier {

  // caching counterexamples
  var cache: DAG[Literal] = DAG.empty
  var formCache: Set[CNF] = Set.empty

  val statistics: CacheStatistics = CacheStatistics()

  def log(s: String) = println(s"[LOG] $s")

  def verify(f: Formula): Option[Boolean] = statistics.timed {
    if cached(f) then
      statistics.hit
      Some(true)
    else 
      val res = uncachedVerify(f)
      statistics.miss
      res
  }
  
  def uncachedVerify(f: Formula): Option[Boolean] =
    val res = solver.checkFormulaInvalidity(f)
    
    if res.isEmpty || res.get._1 then
      res.map(_._1)
    else
      // a counterexample was found, cache it
      val cex = res.get._2.get // guaranteed to be non-empty unless solver failed to find cex but found SAT (??)
      formCache = formCache + f.toCNF

      cache = minimizeCache(cache + cex)

      res.map(_._1)

  def minimizeCache(cache: DAG[Literal]): DAG[Literal] = statistics.timed {
    if randomly.between(0, 100) < minimizeProbability then
      val res = minimizeCache_(cache)
      statistics.minimize(cache.size, res.size)
      res
    else
      cache
  }

  /**
    * Simple greedy set cover
    *
    * @param cache
    * @return cache minimized wrt greedy set cover
    */
  def minimizeCache_(cache: DAG[Literal]): DAG[Literal] =
    val cexs = cache.pathStream.toList
    // ugly
    // basically to each counterexample assign the formulae it covers
    // and then compute and store the lengths for a quick and dirty check later
    val covers = cexs.map(c => 
                  c.toList -> formCache.filter{f => 
                    val e = evaluateFormula(f, c.toSet)
                    e.isDefined && e.get
                  }
                  ).map((c, f) => (c, f.size/c.length, f.size, f))
    val sortedCovers = covers.sortBy(_._2).reverse // sorted by negative size, largest first
    
    val target = math.ceil(1 * formCache.size) // for checking whether we are done somewhat quickly

    sortedCovers.foldLeft((DAG.empty[Literal], 0, Seq.empty[Set[CNF]])) {
      case ((curr, approxCount, formSets), (cex, _, count, forms)) => 
        if approxCount < target then
          // definitely under
          (curr + cex, approxCount + count, formSets :+ forms)
        else if formSets.head.size < target then
          // still need to add
          (curr + cex, approxCount + count, Seq(formSets.head ++ forms))
        else
          // done
          (curr, approxCount, formSets)
    }._1
  
  def cached(f: Formula): Boolean = 
    val cexs = cache.leafStream
    cexs.exists(c => formulaIsTrue(f, c))

}
