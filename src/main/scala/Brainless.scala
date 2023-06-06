package countercaching

import BooleanStructure.* 
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.duration.DurationInt
import java.util.concurrent.TimeoutException
import java.text.Normalizer.Form
import scala.util.Random

/**
  * The Brainless Verification Project
  * 
  * The world's worst and least powerful verifier
  */
class Brainless extends Verifier {
  // caching counterexamples
  var formCache: Set[Formula] = Set.empty

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
      formCache = formCache + f
      res.map(_._1)

  def cached(f: Formula): Boolean = formCache.contains(f)
}
