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
class BrainlessNoCache extends Verifier {


  val statistics: CacheStatistics = CacheStatistics()

  def log(s: String) = println(s"[LOG] $s")

  def verify(f: Formula): Option[Boolean] = statistics.timed {
      val res = uncachedVerify(f)
      statistics.miss
      res
  }
  
  def uncachedVerify(f: Formula): Option[Boolean] =
    val res = solver.checkFormulaInvalidity(f)
    res.map(_._1)
}
