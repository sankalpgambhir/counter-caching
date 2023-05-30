package countercaching

import BooleanStructure.* 
import SolverInterface.*
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
class Painless extends Verifier {

  val timeout = 500.milliseconds
  val randomly = Random()

  // order literals for 
  given Ordering[Polarity] = (p1, p2) =>
    (p1, p2) match
      case (Positive, Negative) =>  1
      case (Negative, Positive) => -1
      case _ =>  0
    
  given Ordering[Literal] = (l1, l2) =>
    val stringOrd = summon[Ordering[String]]
    val polarityOrd = summon[Ordering[Polarity]]
    
    val nameComp = stringOrd.compare(l1.v.name, l2.v.name)

    if nameComp == 0 then
      polarityOrd.compare(l1.polarity, l2.polarity)
    else 
      nameComp

  // caching counterexamples
  var cache: DAG[Literal] = DAG.empty
  var formCache: Set[CNF] = Set.empty

  def log(s: String) = println(s"[LOG] $s")

  def verify(f: Formula): Option[Boolean] = 
    if cached(f) then
      Some(true)
    else uncachedVerify(f)
  
  def uncachedVerify(f: Formula): Option[Boolean] =
    val future = f.isInvalid
    val res =  
      try 
        Some(Await.result(future, timeout))
      catch
        case e: TimeoutException => log(s"Verification timed out for $f"); None
        case e => throw e
    
    if res.isEmpty || res.get._1 then
      res.map(_._1)
    else
      // a counterexample was found, cache it
      val cex = res.get._2.get // guaranteed to be non-empty unless solver failed to find cex but found SAT (??)
      formCache = formCache + f.toCNF

      // TODO: minimize cache?
      // how to decide when to do it?
      // currently just returns
      cache = minimizeCache(cache + cex)

      res.map(_._1)

  def minimizeCache(cache: DAG[Literal]): DAG[Literal] = 
    if randomly.between(0, 100) < 1 then // 1%
      minimizeCache_(cache)
    else
      cache

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
                  ).map((c, f) => (c, f.size, f))
    val sortedCovers = covers.sortBy(-_._2) // sorted by negative size, largest first
    
    val target = formCache.size // for checking whether we are done somewhat quickly

    sortedCovers.foldLeft((DAG.empty[Literal], 0, Seq.empty[Set[CNF]])) {
      case ((curr, approxCount, formSets), (cex, count, forms)) => 
        if approxCount + count < target then
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
    val cexs = cache.pathStream
    cexs.exists(c => formulaIsTrue(f, c))

  def formulaIsTrue(f: Formula, c: Iterable[Literal]): Boolean =
    val eval = evaluateFormula(f, c.toSet)
    eval.isDefined && eval.get

  def evaluateFormula(f: Formula, cx: Set[Literal]): Option[Boolean] = 
    f match
      case l: Literal =>
        if cx.contains(l) then 
          Some(true)
        else if cx.contains(!l) then
          Some(false)
        else 
          None
      case And(children) => 
        val evals = children.map(evaluateFormula(_, cx))

        if evals.exists(e => e.isDefined && !e.get) then
          Some(false)
        else if evals.exists(_.isEmpty) then
          None
        else
          Some(true)
      case Or(children) => 
        val evals = children.map(evaluateFormula(_, cx))

        if evals.exists(e => e.isDefined && e.get) then
          Some(true)
        else if evals.exists(_.isEmpty) then
          None
        else
          Some(false)
      case Not(inner) => evaluateFormula(inner, cx).map(!_)

  def evaluateFormula(f: CNF, cx: Set[Literal]): Option[Boolean] = 
    def evaluateClause(cl: Clause, cx: Set[Literal]): Option[Boolean] =
      val es = cl.literals.map(l => (cx.contains(l), cx.contains(!l)))
      val (pes, nes) = es.unzip

      if pes.exists(identity) then
        Some(true)
      else if nes.forall(identity) then
        Some(false)
      else
        None
    
    val clauseRes = f.clauses.map(evaluateClause(_, cx))

    if clauseRes.exists(e => e.isDefined && !e.get) then
      Some(false)
    else if clauseRes.exists(_.isEmpty) then
      None
    else
      Some(true)

}
