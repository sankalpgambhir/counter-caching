package countercaching

import BooleanStructure.* 
import scala.concurrent.duration.Duration
import scala.concurrent.duration.DurationInt

trait Verifier {
  val randomly = scala.util.Random()
  val fractionToCover = 0.5
  val minimizeProbability = 10
  val timeout = 20.seconds

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

  /**
    * Given a single formula as input, check whether it is unsat
    */
  def verify(f: Formula): Option[Boolean]

  val statistics: CacheStatistics

  val solver = SolverInterface()

  import solver.* 

  def memoize[I, O](f: I => O): I => O = new scala.collection.mutable.HashMap[I, O]() {
    override def apply(key: I) = getOrElseUpdate(key, f(key))
  }

  def formulaIsTrue(f: Formula, c: Iterable[Literal]): Boolean =
    val eval = evaluateFormula(f, c.toSet)
    eval.isDefined && eval.get

  def evaluateFormula(f: Formula, cx: Set[Literal]): Option[Boolean] = 
    memoize[(Formula, Set[Literal]), Option[Boolean]] { (f, cx) => evaluateFormulaMemo(f, cx) } ((f, cx))
    
  def evaluateFormulaMemo(f: Formula, cx: Set[Literal]): Option[Boolean] = 
    f match
      case l: Literal =>
        val r1 = cx.contains(l)
        val r2 = cx.contains(!l)
        if r1 then Some(true) else if r2 then Some(false) else Some(l.polarity)
      case And(children) => 
        val evals = children.map(evaluateFormula(_, cx))
        Some(evals.forall(e => e.get))
      case Or(children) => 
        val evals = children.map(evaluateFormula(_, cx))
        Some(evals.exists(e => e.get))
      case Not(inner) => evaluateFormula(inner, cx).map(!_)

  def evaluateFormula(f: CNF, cx: Set[Literal]): Option[Boolean] = 
    evaluateFormulaCNFMemo((f, cx))

  val evaluateFormulaCNFMemo = memoize[(CNF, Set[Literal]), Option[Boolean]] { (f, cx) =>
    def evaluateClause(cl: Clause, cx: Set[Literal]) =
      inline def evalLit(l: Literal): Boolean =
        val r1 = cx.contains(l)
        val r2 = cx.contains(!l)
        if r1 then true else if r2 then false else l.polarity
      cl.literals.exists(l => evalLit(l))
    
    Some(f.clauses.forall(evaluateClause(_, cx)))
  }
}

enum CacheAction:
  case Hit
  case Miss
  case Minimize(from: Int, to: Int)
class CacheStatistics {
  
  private var stats: Seq[CacheAction] = Seq.empty
  private var times: Seq[Long] = Seq.empty

  def hit = stats = stats :+ CacheAction.Hit
  def miss = stats = stats :+ CacheAction.Miss
  def minimize(from: Int, to: Int) = stats = stats :+ CacheAction.Minimize(from, to)

  def getStats = stats
  def getTime = times

  def timed[A](f: => A) = 
    val start = System.currentTimeMillis()
    val res = f
    val end = System.currentTimeMillis()
    this.synchronized {
      times = times :+ (System.currentTimeMillis() - start)
    }
    res
}
