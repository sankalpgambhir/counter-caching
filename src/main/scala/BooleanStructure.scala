package countercaching

object BooleanStructure {
  sealed trait Formula

  case class Variable(val name: String):
    override def toString(): String = s"[$name]"

  sealed trait Polarity:
    def unary_! : Polarity

  case object Positive extends Polarity:
    def unary_! = Negative
  case object Negative extends Polarity:
    def unary_! = Positive

  given Conversion[Polarity, Boolean] = p => 
    p match
      case Positive => true
      case Negative => false

  case class Literal(val v: Variable, val polarity: Polarity) extends Formula:
    override def toString(): String = s"${if polarity then "" else "!"}$v"
    def unary_! = Literal(v, !polarity)

  sealed trait Connector(val children: Seq[Formula]) extends Formula

  case class And(override val children: Seq[Formula]) extends Connector(children):
    override def toString(): String = s"And(${children.map(_.toString()).reduce(_ + ", " + _)})"

  case class Or(override val children: Seq[Formula]) extends Connector(children):
    override def toString(): String = s"Or(${children.map(_.toString()).reduce(_ + ", " + _)})"

  case class Not(val inner: Formula) extends Formula

  extension (f: Formula) {
    def toCNF: CNF = formulaToCNF(f)
    def toOLNF: Formula = normalizeOL(f)
  }

  def normalizeOL(f: Formula): Formula = ???

  sealed class CNF(val clauses: Seq[Clause])

  sealed class Clause(val literals: Seq[Literal])
  
  /**
    * Converts a formula to conjunctive normal form (CNF) using Tseitin's
    * transformation. *Does not preserve validity*. Prefer usage on UNSAT
    * formulae.
    *
    * @param f the formula
    * @return formula in CNF
    */
  def formulaToCNF(f: Formula): CNF = 
    // generally unique variables for Tseitin's transform
    class VariableGenerator(val name: String, init: Int = 0):
      var count = init
      def generate: Variable = 
        count += 1
        Variable(s"$name$count")

    val generator = VariableGenerator("__Internal_Tseitin_Var")

    // convert to negation normal form
    def nnf(f: Formula, polarity: Polarity = Positive): Formula = 
      f match
        case Literal(v, polarity) => if polarity then f else Literal(v, !polarity)
        case And(children) => 
          val childNNF = children.map(nnf(_, polarity))
          if polarity then And(childNNF) else Or(childNNF)
        case Or(children) =>
          val childNNF = children.map(nnf(_, polarity))
          if polarity then Or(childNNF) else And(childNNF)
        case Not(inner) => nnf(inner, !polarity)
      
    // collect all subformulae of a formula
    // works depth-first
    def getAllSubformulae(f: Formula): Seq[Formula] =
      f match
        case Literal(_, _) => Seq(f)
        case c: Connector => f +: c.children.flatMap(getAllSubformulae(_))
        case Not(inner) => f +: getAllSubformulae(inner)
      
    
    // perform Tseitin's transform
    // to get the formula in "CNF" using boolean connectives
    def tseitinTransform(f: Formula): Seq[Clause] = {
      // get all subformulae
      val sub = getAllSubformulae(f)
      // give them names
      val tVar = sub.map(s => s -> generator.generate).toMap
      // guarantee: this map is total on the subformulae of f

      object TseitinOnNotInNNFException extends Exception

      // traverse the tree, and add the requisite rename equivalence clauses
      // assumes formula is in NNF, throws exception if not true
      def foldOverFormula(f: Formula): Seq[Clause] = {
        f match
          case l@Literal(v, polarity) => 
            val x = Literal(tVar(l), Positive)
            // x <- l /\ x -> l
            Seq(Clause(Seq(!x, l)), Clause(Seq(x, !l)))
          case And(children) => 
            val thisFormula = Literal(tVar(f), Positive)
            // we need to generate two types of clauses
            // x -> (a /\ b /\ ...) = (!x \/ a) /\ (!x \/ b) /\ ...
            val fwd = children.map(c => Clause(Seq(!thisFormula, Literal(tVar(c), Positive))))
            // and
            // x <- (a /\ b /\ ...) = (x \/ a \/ b \/ ...)
            val bwd = Seq(Clause(thisFormula +: children.map(c => Literal(tVar(c), Positive))))
            fwd ++ bwd
          case Or(children) =>
            val thisFormula = Literal(tVar(f), Positive)
            // we need to generate two types of clauses
            // x -> (a \/ b \/ ...) = (!x \/ a \/ b \/ ...)
            val fwd = Seq(Clause(thisFormula +: children.map(c => Literal(tVar(c), Positive))))
            // and
            // x <- (a \/ b \/ ...) = (x \/ !a) /\ (x \/ !b) /\ ...
            val bwd = children.map(c => Clause(Seq(thisFormula, Literal(tVar(c), Negative))))
            fwd ++ bwd
          case Not(inner) => 
            // cannot happen, by assumption
            throw TseitinOnNotInNNFException
        }
        
        foldOverFormula(f)
      }

      CNF(tseitinTransform(nnf(f)))

  def CNFToFormula(f: CNF): Formula = And(f.clauses.map(c => Or(c.literals)))

  extension (f: CNF) {
    def toFormula: Formula = CNFToFormula(f)
  }
}