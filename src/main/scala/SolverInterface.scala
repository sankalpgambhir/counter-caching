package countercaching

import z3.scala.*
import BooleanStructure.*
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object SolverInterface {
  val z3 = new Z3Context("MODEL" -> true)

  class UnknownSatisfiabilityException(val formula: Formula) extends Exception

  /**
    * Convert a Boolean formula to a Z3 AST
    *
    * @param f the formula
    * @return the Z3 AST
    */
  def formulaToZ3AST(f: Formula): Z3AST =
    f match
      case Literal(v, polarity) => if polarity then z3.mkBoolConst(v.name) else z3.mkNot(z3.mkBoolConst(v.name))
      case And(children) => z3.mkAnd(children.map(formulaToZ3AST(_)): _*)
      case Or(children) => z3.mkOr(children.map(formulaToZ3AST(_)): _*)
      case Not(inner) => z3.mkNot(formulaToZ3AST(inner))

  /**
    * Check if a formula is valid by checking for the satisfiability of its negation
    *
    * @param f the formula
    * @return a Future containing a pair, an optional Boolean about validity, and if not valid, a list of variable assignments
    */
  def checkFormulaValidity(f: Formula): Future[(Boolean, Option[List[Literal]])] =
    def modelToCounterexample(m: Z3Model): List[Literal] =
      // get the interpretation of each constant from the model
      val consts = m.getConstInterpretations.toList
      val t = z3.mkTrue()
      val f = z3.mkFalse()
      consts.collect {
        case (fn, v) if z3.isEqAST(v, t) => Literal(Variable(fn.getName.toString()), Positive)
        case (fn, v) if z3.isEqAST(v, f) => Literal(Variable(fn.getName.toString()), Negative)
        // otherwise it does not have an assigned truth value
      }

    // generate a solver locally
    val solver = z3.mkSolver()
    solver.assertCnstr(z3.mkNot(f.toZ3AST))
    
    def getRes =
      val res = solver.check()
      // if SAT, get the model for it
      val model: Option[Z3Model] = res.filter(identity).map(x => solver.getModel())

      // we force the evaluation of the Option res
      // so we can filter to a SAT or UNSAT res, otherwise throw an exception
      if res.isEmpty then
        throw UnknownSatisfiabilityException(f)
      else
        (res.map(!_).get, model.map(modelToCounterexample(_)))
    
    Future(getRes)

  extension (f: Formula) {
    def isValid: Future[(Boolean, Option[List[Literal]])] = checkFormulaValidity(f)
    def toZ3AST = formulaToZ3AST(f)
  }

  extension (f: CNF) {
    def isValid: Future[(Boolean, Option[List[Literal]])] = f.toFormula.isValid
  }
}

