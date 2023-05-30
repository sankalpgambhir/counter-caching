package countercaching

import BooleanStructure.* 
import SolverInterface.*

trait Verifier {
  /**
    * Given a single formula as input, check whether it is unsat
    */
    def verify(f: Formula): Option[Boolean]
}
