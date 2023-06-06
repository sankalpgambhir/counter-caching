package countercaching

import BooleanStructure.*
import scala.util.Random

class FormulaGeneration(val numLiterals: Int = 10, val baseName: String = "x") {
    val randomly = Random(5)

    extension (r: Random) {
        /**
          * Generates a new random literal with the base name of the class, and
          * chooses a number below the number of maximum literals.
          *
          * I just like the unified interface with other random generators.
          */
        def nextLiteral(): Literal = 
            val num: Int = r.between(0, numLiterals)
            Literal(Variable(s"$baseName:$num"), if r.nextBoolean then Positive else Negative)
    }

    def generateRandomFormula(depth: Int): Formula = 
        if depth <= 1 then
            // generate a literal
            randomly.nextLiteral()
        else 
            // generate a connector
            val children = (0 to 1).map(i => generateRandomFormula(depth - 1))
            if randomly.nextBoolean() then
                And(children)
            else 
                Or(children)
}
