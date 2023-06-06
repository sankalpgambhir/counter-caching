package countercaching

import z3.scala.*
import collection.parallel.CollectionConverters.ImmutableSeqIsParallelizable
import java.io.File
import java.io.BufferedWriter
import java.io.FileWriter

object Main {

  val fg = FormulaGeneration()

  val f1 = (1 to 2).map(i => fg.generateRandomFormula(5))

  val p = PainlessLeafFractional()

  // f1.map(p.verify(_))

  val f = f1.head

  val t = VerifierTests()

  println(t.statsToCSV(t.testOnline(p, 15, 8, 50)))

  // val c = DIMACSInterface.getCNFFromFile("sat-comp-bench/extracted/fff3f8c76467cdf9d92689969fd94281-mod2c-rand3bip-sat-240-2.shuffled-as.sat05-2519.cnf")
  // val df = c.toFormula
  // // val f = f1.head

  // val dim = DIMACSInterface

  // val formsRaw = scala.io.Source.fromFile("anniSortedFormulaList").getLines().map(l => s"sat-anni-bench/extracted/$l")

  // val forms = formsRaw.map(dim.getCNFFromFile(_).toFormula)

  // val verifiers: Seq[Verifier] = Seq(
  //   BrainlessNoCache(),
  //   Brainless(),
  //   PainlessLeaf(),
  //   PainlessLeafFractional(),
  //   PainlessPath(),
  //   PainlessPathFractional()
  // )

  // val testSet = forms.take(1).toSeq

  // def SATBenchPar = 
  //   val vs = verifiers.zipWithIndex
  //   val ts = testSet.zipWithIndex
  //   val vts = vs.map(v => (v, ts))
  //   vts.par.map {
  //     case ((v, i), ts) => 
  //       ts.map { (t, j) =>
  //         println(s"$i start $j")
  //         v.verify(t)
  //       }
  //   }

  // def SATBenchSeq = 
  //   verifiers.zipWithIndex.map{(v, j) =>
  //     testSet.zipWithIndex.map{ (t, i) =>
  //       println(s"$j start $i")
  //       v.verify(t)
  //     }
  //   }

  // val start = System.currentTimeMillis()
  // val res = SATBenchPar
  // val end = System.currentTimeMillis()

  // writeFile("resFile", res.toString(), false)

  // /**
  //  * write a `String` to the `filename`.
  //  */
  // def writeFile(filename: String, s: String, app: Boolean = true): Unit = {
  //     val file = new File(filename)
  //     val bw = new BufferedWriter(new FileWriter(file, app))
  //     bw.write("\n-------------\n" + s)
  //     bw.close()
  // }

  // writeFile("outFile", "#################")

  // verifiers.map(_.statistics).map(t.statsToCSV(_)).foreach(writeFile("outFile", _))

  // println(s"${end - start} ms")
}

object ParTest {

  val y0 = 5
  var y1 = 5
  @volatile var y2 = 5
  inline val y3 = 5

  inline def f(inline x: Int) =
    Seq(1).par.foreach( i =>
      println(x)
    )

  // def f0 = 
  //   (1 to 10).par.foreach( i =>
  //     println(y0)
  //   )
  // def f1 = 
  //   (1 to 10).par.foreach( i =>
  //     println(y1)
  //   )
  // def f2 = 
  //   (1 to 10).par.foreach( i =>
  //     println(y2)
  //   )
  // def f3 = 
  //   (1 to 10).par.foreach( i =>
  //     println(y3)
  //   )

  f(y3)

}

object RandomTests {

  val verifiers: Seq[Verifier] = Seq(
    // BrainlessNoCache(),
    // BrainlessNoCache(), // 0
    // Brainless(), // 1
    // PainlessLeaf(), // 2
    // PainlessLeafFractional(), // 3
    PainlessPath(), // 4
    // PainlessPathFractional() // 5
  )

  val t = VerifierTests()

  val forms = t.gen(15, 8, 5000)

  val testSet = forms

  def SATBenchPar = 
    val vs = verifiers.zipWithIndex
    val ts = testSet.zipWithIndex
    val vts = vs.map(v => (v, ts))
    vts.par.map {
      case ((v, i), ts) => 
        ts.map { (t, j) =>
          if j % 1000 == 0 then println(s"$i start $j")
          v.verify(t)
        }
    }

  def SATBenchSeq = 
    verifiers.zipWithIndex.map{(v, j) =>
      val start = System.currentTimeMillis()
      val res = testSet.zipWithIndex.map{ (t, i) =>
        if i % 1000 == 0 then println(s"$j start $i")
        v.verify(t)
      }
      val end = System.currentTimeMillis()
      println(s"$j: ${end - start} ms")
      res
    }

  @main def staticTest = 
    val p = PainlessLeafFractional()
    val res = testSet.zipWithIndex.map{ (t, i) =>
        if i % 100 == 0 then println(s"start $i")
        p.verify(t)
      }
    val s = p.statistics
    val stats = (s.getStats.zipWithIndex zip s.getTime).filter(_._1._1.isInstanceOf[CacheAction.Minimize]).map{case ((c, i), t) => s"$i, $c, $t"}
    println("No, Event, Time\n" + stats.reduce(_ + "\n" + _))

  @main def run = {
    val start = System.currentTimeMillis()
    val res = SATBenchSeq
    val end = System.currentTimeMillis()

    val c = "4T"

    writeFile(s"resFile$c", res.toString(), false)

    /**
     * write a `String` to the `filename`.
     */
    def writeFile(filename: String, s: String, app: Boolean = true): Unit = {
        val file = new File(filename)
        val bw = new BufferedWriter(new FileWriter(file, app))
        bw.write("\n-------------\n" + s)
        bw.close()
    }

    writeFile(s"outFile$c", "#################")

    verifiers.map(_.statistics).map(t.statsToCSV(_)).foreach(writeFile(s"outFile$c", _))

    println(s"${end - start} ms")
  }
}

object SATCompTests {

  val verifiers: Seq[Verifier] = Seq(
    // BrainlessNoCache(),
    // BrainlessNoCache(), // 0
    // Brainless(), // 1
    // PainlessLeaf(), // 2
    // PainlessLeafFractional(), // 3
    PainlessPath(), // 4
    // PainlessPathFractional() // 5
  )

  val t = VerifierTests()
  val dim = DIMACSInterface

  val formsRaw = scala.io.Source.fromFile("anniSortedFormulaList").getLines().map(l => s"sat-anni-bench/extracted/$l")

  val forms = formsRaw.map(dim.getCNFFromFile(_).toFormula)

  val testSet = forms.take(500).toList

  def SATBenchPar = 
    val vs = verifiers.zipWithIndex
    val ts = testSet.zipWithIndex
    val vts = vs.map(v => (v, ts))
    vts.par.map {
      case ((v, i), ts) => 
        ts.map { (t, j) =>
          println(s"$i start $j")
          v.verify(t)
        }
    }

  def SATBenchSeq = 
    verifiers.zipWithIndex.map{(v, j) =>
      val start = System.currentTimeMillis()
      val res = testSet.zipWithIndex.map{ (t, i) =>
        println(s"$j start $i")
        v.verify(t)
      }
      val end = System.currentTimeMillis()
      println(s"$j: ${end - start} ms")
      res
    }

  @main def satComp = {
    val start = System.currentTimeMillis()
    val res = SATBenchSeq
    val end = System.currentTimeMillis()

    val c = "satComp4T1"

    writeFile(s"resFile$c", res.toString(), false)

    /**
     * write a `String` to the `filename`.
     */
    def writeFile(filename: String, s: String, app: Boolean = true): Unit = {
        val file = new File(filename)
        val bw = new BufferedWriter(new FileWriter(file, app))
        bw.write("\n-------------\n" + s)
        bw.close()
    }

    writeFile(s"outFile$c", "", false)

    verifiers.map(_.statistics).map(t.statsToCSV(_)).foreach(writeFile(s"outFile$c", _))

    println(s"${end - start} ms")
  }
}

object NewMain extends App {
  RandomTests.run
}

