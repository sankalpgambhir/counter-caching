package countercaching

import scala.collection.mutable
import scala.util.Try
import scala.util.Failure
import scala.util.Success
import z3.scala.dsl.Or

/**
  * Provides a directed acyclic graph interface
  *
  * @param elems the vertices of the graph
  * @param root  the roots or initial elements of the DAG 
  * @param edges map storing the outgoing edges for each vertex
  */
class DAG[A : Ordering](val elems: Set[A], val roots: Set[A], val edges: Map[A, Set[A]]):
  /**
    * Generates a stream of breadth-first paths starting at the roots of this DAG
    *
    * @return stream of paths starting at each root
    */
  def pathStream: LazyList[Seq[A]] = 
    // Queue to store nodes to be visited
    val queue: mutable.Queue[(A, List[A])] = mutable.Queue(roots.toSeq.map(r => r -> List(r)): _*)
    
    // LazyList to store breadth-first paths
    def paths: LazyList[List[A]] = Try(queue.dequeue()) match {
      case Success((current, path)) =>
        val neighbours = edges.getOrElse(current, Set.empty)
        val newPaths = neighbours.map(neighbour => (neighbour, path :+ neighbour))
        queue.enqueueAll(newPaths)
        path #:: paths
      case Failure(e : NoSuchElementException) => // dequeueing finished
        LazyList.empty[List[A]]
      case Failure(exception) => throw exception
    }

    paths

  /**
    * New DAG with a path added
    *
    * @param path
    * @return updated DAG
    */
  def withPath(path: List[A]): DAG[A] = 
    val sortedPath = path.sorted
    val newElems = elems ++ sortedPath
    val newEdges = edgesWithPath(sortedPath)
    val newRoots = sortedPath.headOption.filterNot(h => roots.contains(h)).map(roots + _).getOrElse(roots)
    DAG(newElems, newRoots, newEdges)

  private def edgesWithPath(path: List[A]): Map[A, Set[A]] =
    path match
      case head :: next => 
          next.foldLeft((edges, head)) {
            case ((map, prev), curr) => (appendAtKey(map, prev, curr), curr)
          }._1
      case Nil => edges

  // Aliases

  infix def + (path: List[A]) = this.withPath(path)
    
  // Helpers

  /**
    * Append a value to the set corresponding to a key in a Map to Sets
    *
    * @param m the map
    * @param k the key
    * @param v the new value
    * @return updated map
    */
  private def appendAtKey[K, V](m: Map[K, Set[V]], k: K, v: V): Map[K, Set[V]] =
    m.updatedWith(k) {
      case Some(vs) => Some(vs + v)
      case None => Some(Set(v))
    }

object DAG:
  def empty[A : Ordering] = DAG[A](Set.empty, Set.empty, Map.empty)
