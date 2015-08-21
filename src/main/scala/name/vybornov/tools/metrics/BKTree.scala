package name.vybornov.tools.metrics

import scala.collection.mutable.Builder
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.ArrayBuffer
import scala.math.max
import scala.math.min

/**
 * Burkhard-Keller tree implementation
 * 
 * @author vvv
 */
trait BKTree[K] extends Serializable {
  import scala.math.{ max, min }
  import scala.collection.generic.CanBuildFrom

  type Node = BKTree.Node[K]
  protected val root: Node

  val distance: Distance[K]

  private def range(accuracy: Int, d: Int)(n: Node) =
    n.children.view(max(d - 1 - accuracy, 0), min(d /*- 1*/ + accuracy, n.children.size)).filter { _ != null }

  private def findStep(accuracy: Int, k: K)(n: Node): Option[(K, Int)] = {
    val d = distance(k, n.k)
    if (d <= accuracy)
      Some((n.k, d))
    else
      range(accuracy, d)(n).map(findStep(accuracy, k)).find(_.isDefined).flatten
  }

  private def searchStep[C](accuracy: Int, k: K, b: Builder[(K, Int), C])(n: Node): Unit = {
    val d = distance(k, n.k)

    if (d <= accuracy)
      b += ((n.k, d))

    range(accuracy, d)(n).foreach(searchStep(accuracy, k, b))
  }

  /**
   * find any
   */
  def find(accuracy: Int)(k: K): Option[(K, Int)] =
    findStep(accuracy, k)(root)

  /**
   * search for all, write output into b
   */
  def search[C](accuracy: Int, k: K, b: Builder[(K, Int), C]): Unit =
    searchStep(accuracy, k, b)(root)

  /**
   * search for all, return output as builder
   */
  def search[C](accuracy: Int)(k: K)(implicit cbf: CanBuildFrom[_, (K, Int), C]): C = {
    val b = cbf()
    searchStep(accuracy, k, b)(root)
    b.result
  }
}

object BKTree {

  class BKTreeBuilder[K: Distance] extends Builder[K, BKTree[K]] {
    import scala.collection.mutable.ArrayBuffer
    private type Node = BKTree.Node[K]
    private var root: Node = null

    private val distance = implicitly[Distance[K]]

    private def node(k: K): Node =
      new Node(k, new ArrayBuffer[Node](0))

    private def eref(ab: ArrayBuffer[Node], offset: Int, onReuse: Node => Unit, onCreate: => Node) = {
      if (offset >= ab.size)
        ab.appendAll(Seq.fill[Node](offset + 1 - ab.size)(null))
      if (ab(offset) == null)
        ab(offset) = onCreate
      else
        onReuse(ab(offset))
    }

    private def addTo(n: Node)(k: K): Unit = {
      val ab = n.children.asInstanceOf[ArrayBuffer[Node]]
      val d = distance(k, n.k)
      if (d == 0)
        throw new IllegalArgumentException("Duplicate key: " + k)
      eref(ab, d - 1, addTo(_)(k), node(k))
    }

    def update(k: K) =
      if (root == null)
        root = node(k)
      else
        addTo(root)(k)

    override def +=(k: K) = {
      update(k)
      this
    }

    override def clear = {
      root = null
    }

    private def ckroot = if (root == null)
      throw new IllegalStateException("No nodes in a BKTree")
    else
      root

    override def result: BKTree[K] = new BKTree[K] {
      val distance = implicitly[Distance[K]]
      val root = ckroot
    }
  }

  protected[metrics] class Node[K](
    val k: K,
    val children: IndexedSeq[Node[K]]
    ) extends Serializable

  def newBuilder[K: Distance] =
    new BKTreeBuilder[K]

  def of[K: Distance](values: K*) = {
    val b = newBuilder[K]
    b ++= values
    b.result
  }
}
