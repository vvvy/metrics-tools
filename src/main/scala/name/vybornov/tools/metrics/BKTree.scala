package name.vybornov.tools.metrics

import scala.collection.mutable.Builder
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.ArrayBuffer
import scala.math.max
import scala.math.min

/**
 * Burchard-Keller tree implementation
 * 
 * @author vvv
 */
trait BKTree[K, V] {
  import scala.math.{ max, min }
  import scala.collection.generic.CanBuildFrom

  type Node = BKTree.Node[K, V]
  protected val root: Node

  val distance: Distance[K]

  private def range(accuracy: Int, d: Int)(n: Node) =
    (max(d - 1 - accuracy, 0) to min(d - 1 + accuracy, n.children.size - 1)).map { n.children(_) }.filter { _ != null }

  private def findStep(accuracy: Int, k: K)(n: Node): Option[((K, V), Int)] = {
    val d = distance(k, n.kv._1)
    if (d <= accuracy)
      Some((n.kv, d))
    else
      range(accuracy, d)(n).map(findStep(accuracy, k)).find(_.isDefined).flatten
  }

  private def searchStep[C](accuracy: Int, k: K, b: Builder[((K, V), Int), C])(n: Node): Unit = {
    val d = distance(k, n.kv._1)

    if (d <= accuracy)
      b += ((n.kv, d))

    range(accuracy, d)(n).foreach(searchStep(accuracy, k, b))
  }

  /**
   * find any
   */
  def find(accuracy: Int)(k: K): Option[((K, V), Int)] =
    findStep(accuracy, k)(root)

  /**
   * search for all
   */
  def search[C](accuracy: Int)(k: K, b: Builder[((K, V), Int), C]): Unit =
    searchStep(accuracy, k, b)(root)

  def search[C](accuracy: Int)(k: K)(implicit cbf: CanBuildFrom[_, ((K, V), Int), C]): C = {
    val b = cbf()
    searchStep(accuracy, k, b)(root)
    b.result
  }
}

object BKTree {

  class BKTreeBuilder[K: Distance, V] extends Builder[(K, V), BKTree[K, V]] {
    import scala.collection.mutable.ArrayBuffer
    private type Node = BKTree.Node[K, V]
    private var root: Node = null

    private val distance = implicitly[Distance[K]]

    private def node(k: K, v: V): Node =
      new Node((k, v), new ArrayBuffer[Node](0))

    private def eref(ab: ArrayBuffer[Node], offset: Int, onReuse: Node => Unit, onCreate: => Node) = {
      if (offset >= ab.size)
        ab.appendAll(Seq.fill[Node](offset + 1 - ab.size)(null))
      if (ab(offset) == null)
        ab(offset) = onCreate
      else
        onReuse(ab(offset))
    }

    private def addTo(n: Node)(k: K, v: V): Unit = {
      val ab = n.children.asInstanceOf[ArrayBuffer[Node]]
      val d = distance(k, n.kv._1)
      if (d == 0)
        throw new IllegalArgumentException("Duplicate key: " + k)
      eref(ab, d - 1, addTo(_)(k, v), node(k, v))
    }

    def update(k: K, v: V) =
      if (root == null)
        root = node(k, v)
      else
        addTo(root)(k, v)

    override def +=(kv: (K, V)) = {
      update(kv._1, kv._2)
      this
    }

    override def clear = {
      root = null
    }

    private def ckroot = if (root == null)
      throw new IllegalStateException("No nodes in a BKTree")
    else
      root

    override def result: BKTree[K, V] = new BKTree[K, V] {
      val distance = implicitly[Distance[K]]
      val root = ckroot
    }
  }

  protected[metrics] class Node[K, V](
    val kv: (K, V),
    val children: IndexedSeq[Node[K, V]])

  def newBuilder[K: Distance, V] =
    new BKTreeBuilder[K, V]

  def of[K: Distance, V](values: (K, V)*) = {
    val b = newBuilder[K, V]
    b ++= values
    b.result
  }
}
