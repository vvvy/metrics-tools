package name.vybornov.tools.metrics

import scala.collection.mutable.Builder
import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom

/**
 * @author vvv
 */
trait CompactBKTree[K] extends Serializable {
  
  val distance: Distance[K]
  val root: K
  val g: CompactBKTree.Tree[K]
  
  
  class SearchContext(k: K, delta: Int) {
    def findStep(kcur: K): Option[(K, Int)] = {
      val d = distance(k, kcur)
      
      if(d <= delta) 
        Some(kcur, d)
      else {
        val cres = (((d - delta) max 0) to (d + delta)).map(g.get(kcur, _))
        cres.collect { case Some(knew) => findStep(knew) }.find(_.isDefined).flatten
      }
    }
    
    def searchStep[C](kcur: K, builder: Builder[(K, Int), C]): Unit = {
      val d = distance(k, kcur)
      
      if(d <= delta) 
        builder += ((kcur, d))
      else {
        val cres = (((d - delta) max 0) to (d + delta)).map(g.get(kcur, _))
        cres.collect { 
          case Some(knew) => knew 
        }.foreach(searchStep(_, builder))
      }
    }
  }
  
  /**
   * find any
   */
  def find(accuracy: Int)(k: K): Option[(K, Int)] =
    new SearchContext(k, accuracy).findStep(root)

  /**
   * search for all, write output into b
   */
  def search[C](accuracy: Int, k: K, b: Builder[(K, Int), C]): Unit =
    new SearchContext(k, accuracy).searchStep(root, b)

  /**
   * search for all, return output as builder
   */
  def search[C](accuracy: Int)(k: K)(implicit cbf: CanBuildFrom[_, (K, Int), C]): C = {
    val b = cbf()
    new SearchContext(k, accuracy).searchStep(root, b)
    b.result
  }
  
}

object CompactBKTree {
  
  type Tree[K] = scala.collection.mutable.HashMap[(K, Int), K] //(key, dist) -> nextKey
  
  class BKTreeBuilder[K: Distance](maxLevels: Int = Int.MaxValue) extends Builder[K, CompactBKTree[K]] {
    builder =>
    
    val distance = implicitly[Distance[K]]
    
    var root: Option[K] = None
    val g = new Tree[K]() 
    
    //returns depth
    @tailrec
    private def step(k: K, kcur: K, depth: Int): Unit = {
      val dist = distance(k, kcur)
      
      if(dist > 0) 
        g.get(kcur, dist) match {
          case Some(knew) => 
            if(depth < maxLevels) 
              step(k, knew, depth + 1)
          case None => 
            g((kcur, dist)) = k
        }
      else
        throw new IllegalArgumentException(s"Duplicate key `$k`")
    }
    
    def update(k: K) = root match {
      case Some(r) => step(k, r, 1)
      case None => root = Some(k)
    }

        
    override def +=(k: K) = {
      update(k)
      this
    }

    override def clear = {
      root = None
      g.clear()
    }

    override def result: CompactBKTree[K] = new CompactBKTree[K] {
      if (!builder.root.isDefined)
        throw new IllegalStateException("No nodes in a BKTree")
      val distance = implicitly[Distance[K]]
      val root = builder.root.get
      val g = builder.g
    }
  }
  
  def newBuilder[K: Distance] =
    new BKTreeBuilder[K]

  def of[K: Distance](values: K*) = {
    val b = newBuilder[K]
    b ++= values
    b.result
  }
  
}