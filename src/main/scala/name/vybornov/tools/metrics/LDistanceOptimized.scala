package name.vybornov.tools.metrics

/**
 * Levenshtein (edit) distance
 *
 * Minimal number of edits (insert, delete, substitute), or cost, needed to make
 * from one word the other.
 *
 * Properties needed to define L-matrix:
 *
 * Let
 * CONV(A, B) is a conversion of A to B
 * Aa is concatenation of word A and char a
 * DEL is deletion of last char in A, i.e. DEL(Aa) = A
 * INS<a> is addition of a, ie. INS<a>(A) = Aa
 *
 * Then two properties hold
 * CONV(Aa, Bb) = DEL * CONV(A, Bb)
 * CONV(Aa, Bb) = CONV(Aa, B) * Ins<a>
 *
 * Each cell (i, j) of the matrix contains distance between i- and j-long prefixes of
 * original and target words.
 * Matrix calculation formula (rows constitute original word, columns do target):
 *  m(i, 0) = i * deletionCost  //i-char => empty costs i deletions
 *  m(0, j) = j * insertionCost //empty => j-char costs j insertions
 *  m(i, j) = min { // i-char prefix to j-char prefix is a minimum of
 *   m(i - 1, j) + deleteCost // one deletion plus (i - 1)-char prefix => j-char prefix
 *   m(i, j - 1) + insertCost // i-char prefix to (j - 1)-char prefix plus one insertion
 *   m(i - 1, j- 1) + delta(o(i) == t(j)) * substituteCost //i-1 to j-1 plus 0 or one substitutions
 *  }
 *
 *  @author vvv
 */
class LDistanceOptimized[T] extends Distance[IndexedSeq[T]] {
  val insertCost = 1
  val deleteCost = 1
  val substituteCost = 1

  /**
   *  To iterate efficiently (memory consumption O(max(m,n)) rather than O(m*n)),
   *  we introduce new coordinate system (k, j), where k is defined as
   *
   *  k = i + j
   *
   *  i = k - j
   *
   *  0 <= i <= imax
   *  0 <= j <= jmax
   *
   *  k runs in [0, imax + jmax]
   *  0 <= k - j <= imax; so -k <= -j <= imax - k and k - imax <= j <= k
   *
   *  j runs in [max(0, k - imax), min(k, jmax)]
   *
   *  (i, j - 1) => (k - 1, j - 1)
   *  (i - 1, j) => (k - 1, j)
   *  (i - 1, j - 1) => (k - 2, j - 1)
   *
   */
  def apply(o: IndexedSeq[T], t: IndexedSeq[T]) = {
    import scala.collection.mutable.ArrayBuffer
    import scala.math.{ min, max }

    val (imax, jmax) = (o.length, t.length)

    (imax + jmax) match {
      case 0 => 0
      case 1 => 1
      case ijmax @ _ =>
        val a0 = ArrayBuffer(0)
        val a11 = ArrayBuffer(1, 1)
        def a = ArrayBuffer.fill(jmax + 1)(0)

        (2 to ijmax).foldLeft((a11, a0)) {
          (c, k) =>
            val at = a
            (max(0, (k - imax)) to min(k, jmax)) map {
              j =>
                val i = k - j
                def ci = c._1(j - 1) + insertCost
                def cd = c._1(j) + deleteCost
                def cs = c._2(j - 1) + (if (o(i - 1) == t(j - 1)) 0 else substituteCost)

                at(j) = (i, j) match {
                  case (0, j) => ci
                  case (i, 0) => cd
                  case _ => min(ci, min(cd, cs))
                }
            }
            (at, c._1)
        }._1(jmax)
    }
  }
}

object LDistanceOptimized { 
  
  object genDistance extends LDistance
  object charDistance extends LDistance[Char] 
  
  implicit def distance[T <: IndexedSeq[Char]]: Distance[T] = new Distance[T] {
    def apply(e1: T, e2: T) = charDistance(e1, e2)
  }

  implicit object stringDistance extends Distance[String] {
    def apply(e1: String, e2: String) = charDistance(e1, e2)
  }
  def apply(e1: String, e2: String) = stringDistance(e1, e2)
  def apply[T <: Iterable[Char]](e1: T, e2: T) = charDistance(e1, e2)
}

