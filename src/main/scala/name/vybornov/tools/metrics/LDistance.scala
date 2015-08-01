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
class LDistance[T] extends Distance[Iterable[T]] {
  val insertCost = 1
  val deleteCost = 1
  val substituteCost = 1

  /**
   * Idea taken from https://gist.github.com/tixxit/1246894
   */
  def apply(o: Iterable[T], t: Iterable[T]) = {

    val rightmostColumn = List.iterate(0, o.size + 1) { _ + deleteCost }

    def nextColumn(ccol: List[Int], tChar: T): List[Int] =
      (ccol zip ccol.tail zip o).scanLeft(ccol.head + insertCost) {
        case (t, ((d, r), oChar)) =>
          (t + deleteCost) min
            (r + insertCost) min
            (d + (if (tChar == oChar) 0 else substituteCost))
      }

    t.foldLeft(rightmostColumn)(nextColumn).last
  }

}

object LDistance { 
  
  object genDistance extends LDistance
  object charDistance extends LDistance[Char] 
  
  implicit def distance[T <: Iterable[Char]]: Distance[T] = new Distance[T] {
    def apply(e1: T, e2: T) = charDistance(e1, e2)
  }

  implicit object stringDistance extends Distance[String] {
    def apply(e1: String, e2: String) = charDistance(e1, e2)
  }
  
  def apply(e1: String, e2: String) = stringDistance(e1, e2)
  def apply[T <: Iterable[Char]](e1: T, e2: T) = charDistance(e1, e2)
}

