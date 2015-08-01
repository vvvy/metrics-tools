package name.vybornov.tools.metrics

/**
 * Generic distance between objects of type T
 * @author vvv
 */
trait Distance[T] {
    def apply(e1: T, e2: T): Int
}