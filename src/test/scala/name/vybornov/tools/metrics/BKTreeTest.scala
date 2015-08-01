package name.vybornov.tools.metrics

import org.scalatest.FlatSpec

/**
 * @author vvv
 */
class BKTreeTest extends FlatSpec {
  "BKTree" should "pass basic tests" in {

    import LDistanceOptimized._
    
    val testSet = List(
        ("apples", "apples"), 
        ("oranges", "oranges"), 
        ("pineapples", "pineapples"))
    val t = BKTree.of(testSet: _*)

    assertResult(Some((("apples", "apples"),1)))(t.find(3)("aples"))
    assertResult(Some((("apples", "apples"),1)))(t.find(1)("aples"))
    assertResult(None)(t.find(1)("ales"))


  }
}