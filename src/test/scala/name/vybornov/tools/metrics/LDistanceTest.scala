package name.vybornov.tools.metrics
import LDistance._

import org.scalatest.FlatSpec

/**
 * @author vvv
 */
class LDistanceTest extends FlatSpec {
  
  
  "LDistance*" should "pass basic tests" in {
    
    def t(vs: ((String, String), Int)*) = 
      for(((v1, v2), d) <- vs) {
        assertResult(d)(LDistance(v1, v2))
        assertResult(d)(LDistanceOptimized(v1, v2))
      }
        
    t( 
        (("", "") -> 0),
        (("a", "a") -> 0), 
        (("a", "b") -> 1),      
        (("longword", "longwort") -> 1),
        (("longwor", "longword") -> 1),
        (("lonwwor", "longword") -> 2),
        (("uffew", "longwort") -> 7)
       )

  }
}