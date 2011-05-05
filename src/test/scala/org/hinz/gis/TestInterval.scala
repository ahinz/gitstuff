import org.scalatest._
import org.scalatest.matchers._

import org.hinz.gis._

class IntervalSpec extends Spec with ShouldMatchers {
  
  describe("An interval") {
    it("should properly implement contains") {
      val i = Interval(10,20,10,null)

      i.contains(0) should equal(false)
      i.contains(10) should equal(true)
      i.contains(11) should equal(true)
      i.contains(20) should equal(true)
      i.contains(21) should equal(false)
    }

    it ("should split properly") {
      val i = Interval(1,9,9,null)
      val is = i.split(4)

      val i1 = is._1
      val i2 = is._2

      i1.start should equal (i.start)
      i1.end should equal (4)
      i1.time should equal (3.375)
      i2.start should equal (i1.end)
      i2.end should equal (i.end)
      i2.time should equal (9.0 - 3.375)
      
    }
  }
  

}
