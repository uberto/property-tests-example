import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Prop.forAll


object PhysicalUnit extends Enumeration {
  type PhysicalUnit = Value
  val None, Meters, Seconds, Kilograms = Value
}

import PhysicalUnit._

case class PhysicalQuantity(unit: PhysicalUnit, quantity: Double) {
  def +(otherQuantity: PhysicalQuantity) = {
    if (unit != otherQuantity.unit)
      throw new RuntimeException("Different unit of measure!")
    new PhysicalQuantity(unit, quantity + otherQuantity.quantity)
  }
}


object ScalaCheckDemo extends Properties("Demo") {

  implicit lazy val arbUnit: Arbitrary[PhysicalUnit] = Arbitrary(Gen.oneOf(None, Meters, Seconds, Kilograms))


  implicit def implThrows(x: => Any) = new {
    def throws[T <: Throwable](c: Class[T]) = try {
      x
      false
    } catch {
      case e if c.isInstance(e) => true
      case _ => false
    }
  }

  property("myprop") = forAll {
    l: List[Int] =>
      l.reverse.reverse == l
  }


  property("addition same units") = forAll {
    (u: PhysicalUnit, n: Double, m: Double) =>
      val q1 = new PhysicalQuantity(u, n)
      val q2 = new PhysicalQuantity(u, m)

      val q3 = q1 + q2

      //    println(" unit " + u + "  n " + n)

      q3.unit == u
  }


  property("addition same units addition of quantities") = forAll {
    (u: PhysicalUnit, n: Double, m: Double) =>
      val q1 = new PhysicalQuantity(u, n)
      val q2 = new PhysicalQuantity(u, m)

      val q3 = q1 + q2
      q3.quantity == m + n

  }

  property("addition different units") = forAll {
    (u: PhysicalUnit, v: PhysicalUnit, n: Double, m: Double) =>
      val q1 = new PhysicalQuantity(u, n)
      val q2 = new PhysicalQuantity(v, m)

      if (u == v)
        true
      else {
        q1 + q2 throws classOf[RuntimeException]
      }
  }
}
