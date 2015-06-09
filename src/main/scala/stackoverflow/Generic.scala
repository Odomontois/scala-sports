/**
 * Author: Odomontois
 * Date  : 28-Apr-15
 * Time  : 18:43
 */
package stackoverflow

object Generic extends App {
  class ScalaGenericTest {
    def getValue[A, B](clazz: B)(implicit evidence: B <:< Abstract[A]): A = clazz.a

    def call: String = {
      val sub: Subclass = new Subclass
      getValue(sub)
    }
  }

  class Subclass extends Abstract[String] {
    def a: String = "STRING"
  }

  abstract class Abstract[A] {
    type elem = A

    def a: A
  }

  println(new ScalaGenericTest().call)
}
