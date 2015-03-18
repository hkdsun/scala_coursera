import scala._
/*
object maps {

  val romanNumerals = Map('I' -> 1, 'V' -> 5, 'X' -> 10)
  val capitalOfCountry = Map("US" -> "Washington", "Switzerland" -> "Bern")

  capitalOfCountry get "Andorra"
  capitalOfCountry get "US"
  val fruit = List("apple", "pear", "orange", "pineapple")
}
*/
object polynomials {
  class Poly (val terms0: Map[Int, Double]){
    def this(bindings: (Int,Double)*) = this(bindings.toMap)
    val terms = terms0 withDefaultValue 0.0

    def + (other: Poly) = new Poly((other.terms foldLeft terms)(addTerm))

    def addTerm(terms: Map[Int,Double], term:(Int,Double)) : Map[Int,Double] = {
      val (t,c) = term
      terms + (t -> (c+terms(t)))
    }
    override def toString =
      (for((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString " + "
  }
  val p1 = new Poly(1 -> 2.0, 3->4.0, 5 -> 6.2)
  val p2 = new Poly(0 -> 3.0, 3->7.0)
  p1 + p2
  p1.terms(7)

}