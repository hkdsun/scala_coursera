import math.abs
object exercise{
  val tolerance = 0.0001
  def isCloseEnough(a: Double, b: Double) = abs((a-b)/a)/a<=tolerance
  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
    def iterate(a: Double) : Double = {
      val next = f(a)
      if(isCloseEnough(a,next)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }
  def averageDamp(f: Double => Double)(x: Double) = (f(x) + x)/2
  def sqrt(x: Double) = fixedPoint(averageDamp(y => x/y))(1)
  sqrt(12234)
}