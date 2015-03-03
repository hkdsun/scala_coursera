object rationals{
  val x = new Rational(1,3)
  x.numer
  x.denom
  val y = new Rational(5,7)
  val z = new Rational(3,2)

  x.sub(y).sub(z)
  y.add(y)

}
class Rational(x: Int, y: Int){
  require(y != 0, "denominator must be less than zero")

  def this(x: Int) = this(x,1)

  private def gcd(a: Int, b: Int): Int = if(b==0) a else gcd(b, a%b)

  private val g = gcd(x,y)

  def numer = x/g
  def denom = y/g

  def neg = new Rational(-x,y)

  def less(that: Rational) = this.numer * that.denom < that.numer * this.denom

  def max(that: Rational) = if(this.less(that)) that else this

  def add(that: Rational) =
    new Rational(
    numer * that.denom + that.numer * denom,
    denom * that.denom)

  def sub(that: Rational) = add(that.neg)

  override def toString = numer.toString + '/' + denom.toString
}

