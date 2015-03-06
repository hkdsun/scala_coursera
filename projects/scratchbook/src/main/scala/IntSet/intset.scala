package IntSet

abstract class IntSet {
  def contains(x: Int): Boolean
  def incl(x : Int): IntSet
  def union(other: IntSet): IntSet
}

object Empty extends IntSet {
  def contains(x: Int) = false
  def incl(x: Int) = new NonEmpty(x, Empty, Empty)
  def union (other: IntSet) = other
  override def toString = "."
}

class NonEmpty (elem: Int, left: IntSet, right: IntSet) extends IntSet  {
  def contains(x: Int) : Boolean =
    if(x < elem) left contains elem
    else if(x>elem) right contains elem
    else true

  def incl(x: Int)=
    if(x < elem) new NonEmpty(elem, left incl x, right)
    else if(x > elem) new NonEmpty(elem, left, right incl x)
    else this

  def union(other: IntSet) =
    ((left union right) union other) incl elem

  override def toString = "{" + ' ' + left +' ' +  elem + ' ' + right + ' ' + "}"
}

object IntSet {
  def main (args: Array[String]) {
    println (Empty)
    val x = Empty incl 3
    val y = (((x incl 4) incl 1) incl 20)
    val z = y incl 10
    println(x)
    println(y)
  }
}