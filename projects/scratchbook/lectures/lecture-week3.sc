abstract class IntSet {
  def contains(x: Int): Boolean
  def incl(x : Int): IntSet
  def union(other: IntSet): IntSet
}

object Empty extends IntSet {
  def contains(x: Int) = false
  def incl(x: Int) = new NonEmpty(x, Empty, Empty)
  def union (other: IntSet) = other
}

class NonEmpty (elem: Int, left: IntSet, right: IntSet) extends IntSet  {
  def contains(x: Int) : Boolean =
    if(x < elem) left contains elem
    else if(x>elem) right contains elem
    else true

  def incl(x: Int)=
    if(x < elem) new NonEmpty(elem, new NonEmpty(x, Empty,Empty), Empty)
    else if(x > elem) new NonEmpty(elem, Empty, new NonEmpty(x, Empty,Empty))
    else this

  def union(other: IntSet) =
    ((left union right) union other) incl elem

}