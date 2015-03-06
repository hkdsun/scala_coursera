package List

trait List [T]{
  def isEmpty : Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T]{
  def isEmpty = false
}

class Nil[T] extends List[T] {
  def isEmpty = true
  def head: Nothing = throw new NoSuchFieldException
  def tail: Nothing = throw new NoSuchFieldException
}

object List{
  def nth[T](list: List[T], n: Int) = {
    def loop(root: List[T], i: Int) : T =
      if(root.isEmpty) throw new IndexOutOfBoundsException("uh oh")
      else if(n == i) root.head
      else loop(root.tail,n+1)

    loop(list,0)
  }
}

