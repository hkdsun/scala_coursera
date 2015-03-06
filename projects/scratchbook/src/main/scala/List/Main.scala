package List



object Main {

  def nth[T](root: List[T], n: Int) : T = {
    if (root.isEmpty) throw new IndexOutOfBoundsException("uh oh")
    else if (n == 0) root.head
    else nth(root.tail,n-1)
  }

  def main(args: Array[String]) {
    val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))
    println(nth(list, 1))
  }

}
