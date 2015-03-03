package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println("Counting Change")
    println(countChange(4,List(1,2,3)))
    println(countChange(0,List(1,2,3)))
    println(countChange(4,List()))
    println(countChange(20,List(1,10,5,2,3)))

    println(balance("(if (zero? x) max (/ 1 x))".toList))
    println(balance("I told him (that it's not (yet) done). (But he wasn't listening)".toList))
    println(balance("I told him that it's not (yet) done). (But he wasn't listening)".toList))
    println(balance("".toList))
    println(balance(List()))
    println(balance(":-)".toList))
    println(balance("()))(".toList))
    println(balance("))((".toList))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if(c==r || c==0) 1
    else pascal(c-1,r-1) + pascal(c,r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def loop(list: List[Char], par: List[Char]) : Boolean=
      if (list.isEmpty && par.isEmpty) true
      else if(list.head == '(') loop(list.tail, par :+ '(')
      else if (list.head == ')') {
        if(par.isEmpty) false
        else loop(list.tail, par.tail)
      }
      else loop(list.tail, par)

    loop(chars, List())
  }
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    def loop(breakup: Int, change: List[Int]): Int ={
      if (breakup==0) 1
      else if (breakup<0) 0
      else if (change.size <= 0 && breakup>=1) 0
      else loop(breakup, change.tail) + loop(breakup - change.head, change)
    }

    loop(money, coins.sortWith(_ > _))
  }
}
