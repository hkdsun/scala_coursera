def isort (list : List[Int]) : List[Int] = list match {
  case List() => List()
  case y :: ys => insert(y,isort(ys))
}

def insert(x : Int, list : List[Int]) : List[Int] = list match {
  case List() => List(x)
  case y :: ys =>
    if(x <= y) x :: list
    else y :: insert(x,ys)
}

val l = scala.collection.immutable.List(1,4,6,2,5,3)

isort(l)
