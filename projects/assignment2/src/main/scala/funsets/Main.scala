package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))
  val set = union(singletonSet(1),singletonSet(2))
  println(contains(intersect(singletonSet(1),set),1))
  println(contains(intersect(singletonSet(1),set),2))
  println(forall(set,(x:Int)=>x==1||x==2))
  println(exists(set,(x:Int)=>x==2||x==1))
  println(FunSets.toString(map(set,(x:Int) => x*2)))
  println(FunSets.toString(set))


}
