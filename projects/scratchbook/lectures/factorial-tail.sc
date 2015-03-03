def factorial(x: Double) : Double = {
  def loop(d: Double, res: Double) : Double =
    if(d==0) res
    else loop(d-1, res) * d

  loop(x,1)
}

factorial(4)
