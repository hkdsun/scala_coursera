object exercise {
  def product(f: Int => Int)(a: Int, b: Int): Int =
    if(a>b) 1
    else f(a) * product(f)(a+1,b)
  product(x => x*x)(3,4)

  def factorial(a: Int) = product(x => x)(1,a)
  factorial(5)

  def general(operator: (Int, Int) => Int)(f: Int => Int)(identity: Int)(a: Int , b: Int): Int =
    if(a>b) identity
    else operator(f(a),general(operator)(f)(identity)(a+1,b))

  general((x,y)=> x*y)((x) => x*x)(1)(3,4)
  general((x,y)=> x+y)((x) => x*x)(0)(3,4)
}
