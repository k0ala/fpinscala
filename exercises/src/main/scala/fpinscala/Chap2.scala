package fpinscala

object Chap2 {

  /** Ex 2.1
   *
   * returns the nth Fibonaci number, using a local tail-recursive function
   */
  def fib(n: Int): Int = {
    @annotation.tailrec
    def fibint(i: Int, prev: (Int, Int)): (Int, Int) = {
      val next = (prev._2, prev._1 + prev._2)
      if (i<n-1) fibint(i+1, next)
      else next
    }
    n match {
      case 0 => 0
      case 1 => 1
      case _ => fibint(1, (0,1))._2
    }
  }

  /** Ex 2.2
   *
   * checks whether an Array[A] is sorted according comparison function
   */
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    as.size match {
      case 0 => true
      case 1 => true
      case _ => as.zip(as.tail).foldLeft(true){
        case (x,(y,z)) => x && ordered(y,z) }
    }
  }
  def isSorted2[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def upto(pos: Int, sorted: Boolean): Boolean = {
      if (pos >= as.size - 1) sorted
      else upto(pos + 1, sorted && ordered(as(pos), as(pos+1)))
    }
    upto(0, true)
  }

  /** Ex 2.3
   *
   * the only function with this signature that compiles
   */
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    (a: A) => f(a, _)
  }

  /** Ex 2.4
   *
   * the only function with this signature that compiles
   */
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  /** Ex 2.5
   *
   * the only function with this signature that compiles
   */
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

}




