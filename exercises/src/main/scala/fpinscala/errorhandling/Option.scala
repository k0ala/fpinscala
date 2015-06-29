package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  def flatMapPM[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    this.map(f).getOrElse(None)
  }

  def orElsePM[B>:A](ob: => Option[B]): Option[B] = this match {
    case Some(a) => Some(a)
    case None => ob
  }

  def orElse[B>:A](ob: => Option[B]): Option[B] = {
    this.map(a => Some(a)).getOrElse(ob)
  }

  def filterPM(f: A => Boolean): Option[A] = this match {
    case Some(a) if (f(a)) => Some(a)
    case _ => None
  }

  def filter(f: A => Boolean): Option[A] = {
    this.flatMap{ a => if(f(a)) Some(a) else None }
  }

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap(m => mean(xs.map(x => (x-m)*(x-m))))

  def map2[A,B,C](oa: Option[A], ob: Option[B])(f: (A, B) => C): Option[C] =
    oa.flatMap(a => { ob.map(b => f(a,b)) })

  def map3[A,B,C,D](oa: Option[A], ob: Option[B], oc: Option[C])(f: (A, B, C) => D): Option[D] = {
    val fa = (a:A) => (b:B,c:C) => f(a,b,c)
    oa.flatMap(a => map2(ob, oc)(fa(a)))
  }

  def sequence[A](oas: List[Option[A]]): Option[List[A]] = {
    oas.foldRight(Some(Nil): Option[List[A]])({ (oa, ol) => map2(oa, ol)(_ :: _) })
  }

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = {
    as.foldRight(Some(Nil): Option[List[B]]){
      (a, obs) => map2(f(a), obs)(_ :: _) }
  }

  def sequenceT[A](oas: List[Option[A]]): Option[List[A]] = {
    traverse(oas)(oa => oa)
  }

}
