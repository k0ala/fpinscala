package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def toList: List[A] = {
    this match {
      case Cons(h,t) => h() :: t().toList
      case _ => Nil
    }
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = (this, n) match {
    case (_, 0) => Empty
    case (Empty, _) => Empty
    case (Cons(h, t), _) => Cons(h, () => t().take(n - 1))
  }

  def drop(n: Int): Stream[A] = (this, n) match {
    case (_, 0) => this
    case (Empty, _) => this
    case (Cons(h,t), _) => t().drop(n-1)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if p(h()) => Cons(h, () => t().takeWhile(p))
    case _ => empty
  }

  def takeWhileFR(p: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A])(
      (a,acc) => if (p(a)) cons(a, acc) else acc)
  }

  def forAll(p: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(h,_) if (p(h()) == false) => false
    case Cons(_,t) => t().forAll(p)
  }

  def headOption: Option[A] = {
    foldRight(None: Option[A])((a, acc) => Some(a) orElse acc)
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def mapFR[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((a, acc) => cons(f(a), acc))

  def filterFR(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, acc) => if (f(a)) cons(a, acc) else acc)

  def append[B >: A](bs: => Stream[B]): Stream[B] =
    foldRight(bs: Stream[B])((a, acc) => cons(a, acc))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((a, acc) => f(a).append(acc))

  def startsWith[B](s: Stream[B]): Boolean =
    this.zipWithU(s).forAll{ case (a,b) => a == b }


  def mapU[B](f: A => B): Stream[B] = unfold(this: Stream[A]){
    case Cons(a, s) => Some((f(a()), s()))
    case _ => None
  }

  def takeU(n: Int): Stream[A] = unfold((n,this)){
    case (i, Cons(a, s)) => if (i <= 0) None else Some((a(), (i-1, s()))) }

  def takeWhileU(p: A => Boolean): Stream[A] =
    unfold(this){
      case Cons(a, s) if p(a()) => Some((a(), s()))
      case _ => None
    }

  def zipWithU[B](bs: Stream[B]): Stream[(A,B)] =
    unfold((this, bs)){
      case (Cons(a, aa), Cons(b, bb)) => Some((a(),b()), (aa(),bb()))
      case _ => None
    }

  def zipAllU[B](bs: Stream[B]): Stream[(Option[A],Option[B])] =
    unfold((this, bs)) {
      case (Cons(a, aa), Cons(b, bb)) => Some((Some(a()),Some(b())), (aa(),bb()))
      case (Cons(a, aa), _) => Some(((Some(a()), None), (aa(), Stream.empty)))
      case (_, Cons(b, bb)) => Some(((None, Some(b())), (Stream.empty, bb())))
      case _ => None
    }

  def hasSub[B >: A](sub: List[B]): Boolean = {
    (this, sub) match {
      case (Cons(a, as), s :: ss) =>
        if (a()==s) (as().hasSub(ss) || as().hasSub(sub))
        else as().hasSub(sub)
      case (_, List()) => true
      case _ => false
    }
  }

  def tails: Stream[Stream[A]] =
    unfold(Some(this): Option[Stream[A]]){
      case Some(Cons(a, as)) => Some((Cons(a, as), Some(as())))
      case Some(_) => Some((Stream.empty, None))
      case None => None
    }

  def scanRightT[B](z: B)(f: (A, => B) => B): Stream[B] =
    this.tails.mapU(_.foldRight(z)(f))

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight(Stream(z)){ case (a, Cons(b, bs)) =>
      cons(f(a,b()), Cons(b, bs)) }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def const[A](c: A): Stream[A] = Stream.cons(c, const(c))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))

  def fibs(z: Int, o: Int): Stream[Int] = Stream.cons(z, fibs(o, z+o))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a,s)) => Stream.cons(a, unfold(s)(f))
      case None => Stream.empty
    }

  def fibsU(zero: Int, one: Int) = Stream.unfold((zero,one)){
    case (z, o) => Some(z, (o, z+o)) }
  def onesU = Stream.unfold("same"){ s => Some(1, "same") }
  def fromU(n: Int) = Stream.unfold(n){ s => Some(s, s+1) }
  def constU[B](a: B) = Stream.unfold(a){ s => Some(s,s) }

}
