package fpinscala

import fpinscala.datastructures.{List => CList}
import fpinscala.datastructures.{Nil => CNil}
import fpinscala.datastructures.Cons


object Chap3 extends App {


  /** Ex 3.2
    *
    * get the tail of a list
    */
  def tail[A](xs: CList[A]) = xs match {
    case Cons(y, ys) => ys
    case CNil => CNil
  }
  assert(tail(CList(1,2,3)) == CList(2,3))


  /** Ex 3.3
    *
    * replace the head of a list
    */
  def setHead[A](xs: CList[A], h: A) = xs match {
    case Cons(y, ys) => Cons(h, ys)
    case CNil => CNil
  }
  assert(setHead(CList(1,2,3), 4) == CList(4,2,3))


  /** Ex 3.4
    *
    * drop the n first elements
    */
  @annotation.tailrec
  def drop[A](xs: CList[A], n: Int): CList[A] = (xs, n) match {
    case (_, 0) => xs
    case (Cons(y, ys), _) => drop(ys, n-1)
    case (CNil, _) => CNil
  }
  assert(drop(CList(1,2,3,4), 3) == CList(4))


  /** Ex 3.5
    *
    * drop elements while predicate is true
    */
  @annotation.tailrec
  def dropWhile[A](xs: CList[A], p: A => Boolean): CList[A] = xs match {
    case CNil => CNil
    case Cons(y, ys) => if (p(y)) dropWhile(ys, p) else xs
  }
  assert(dropWhile(CList(1,2,3,4,5,6), (_:Int) < 4) == CList(4,5,6))


  def reverse[A](as: CList[A]) = {
    @annotation.tailrec
    def rev[A](rem: CList[A], acc: CList[A]): CList[A] = rem match {
      case CNil => acc
      case Cons(a, aa) => rev(aa, Cons(a, acc))
    }
    rev(as, CNil)
  }
  assert(reverse(CList(1,2,3)) == CList(3,2,1))


  /** Ex 3.6
    *
    * all but the last element of the list
    */
  def init[A](xs: CList[A]): CList[A] = xs match {
    case CNil => CNil
    case _ => reverse(tail(reverse(xs)))
  }
  assert(init(CList(1,2,3)) == CList(1,2))


  def foldRight[A,B](as: CList[A], z: B)(f: (A, B) => B): B = as match {
    case CNil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }


  /** Ex 3.7
    *
    * product with foldRight
    */
  def product(xs: CList[Double]): Double = {
    foldRight(xs, 1.0)(_ * _)
  }
  assert(product(CList(1.0,2.0,3.0)) == 6.0)


  /** Ex 3.8
    *
    * copying a list with foldRight
    */
  val l = CList(1,2,3,4)
  assert(foldRight(l, CNil : CList[Int])(Cons(_,_)) == l)


  /** Ex 3.9
    *
    * length of a list with foldRight
    */
  def length[A](as: CList[A]): Int = {
    foldRight(as, 0)((a, acc) => acc+1)
  }
  assert(length(CList(1,2,3)) == 3)


  /** Ex 3.10
    *
    * tail-recursive foldLeft
    */
  @annotation.tailrec
  def foldLeft[A,B](as: CList[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Cons(a, aa) => foldLeft(aa, f(z, a))(f)
      case CNil => z
    }
  }
  assert(foldLeft(CList(1,2,3), 0)(_ + _) == 6)


  /** Ex 3.11
    *
    * sum, product and length using foldLeft
    */
  def sumL(is: CList[Int]) = foldLeft(is, 0)(_ + _)
  assert(sumL(CList(2,3,4)) == 9)
  def productL(ds: CList[Double]) = foldLeft(ds, 1.0)(_ * _)
  assert(productL(CList(2.0,3.0,4.0)) == 24.0)
  def lengthL[A](as: CList[A]) = foldLeft(as, 0)((acc, a) => acc + 1)
  assert(lengthL(CList(1,2,3)) == 3)


  /** Ex 3.12
    *
    * reverse a list using a fold
    */
  def revL[A](as: CList[A]): CList[A] = {
    foldLeft(as, CNil:CList[A])((acc, a) => Cons(a, acc))
  }
  assert(revL(CList(1,2,3)) == CList(3,2,1))


  /** Ex 3.13
    *
    * foldRight in terms of foldLeft
    */
  def foldRightL[A,B](as: CList[A], z: B)(f: (A,B) => B): B = {
    val fl = (b: B, a: A) => f(a,b)
    foldLeft(revL(as), z)(fl)
  }
  assert( foldRight(l, CNil : CList[Int])(Cons(_,_)) ==
         foldRightL(l, CNil : CList[Int])(Cons(_,_)))


  /** Ex 3.14
    *
    * append in terms of foldRight
    */
  def append[A](as: CList[A], a: A): CList[A] = {
    foldRight(as, Cons(a, CNil))(Cons(_,_))
  }
  assert(append(CList(1,2,3), 4) == CList(1,2,3,4))


  /** Ex 3.15
    *
    * concatenates a list of lists into a single list
    */
  def concat[A](a1s: CList[A], a2s: CList[A]): CList[A] = {
    foldRight(a1s, a2s)(Cons(_,_))
  }
  assert(concat(CList(1,2), CList(3,4)) == CList(1,2,3,4))

  def flatten[A](ls: CList[CList[A]]): CList[A] = {
    foldRight(ls, CNil: CList[A])(concat(_,_))
  }
  assert(flatten(CList(CList(1,2), CList(3,4), CList(5,6))) == CList(1,2,3,4,5,6))


  /** Ex 3.16
    *
    * transforms a list of integers by adding 1 to each element
    */
  def add1(is: CList[Int]): CList[Int] = {
    foldRight(is, CNil : CList[Int])((a, acc) => Cons((a+1), acc))
  }
  assert(add1(CList(4,2,7)) == CList(5,3,8))


  /** Ex 3.17
    *
    * turns each value in a List[Double] into a String
    */
  def doubles2strings(ds: CList[Double]) = {
    foldRight(ds, CNil : CList[String])((a, acc) => Cons(a.toString, acc))
  }
  assert(doubles2strings(CList(1.0,2.0,10.0)) == CList("1.0","2.0","10.0"))


  /** Ex 3.18
    *
    * generalizes modifying each element in a list while maintaining
    * the structure of the list.
    */
  def map[A,B](as: CList[A])(f: A => B): CList[B] = {
    foldRightL(as, CNil: CList[B])((a, acc) => Cons(f(a), acc))
  }
  assert(doubles2strings(CList(1.5,3.0)) == map(CList(1.5,3.0))(_.toString))


  /** Ex 3.19
    *
    * removes elements from a list unless they satisfy a given predicate.
    */
  def filter[A](as: CList[A])(f: A => Boolean) = {
    foldRightL(as, CNil: CList[A])(
      (a, acc) => if (f(a)) Cons(a, acc) else acc)
  }
  assert(filter(CList(1,5,2,4,1))(_ < 3) == CList(1,2,1))


  /** Ex 3.20
    *
    * works like map except that the function given will return
    * a list instead of a single result, and that list should be inserted
    * into the final resulting list
    */
  def flatMap[A,B](as: CList[A])(f: A => CList[B]): CList[B] = {
    foldRight(as, CNil: CList[B])(
      (a, acc) => concat(f(a), acc))
  }
  assert(flatMap(CList(1,2,3))(i => CList(i,i)) == CList(1,1,2,2,3,3))


  /** Ex 3.21
    *
    * implement filter using flatMap
    */
  def filterFM[A](as: CList[A])(f: A => Boolean): CList[A] = {
    flatMap(as)(a => if (f(a)) CList(a) else CNil)
  }
  assert(filterFM(CList(1,5,2,4,1))(_ < 3) == filter(CList(1,5,2,4,1))(_ < 3))


  /** Ex 3.22
    *
    * accepts two lists and constructs a new list by adding corresponding
    * elements.
    */
  def elwiseSum(ls: CList[Int], rs: CList[Int]): CList[Int] = {
    (ls, rs) match {
      case (Cons(l, ll), Cons(r, rr)) => Cons(l+r, elwiseSum(ll,rr))
      case _ => CNil
    }
  }
  assert(elwiseSum(CList(1,2,3), CList(4,5,6)) == CList(5,7,9))


  /** Ex 3.23
    *
    * generalize the above for all types and functions
    */
  def zipWith[A,B,C](ls: CList[A], rs: CList[B])(f: (A,B) => C): CList[C] = {
    (ls, rs) match {
      case (Cons(l, ll), Cons(r, rr)) => Cons(f(l,r), zipWith(ll,rr)(f))
      case _ => CNil
    }
  }
  assert(zipWith(CList(1,2,3), CList(4,5,6))(_ + _) ==
    elwiseSum(CList(1,2,3), CList(4,5,6)))


  /** Ex 3.24
    *
    * check whether a list has a subsequence
    */
  def hasSubsequence[A](as: List[A], sub: List[A]): Boolean = {
    as match {
      case Nil => false
      case _ :: aa => as.zip(sub).forall(x => x._1 == x._2) ||
        hasSubsequence(aa, sub)
    }
  }
  assert(hasSubsequence(List(1,2,3,4),List(2,3)) == true)
  assert(hasSubsequence(List(1,2,3,4),List(1,2,3,4)) == true)
  assert(hasSubsequence(List(1,2,3,4),List(2,2)) == false)



  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


  /** Ex 3.25
    *
    * number of nodes in a tree
    */
  def size[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }
  }
  assert(size(Leaf(1)) == 1)
  assert(size(Branch(Leaf(1), Branch(Branch(Leaf(2), Leaf(3)), Leaf(4)))) == 7)


  /** Ex 3.26
    *
    * returns the maximum element in a Tree[Int]
    */
  def max(t: Tree[Int]): Int = t match {
    case Leaf(i) => i
    case Branch(left, right) => max(left).max(max(right))
  }
  assert(max(Branch(Branch(Leaf(1), Leaf(4)), Branch(Leaf(7), Leaf(3)))) == 7)


  /** Ex 3.27
    *
    * returns the maximum path length from the root of a tree to any leaf.
    */
  def maxDepth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + maxDepth(left).max(maxDepth(right))
  }
  assert(maxDepth(Branch(Branch(Leaf(1), Leaf(4)), Branch(Leaf(7), Leaf(3)))) == 3)


  /** Ex 3.28
    *
    * map for trees
    */
  def map[A,B](as: Tree[A])(f: A => B): Tree[B] = as match {
    case Leaf(a) => Leaf(f(a))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }
  assert(map(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))(_.toString) ==
    Branch(Leaf("1"), Branch(Leaf("2"), Leaf("3"))))


  /** Ex 3.29
    *
    * Generalize size, maximum, depth, and map, writing a new function fold
    * that abstracts over their similarities.
    */
   def fold[A,B](as: Tree[A])(lf: A => B)(br: (B,B) => B): B = as match {
    case Leaf(a) => lf(a)
    case Branch(left, right) =>
      br(fold(left)(lf)(br),
         fold(right)(lf)(br))
  }
  def sizeF[A](as: Tree[A]) =
    fold(as)(x => 1){ case (left, right) => 1 + left + right }

  def maxF(is: Tree[Int]) =
    fold(is)(x => x){ case (left, right) => left.max(right) }

  def maxDepthF[A](as: Tree[A]) =
    fold(as)(x => 1){ case (left, right) => 1 + left.max(right) }

  def mapF[A,B](as: Tree[A])(f: A => B): Tree[B] =
    fold(as)(a => Leaf(f(a)): Tree[B]){
      case (left, right) => Branch(left, right): Tree[B] }

  val t = Branch(Branch(Leaf(1), Leaf(4)), Branch(Leaf(7), Leaf(3)))
  assert(sizeF(t) == size(t))
  assert(maxF(t) == max(t))
  assert(maxDepthF(t) == maxDepth(t))
  assert(mapF(t)(_ * 3) == map(t)(_ * 3))


}





