package fpinscala

import fpinscala.laziness._


object Chap5 extends App {


  /** Ex 5.1
    *
    * convert a Stream to a List, which will force its evaluation
    */
  assert(Stream.empty[Double].toList == List.empty[Double])
  assert(Stream(1,2,3).toList == List(1,2,3))


  /** Ex 5.2
    *
    * Write the function take(n) for returning the first n elements of a
    * Stream, and drop(n) for skipping the first n elements of a Stream.
    */
  assert(Stream(1,2,3,4,5).take(3).toList == List(1,2,3))
  assert(Stream(1).take(4).toList == List(1))
  assert(Stream(1,2,3,4,5).drop(3).toList == List(4,5))
  assert(Stream(1).drop(4).toList == List.empty)


  /** Ex 5.3
    *
    * Write the function takeWhile for returning all starting elements of a
    * Stream that match the given predicate.
    */
  assert(Stream(1,2,3,4,5).takeWhile(_ < 4).toList == List(1,2,3))


  /** Ex 5.4
    *
    * checks that all elements in the Stream match a given predicate.
    * Your implementation should terminate the traversal as soon as it
    * encounters a nonmatching value.
    */
  assert(Stream(1,2,3,4).forAll(_ < 5) == true)
  assert(Stream.empty[Int].forAll(_ < 5) == true)
  assert(Stream(1,2,3,4,5).forAll(_ < 5) == false)


  /** Ex 5.5
    *
    * foldRight in terms of takeWhile.
    */
  assert(Stream(1,2,3,4,5).takeWhileFR(_ < 4).toList == List(1,2,3))


  /** Ex 5.6
    *
    * headOption using foldRight.
    */
  assert(Stream(1).headOption == Some(1))
  assert(Stream(1,2,3).headOption == Some(1))
  assert(Stream.empty[Double].headOption == (None: Option[Double]))


  /** Ex 5.7
    *
    * map, filter, append, and flatMap using foldRight. The append method
    * should be non-strict in its argument.
    */
  // map:
  assert(Stream(1,2,3).mapFR(_ * 2).toList == List(2,4,6))
  assert(Stream.empty[Double].mapFR(_ * 2.0).toList == List.empty)
  // filter:
  assert(Stream(1,2,3,4).filterFR(_ % 2 == 0).toList == List(2,4))
  assert(Stream.empty[Double].filterFR(_ % 2 == 0).toList == List.empty)
  // append:
  assert(Stream.empty[Double].append(Stream(1.0,2.0)).toList == List(1.0,2.0))
  assert(Stream(1.0,2.0).append(Stream.empty[Double]).toList == List(1.0,2.0))
  assert(Stream(1,2).append(Stream(3,4)).toList == List(1,2,3,4))
  // flatMap:
  assert(Stream(1,3).flatMap(x => Stream(x + 1, x + 2)).toList ==
    Stream(2,3,4,5).toList)
  assert(Stream.empty[Int].flatMap(x => Stream(x + 1, x + 2)).toList ==
    List.empty)
  assert(Stream(1,3).flatMap(x => Stream.empty[Double]).toList == List.empty)


  /** Ex 5.8
    *
    * Generalize ones slightly to the function constant, which returns an
    * infinite Stream of a given value.
    */
  assert(Stream.const(3).take(5).toList == List(3,3,3,3,3))
  assert(Stream.const(3).take(0).toList == List())


  /** Ex 5.9
    *
    * generates an infinite stream of integers, starting from n.
    */
  assert(Stream.from(7).take(3).toList == List(7,8,9))


  /** Ex 5.10
    *
    * generates the infinite stream of Fibonacci numbers:
    * 0, 1, 1, 2, 3, 5, 8, and so on.
    */
  assert(Stream.fibs(0,1).take(7).toList == List(0, 1, 1, 2, 3, 5, 8))

  /** Ex 5.11
    *
    * takes an initial state, and a function for producing both the next
    * state and the next value in the generated stream.
    */
  assert(Stream.unfold(1){ x =>
    if (x<5) Some("*"*x, x*2) else None}.toList == List("*", "**", "****"))


  /** Ex 5.12
    *
    * fibs, from, constant, and ones in terms of unfold.
    */
  assert(Stream.onesU.take(3).toList == List(1,1,1))
  assert(Stream.constU("whee").take(3).toList == List("whee","whee","whee"))
  assert(Stream.fromU(5).take(3).toList == List(5,6,7))
  assert(Stream.fibsU(0,1).take(7).toList == List(0, 1, 1, 2, 3, 5, 8))


  /** Ex 5.13
    *
    * map, take, takeWhile, zipWith  and zipAll in terms of unfold.
    * The zipAll function should continue the traversal as long as
    * either stream has more elements—it uses Option to indicate whether
    * each stream has been exhausted.
    */
  assert(Stream.fibs(0,1).mapU(_ * 2).take(5).toList == List(0,2,2,4,6))
  assert(Stream.fibs(0,1).takeU(7).toList == List(0, 1, 1, 2, 3, 5, 8))
  assert(Stream(1,2,3,4,5).takeWhileU(_ < 4).toList == List(1,2,3))
  assert(Stream(1,2,3).zipWithU(Stream(4,5)).toList == List((1,4),(2,5)))
  assert(Stream(1,2).zipWithU(Stream(4,5,6)).toList == List((1,4),(2,5)))
  assert(Stream(1,2).zipAllU(Stream(3,4)).toList ==
    List((Some(1),Some(3)),(Some(2),Some(4))))
  assert(Stream(1,2).zipAllU(Stream(3)).toList ==
    List((Some(1),Some(3)),(Some(2),None)))
  assert(Stream(1).zipAllU(Stream(3,4,5)).toList ==
    List((Some(1),Some(3)),(None, Some(4)),(None, Some(5))))


  /** (Ex "5.13b")
    *
    * hasSub determines whether a stream has a given subsequence.
    */
  assert(Stream.empty.hasSub(List(1,2,3)) == false)
  assert(Stream(1,2,3,4).hasSub(List(1,2,3)) == true)
  assert(Stream(1,2,3,4).hasSub(List(1,2,3,4)) == true)
  assert(Stream(1,2,3,4).hasSub(List(2,3,4)) == true)
  assert(Stream(1,2,1,2,3).hasSub(List(1,2,3)) == true)


  /** Ex 5.14
    *
    * check if one Stream is a prefix of another.
    */
  assert(Stream(1,2,3) startsWith Stream.empty)
  assert(Stream(1,2,3) startsWith Stream(1,2))
  assert(Stream(1,2,3).startsWith(Stream(2,3)) == false)


  /** Ex 5.15
    *
    * tails in terms of unfold; For a given Stream, tails returns the Stream
    * of suffixes of the input sequence, including the original Stream.
    */
  assert(Stream(1,2,3).tails.toList.map(_.toList) ==
    List(List(1,2,3), List(2,3), List(3), List()))
  assert(Stream.empty[Int].tails.toList == List(Empty))

  /** Ex 5.16
    *
    * like a foldRight that returns a stream of the intermediate results.
    */
  Stream(1,2,3).scanRight(0)(_ + _)
  assert(Stream(1,2,3).scanRight(0)(_ + _).toList == List(6,5,3,0))
  println(Stream(1,2).toList)
  println(Stream.cons(1, Stream.cons(2, Stream.empty)).toList)
  val s = Stream.cons(true,sys.error("oops")).scanRightAnswer(0)((a, b) => if (a) 1 else b)
  //println(s.take(1).toList)


}
