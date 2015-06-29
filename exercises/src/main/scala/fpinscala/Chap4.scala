package fpinscala

import fpinscala.errorhandling._


object Chap4 extends App {

  /** Ex 3.1
    *
    * implement the methods of the Option trait
    */
  val noneD: Option[Double] = None
  // map:
  assert(noneD.map(_ * 2) == None)
  assert(Some(1.0).map(_ * 2) == Some(2.0))
  // getOrElse:
  assert(noneD.getOrElse(3.0) == 3.0)
  assert(Some(1.0).getOrElse(3.0) == 1.0)
  // flatMap:
  assert(noneD.flatMap(x => Some(3*x)) == None)
  assert(Some(1.0).flatMap(x => Some(3*x)) == Some(3.0))
  // orElse:
  assert(noneD.orElse(Some(2.0)) == Some(2.0))
  assert(noneD.orElse(None) == None)
  assert(Some(1.0).orElse(Some(1.0)) == Some(1.0))
  assert(Some(1.0).orElse(None) == Some(1.0))
  // filter:
  assert(noneD.filter(_ < 0) == None)
  assert(Some(1.0).filter(_ < 0) == None)
  assert(Some(- 1.0).filter(_ < 0) == Some(- 1.0))


  /** Ex 3.2
    *
    * implement variance in terms of flatMap
    */
  val emptyD: Seq[Double] = Seq()
  assert(Option.variance(emptyD) == None)
  assert(Option.variance(Seq(1.0,2.0)) == Some(0.25))


  /** Ex 3.3
    *
    * generic function map2 that combines two Option values using a binary
    * function.  If either Option value is None, then the return value is too.
    */
  def intDoubString = (i: Int, d: Double) => i.toString + "," + d.toString
  assert(Option.map2(None, None)(intDoubString) == None)
  assert(Option.map2(Some(2), None)(intDoubString) == None)
  assert(Option.map2(None, Some(3.0))(intDoubString) == None)
  assert(Option.map2(Some(2), Some(3.0))(intDoubString) == Some("2,3.0"))

  def intDoubCharString = (i: Int, d: Double, c: Char) =>
    i.toString + c + d.toString
  assert(Option.map3(None, None, None)(intDoubCharString) == None)
  assert(Option.map3(Some(2), None, None)(intDoubCharString) == None)
  assert(Option.map3(Some(2), None, Some(','))(intDoubCharString) == None)
  assert(Option.map3(None, Some(3.0), Some(','))(intDoubCharString) == None)
  assert(Option.map3(Some(2), Some(3.0), Some(','))(intDoubCharString) == Some("2,3.0"))


  /** Ex 3.4
    *
    * combines a list of Options into one Option containing a list of all the
    * Some values in the original list. If the original list contains None
    * even once, the result of the function should be None.
    */
  assert(Option.sequence(List()) == Some(List()))
  assert(Option.sequence(List(None)) == None)
  assert(Option.sequence(List(Some(1), None)) == None)
  assert(Option.sequence(List(None, Some(1))) == None)
  assert(Option.sequence(List(Some(2), Some(1))) == Some(List(2,1)))


  /** Ex 3.5
    *
    * Implement a traverse function that only looks at the list once. Then,
    * implement sequence in terms of traverse.
    */
  assert(Option.traverse(List(1.0,2.0))(Some(_)) == Some(List(1.0, 2.0)))
  assert(Option.traverse(List(0.0,2.0))(_ => None) == None)
  assert(Option.traverse(List(0.0,1.0,2.0))(x => if(x==1.0) None else Some(x)) == None)

  assert(Option.sequenceT(List()) == Some(List()))
  assert(Option.sequenceT(List(None)) == None)
  assert(Option.sequenceT(List(Some(1), None)) == None)
  assert(Option.sequenceT(List(None, Some(1))) == None)
  assert(Option.sequenceT(List(Some(2), Some(1))) == Some(List(2,1)))


  /** Ex 3.6
    *
    * implement the methods for Either
    */
  val one: Either[String, Int] = Right(1)
  val err: Either[String, Int] = Left("err")
  // map:
  assert(one.map(_ + 2) == Right(3))
  assert(err.map(_ + 2) == err)
  // flatMap:
  assert(one.flatMap(r => Right(r + 2)) == Right(3))
  assert(err.flatMap(r => Right(r + 2)) == err )
  // orElse:
  assert(one.orElse(Right(2)) == one)
  assert(err.orElse(Right(2)) == Right(2))
  assert(one.orElse(Left("err2")) == one)
  assert(err.orElse(Left("err2")) == Left("err2"))
  // map2:
  assert(one.map2(one)(_ + _) == Right(2))
  assert(err.map2(one)(_ + _) == err)
  assert(err.map2(Left("err2"))(_ + _) == err)
  assert(one.map2(Left("err2"))(_ + _) == Left("err2"))


  /** Ex 3.7
    *
    * sequence and traverse for Either. These should return the first error
    * that’s encountered, if there is one.
    */
  def erri(i: Int) = Left("err" + i)

  assert(Either.sequence(List(one,Right(2))) == Right(List(1,2)))
  assert(Either.sequence(List(err,one)) == err)
  assert(Either.sequence(List(err,erri(2))) == err)
  assert(Either.sequence(List(one,erri(2))) == erri(2))

  assert(Either.traverse(List(1,2))(Right(_)) == Right(List(1,2)))
  assert(Either.traverse(List(1,2))(erri) == erri(1))
  assert(Either.traverse(List(1,2))(x => if (x==1) one else err) == err)
  assert(Either.traverse(Nil)(_ => err) == Right(Nil))

  assert(Either.traverse(List(1,2))(erri) == erri(1))
  assert(Either.traverseFR(List(1,2))(erri) == erri(1))



  /** Ex 3.8
    *
    *
    */


}
