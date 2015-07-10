package fpinscala

import fpinscala.state._


object Chap6 extends App {


  /** Ex 6.1
    *
    * generate a random integer between 0 and Int.maxValue (inclusive). Make
    * sure to handle the corner case when nextInt returns Int.MinValue.
    */
  @annotation.tailrec
  def assertStream[C](n: Int, f: RNG => (C, RNG))
      (p: C => Boolean, rng: RNG = RNG.Simple(0)): Unit = {
    if (n > 0) {
      val (c, newRng) = f(rng)
      assert(p(c))
      assertStream(n-1, f)(p, newRng)
    }
  }
  assertStream(1000, RNG.nonNegativeInt)(_ >= 0)


  /** Ex 6.2
    *
    * generate a Double between 0 and 1, not including 1.
    */
  assertStream(1000, RNG.double _)(d => d >= 0.0 && d < 1.0)


  /** Ex 6.3
    *
    * generate an (Int, Double) pair, a (Double, Int) pair, and a
    * (Double, Double, Double) 3-tuple.
    */
  println("intDouble:")
  assertStream(3, RNG.intDouble _)(pair => {println(pair); true})
  println("doubleInt:")
  assertStream(3, RNG.doubleInt _)(pair => {println(pair); true})
  println("double3:")
  assertStream(3, RNG.double3 _)(triple => {println(triple); true})


  /** Ex 6.4
    *
    * generate a list of random integers.
    */
  println("ints:")
  println(RNG.ints(5)(RNG.Simple(0)))


  /** Ex 6.5
    *
    * double in terms of map
    */
  assertStream(1000, RNG.doubleM)(d => d >= 0.0 && d < 1.0)


  /** Ex 6.6
    *
    * takes two actions, ra and rb, and a function f for combining their
    * results, and returns a new action that combines them.
    */
  assertStream(3, RNG.map2(RNG.nonNegativeInt, RNG.doubleM)((i,d) => i*d)){
    prod => {println(prod); true} }


  /** Ex 6.7
    *
    * combining a List of transitions into a single transition. Use it to
    * reimplement the ints function.
    */
  println(RNG.sequence(List.fill(3)(RNG.doubleM))(RNG.Simple(0)))
  println("intsS:")
  println(RNG.intsS(5)(RNG.Simple(0)))

  /** Ex 6.8
    *
    *
    */


  /** Ex 6.9
    *
    *
    */


  /** Ex 6.10
    *
    *
    */


  /** Ex 6.11
    *
    *
    */

}

