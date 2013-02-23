package the4e6.fpinscala

trait Chapter2 {

  case class Box(height: Double, width: Double)

  def greaterBy(x: Box, y: Box, f: Box => Double): Box =
    if (f(x) > f(y)) x else y

  // Excercise 1
  def wider(x: Box, y: Box): Box =
    greaterBy(x, y, _.width)

  def taller(x: Box, y: Box): Box =
    greaterBy(x, y, _.height)

  // Excercise 2
  def abs(x: Int): Int =
    if (x > 0) x else -x

  def absolute0(f: Int => Int): Int => Int =
    x => abs(f(x))

  // Excercise 3
  def absolute[A](f: A => Int): A => Int =
    x => abs(f(x))

  // Excercise 4
  type Pred[A] = A => Boolean

  def divisibleBy(k: Int): Pred[Int] =
    x => x % k == 0

  // Excercise 5
  def even(x: Int): Boolean =
    divisibleBy(2)(x)

  // Excercise 6
  def lift0[A](f: (Boolean, Boolean) => Boolean, g: Pred[A], h: Pred[A]): Pred[A] =
    x => f(g(x), h(x))

  def divisibleBy_3and5(x: Int): Boolean =
    lift0(_ && _, divisibleBy(3), divisibleBy(5))(x)

  def divisibleBy_3or5(x: Int): Boolean =
    lift0(_ || _, divisibleBy(3), divisibleBy(5))(x)

  // Excercise 7
  def curry[A, B, C](f: (A, B) => C): A => B => C =
    x => f(x, _)

  // Excercise 8
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (x, y) => f(x)(y)

  // Excercise 9
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    x => f(g(x))

  // Polymorphic lift
  def lift[A, B, C, D](f: (B, C) => D)(g: A => B, h: A => C): A => D =
    x => f(g(x), h(x))

  // Excercise 10
  def lift3[A, B, C, D, E](f: (B, C, D) => E)
    (g: A => B, h: A => C, i: A => D): A => E =
    x => f(g(x), h(x), i(x))

  // Excercise 11
  def lift3a[A, B, C, D, E](f: (B, C, D) => E)
    (g: A => B, h: A => C, i: A => D): A => E =
    x => lift[A, C, D, E](f(g(x), _, _))(h, i)(x)

  // Exercise 12
  def fib(n: Int): Int = {
    def loop(prev: Int, curr: Int, step: Int): Int =
      if (step == n) curr
      else loop(curr, prev + curr, step + 1)

    n match {
      case x if x < 2 => 0
      case _ => loop(0, 1, step = 2)
    }
  }

  // Excercise 13
  def iterateWhile[A](a: A)(f: A => A, p: Pred[A]): A =
    if (p(f(a))) iterateWhile(f(a))(f, p)
    else f(a)

  def sqrt(n: Double): Double = {
    def f(x: Double) = x * x - n
    iterateWhile(2.0)(x => x - f(x) / (2 * x), x => f(x).abs > 1e-14)
  }
}

object ch2 extends Chapter2
