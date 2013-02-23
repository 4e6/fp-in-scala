package the4e6.fpinscala

trait Chapter3 {

  /* Excercise 1
   * case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
   * 3
   */

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    // Excercise 2
    def tail[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(h, t) => t
    }

    // Excrecise 3
    def drop[A](l: List[A], n: Int): List[A] =
      if (n == 0) l
      else l match {
        case Nil => Nil
        case Cons(_, t) => drop(t, n - 1)
      }

    // Excercise 4
    def dropWhile[A](xs: List[A])(p: A => Boolean): List[A] = xs match {
      case Nil => Nil
      case Cons(h, t) if p(h) => dropWhile(t)(p)
      case _ => xs
    }

    // Excercise 5
    def setHead[A](l: List[A], e: A): List[A] = l match {
      case Nil => Nil
      case Cons(h, t) => Cons(e, t)
    }

    // Excrecise 6
    def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(h, Cons(_, Nil)) => Cons(h, Nil)
      case Cons(h, t) => Cons(h, init(t))
    }

    // foldRight
    def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }

    /* Excercise 7
     * foldRight(Cons(1, Cons(2, Cons(3, Nil))), 0)(_ + _)
     * 1 + foldRight(Cons(2, Cons(3, Nil)), 0)(_ + _)
     * 1 + (2 + foldRight(Cons(3, Nil), 0)(_ + _))
     * 1 + (2 + (3 + foldRight(Nil, 0)(_ + _)))
     * 1 + (2 + (3 + 0))
     * 1 + (2 + 3)
     * 1 + 5
     * 6
     */

    // Excercise 8
    // No

    /* Excercise 9
     * scala> List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_, _))
     * res0: the4e6.fpinscala.ch3.List[Int] = Cons(1,Cons(2,Cons(3,Nil)))
     */

    // Excercise 10
    def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

    // Excercise 11
    def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

    // Excercise 12
    def suml(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

    def productl(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

    def lengthl[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

    // Excercise 13
    def reverse[A](l: List[A]): List[A] =
      foldLeft(l, Nil: List[A])((acc, h) => Cons(h, acc))

  }

}

object ch3 extends Chapter3
