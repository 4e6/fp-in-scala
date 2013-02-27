package the4e6.fpinscala

trait Chapter5 {

  trait Stream[+A] {
    import Stream._

    def uncons: Option[(A, Stream[A])]
    def isEmpty: Boolean = uncons.isEmpty

    // Excercise 1
    def toList: List[A] =
      if (isEmpty) Nil
      else {
        val Some((h, t)) = uncons
        h :: t.toList
      }

    // Excercise 2
    def take(n: Int): Stream[A] =
      if (n <= 0 || isEmpty) empty
      else {
        val Some((h, t)) = uncons
        cons(h, t.take(n - 1))
      }

    // Excercise 3
    def takeWhile(p: A => Boolean): Stream[A] = uncons match {
      case Some((h, t)) if p(h) => cons(h, t takeWhile p)
      case _ => empty
    }

    // Excercise 4
    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      uncons match {
        case Some((h, t)) => f(h, t.foldRight(z)(f))
        case _ => z
      }

    def forAll(p: A => Boolean): Boolean =
      foldRight(true)(p(_) && _)

    // Excercise 5
    def takeWhilef(p: A => Boolean): Stream[A] =
      foldRight(empty[A])((a, as) => if (p(a)) cons(a, as) else empty)

    // Excercise 6
    def map[B](f: A => B): Stream[B] =
      foldRight(empty[B])((a, as) => cons(f(a), as))

    def filter(p: A => Boolean): Stream[A] =
      foldRight(empty[A])((a, as) => if (p(a)) cons(a, as) else as)

    def append[B >: A](xs: Stream[B]): Stream[B] =
      foldRight(xs)(cons(_, _))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(empty[B])((a, as) => f(a) append as)

  }

  object Stream {

    def empty[A]: Stream[A] =
      new Stream[A] { def uncons = None }

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      new Stream[A] { lazy val uncons = Some(hd -> tl) }

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))
  }

}

object ch5 extends Chapter5
