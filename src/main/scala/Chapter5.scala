package the4e6.fpinscala

trait Chapter5 {

  trait Stream[+A] {
    import Stream._

    def uncons: Option[(A, Stream[A])]
    def isEmpty: Boolean = uncons.isEmpty

    // Exercise 1
    def toList: List[A] =
      if (isEmpty) Nil
      else {
        val Some((h, t)) = uncons
        h :: t.toList
      }

    // Exercise 2
    def take(n: Int): Stream[A] =
      if (n <= 0 || isEmpty) empty
      else {
        val Some((h, t)) = uncons
        cons(h, t.take(n - 1))
      }

    // Exercise 3
    def takeWhile(p: A => Boolean): Stream[A] = uncons match {
      case Some((h, t)) if p(h) => cons(h, t takeWhile p)
      case _ => empty
    }

    // Exercise 4
    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      uncons match {
        case Some((h, t)) => f(h, t.foldRight(z)(f))
        case None => z
      }

    def exists(p: A => Boolean): Boolean =
      foldRight(false)(p(_) || _)

    def forAll(p: A => Boolean): Boolean =
      foldRight(true)(p(_) && _)

    // Exercise 5
    def takeWhilef(p: A => Boolean): Stream[A] =
      foldRight(empty[A])((a, as) => if (p(a)) cons(a, as) else empty)

    // Exercise 6
    def map[B](f: A => B): Stream[B] =
      foldRight(empty[B])((a, as) => cons(f(a), as))

    def filter(p: A => Boolean): Stream[A] =
      foldRight(empty[A])((a, as) => if (p(a)) cons(a, as) else as)

    def append[B >: A](xs: Stream[B]): Stream[B] =
      foldRight(xs)(cons(_, _))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(empty[B])((a, as) => f(a) append as)

    // Exercise 12
    def mapU[B](f: A => B): Stream[B] =
      unfold(this)(s => s.uncons.map { case (h, t) => f(h) -> t })

    def takeU(n: Int): Stream[A] =
      unfold(n -> this) { case (n, s) => if (n > 0) s.uncons.map(ht => ht._1 -> (n - 1, ht._2)) else None }

    def takeWhileU(p: A => Boolean): Stream[A] =
      unfold(this)(s => s.uncons.filter { case (h, t) => p(h) })

    def zip[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] =
      unfold(this -> s) { case (s1, s2) =>
          for {
            (a, t1) <- s1.uncons
            (b, t2) <- s2.uncons
          } yield f(a, b) -> (t1, t2)
      }

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

  def ones: Stream[Int] =
    Stream.cons(1, ones)

  // Exercise 7
  def constant[A](a: A): Stream[A] =
    Stream.cons(a, constant(a))

  // Exercise 8
  def from(n: Int): Stream[Int] =
    Stream.cons(n, from(n + 1))

  // Exercise 9
  def fibs: Stream[Int] = {
    import Stream._
    def recur(a: Int, b: Int): Stream[Int] =
      cons(b, recur(b, a + b))

    cons(0, recur(0, 1))
  }

  // Exercise 10
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).fold(Stream.empty[A]) {
      case (a, s) => Stream.cons(a, unfold(s)(f))
    }

  trait WithUnfold {
    // Exercise 11
    def fibs: Stream[Int] =
      unfold((0, 1)) { case (a, b) => Some(b -> (b, a + b)) }

    def from(n: Int): Stream[Int] =
      unfold(n)(s => Some(s, s + 1))

    def constant[A](a: A): Stream[A] =
      unfold(a)(s => Some(s, s))

    def ones: Stream[Int] =
      unfold(1)(_ => Some(1, 1))
  }

  object unfolded extends WithUnfold

}

object ch5 extends Chapter5
