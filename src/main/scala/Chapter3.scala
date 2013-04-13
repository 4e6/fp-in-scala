package the4e6.fpinscala

trait Chapter3 {

  /* Exercise 1
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

    // Exercise 2
    def tail[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(h, t) => t
    }

    // Exercise 3
    def drop[A](l: List[A], n: Int): List[A] =
      if (n == 0) l
      else l match {
        case Nil => Nil
        case Cons(_, t) => drop(t, n - 1)
      }

    // Exercise 4
    def dropWhile[A](xs: List[A])(p: A => Boolean): List[A] = xs match {
      case Nil => Nil
      case Cons(h, t) if p(h) => dropWhile(t)(p)
      case _ => xs
    }

    // Exercise 5
    def setHead[A](l: List[A], e: A): List[A] = l match {
      case Nil => Nil
      case Cons(h, t) => Cons(e, t)
    }

    // Exercise 6
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

    /* Exercise 7
     * foldRight(Cons(1, Cons(2, Cons(3, Nil))), 0)(_ + _)
     * 1 + foldRight(Cons(2, Cons(3, Nil)), 0)(_ + _)
     * 1 + (2 + foldRight(Cons(3, Nil), 0)(_ + _))
     * 1 + (2 + (3 + foldRight(Nil, 0)(_ + _)))
     * 1 + (2 + (3 + 0))
     * 1 + (2 + 3)
     * 1 + 5
     * 6
     */

    // Exercise 8
    // No

    /* Exercise 9
     * scala> List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_, _))
     * res0: the4e6.fpinscala.ch3.List[Int] = Cons(1,Cons(2,Cons(3,Nil)))
     */

    // Exercise 10
    def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

    // Exercise 11
    def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

    // Exercise 12
    def suml(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

    def productl(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

    def lengthl[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

    // Exercise 13
    def reverse[A](l: List[A]): List[A] =
      foldLeft(l, Nil: List[A])((acc, h) => Cons(h, acc))

    // Exercise 14
    def foldLeftR[A, B](l: List[A], z: B)(f: (B, A) => B): B =
      foldRight(reverse(l), z)((acc, h) => f(h, acc))

    def foldRightL[A, B](l: List[A], z: B)(f: (A, B) => B): B =
      foldLeft(reverse(l), z)((h, acc) => f(acc, h))

    // Exercise 15
    def append[A](l1: List[A], l2: List[A]): List[A] =
      foldRight(l1, l2)(Cons.apply)

    // Exercise 16
    def flatten[A](ll: List[List[A]]): List[A] =
      foldRight(ll, Nil: List[A])(append)

    // Exercise 17
    def map_++(l: List[Int]): List[Int] = l match {
      case Nil => Nil
      case Cons(h, t) => Cons(h + 1, map_++(t))
    }

    // Exercise 18
    def map_toString(l: List[Double]): List[String] = l match {
      case Nil => Nil
      case Cons(h, t) => Cons(h.toString, map_toString(t))
    }

    // Exercise 19
    def map[A, B](l: List[A])(f: A => B): List[B] = l match {
      case Nil => Nil
      case Cons(h, t) => Cons(f(h), map(t)(f))
    }

    // Exercise 20
    def filter[A](l: List[A])(p: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(h, t) =>
        if (p(h)) Cons(h, filter(t)(p))
        else filter(t)(p)
    }

    // Exercise 21
    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match {
      case Nil => Nil
      case Cons(h, t) => append(f(h), flatMap(t)(f))
    }

    // Exercise 22
    def filterf[A](l: List[A])(p: A => Boolean): List[A] =
      flatMap(l)(x => if (p(x)) List(x) else Nil)

    // Exercise 23
    def zip_+(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, zip_+(t1, t2))
    }

    // Exercise 24
    def zip[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zip(t1, t2)(f))
    }

    // Exercise 25
    def forall[A](l: List[A])(p: A => Boolean): Boolean = l match {
      case Nil => true
      case Cons(h, t) => p(h) && forall(t)(p)
    }

    def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = l match {
      case Nil => false
      case Cons(h, t) => forall(zip(l, sub)(_ == _))(identity) || hasSubsequence(t, sub)
    }
  }

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {
    val example =
      Branch(
        Branch(Leaf(1), Leaf(2)),
        Branch(
          Branch(Leaf(3), Leaf(4)),
          Leaf(5)
        )
      )

    // Exercise 26
    def size[A](t: Tree[A]): Int = t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }

    // Exercise 27
    def maximum(t: Tree[Int]): Int = t match {
      case Leaf(v) => v
      case Branch(l, r) => maximum(l) max maximum(r)
    }

    // Exercise 28
    def depth[A](t: Tree[A]): Int = t match {
      case Leaf(_) => 0
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }

    // Exercise 29
    def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

    // Exercise 30
    def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
      case Leaf(v) => f(v)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

    def sizef[A](t: Tree[A]): Int =
      fold(t)(_ => 1)(1 + _ + _)

    def maxf(t: Tree[Int]): Int =
      fold(t)(identity)(_ max _)

    def depthf[A](t: Tree[A]): Int =
      fold(t)(_ => 0)((l, r) => 1 + (l max r))

    def mapf[A, B](t: Tree[A])(f: A => B): Tree[B] =
      fold(t)(v => Leaf(f(v)): Tree[B])(Branch.apply)
  }
}

object ch3 extends Chapter3
