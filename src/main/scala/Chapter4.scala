package the4e6.fpinscala

import java.util.regex.{ Pattern, PatternSyntaxException }

trait Chapter4 {

  sealed trait Option[+A] {
    // Excercise 1
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(v) => Some(f(v))
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(v) => v
    }

    def flatMap[B](f: A => Option[B]): Option[B] =
      this map f getOrElse None

    def orElse[B >: A](ob: Option[B]): Option[B] =
      this map Some.apply getOrElse ob

    def filter(p: A => Boolean): Option[A] =
      if (this map p getOrElse false) this
      else None
  }

  case object None extends Option[Nothing]
  case class Some[A](get: A) extends Option[A]

  // Excercise 2
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.size)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap { m => mean(xs.map(x => math.pow(x - m, 2))) }

  // Excercise 3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  // Excercise 4
  def pattern(s: String): Option[Pattern] =
    try { Some(Pattern.compile(s)) }
    catch { case _: PatternSyntaxException => None }

  def mkMatcher(pat: String): Option[String => Boolean] =
    pattern(pat) map { p => s => p.matcher(s).matches }

  def bothMatch_2(pat1: String, pat2: String, s: String): Option[Boolean] =
    map2(mkMatcher(pat1), mkMatcher(pat2))(_(s) && _(s))

  // Excercise 5
  def sequence0[A](a: List[Option[A]]): Option[List[A]] =
    try { Some(a map { _.getOrElse(throw new Exception) }) }
    catch { case _: Exception => None }

  def sequence1[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case o :: os =>
        for {
          x <- o
          xs <- sequence1(os)
        } yield x :: xs
    }

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight(Some(Nil): Option[List[A]]) { (o, acc) => map2(o, acc)(_ :: _) }

  // Ecxercise 6
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight(Some(Nil): Option[List[B]]) { (a, acc) =>
      for {
        b <- f(a)
        xs <- acc
      } yield b :: xs
    }

  sealed trait Either[+E, +A] {
    // Excercise 7
    def map[B](f: A => B): Either[E, B] = this match {
      case Left(e) => Left(e)
      case Right(a) => Right(f(a))
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(e) => b
      case Right(a) => Right(a)
    }

    def map2[EE >: E, B, C](b: => Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      for {
        aa <- this
        bb <- b
      } yield f(aa, bb)
  }

  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  // Excercise 8
  object Eihter {
    def sequence0[E, A](a: List[Either[E, A]]): Either[E, List[A]] =
      a.foldRight(Right(Nil): Either[E, List[A]]) { (a, acc) => a.map2(acc)(_ :: _) }

    def traverse[E, A, B](a: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
      a.foldRight(Right(Nil): Either[E, List[B]]) { (a, acc) => f(a).map2(acc)(_ :: _) }

    def sequence[E, A](a: List[Either[E, A]]): Either[E, List[A]] =
      traverse(a)(identity)
  }

  // Excercise 9
  case class Person(name: Name, age: Age)
  sealed class Name(val value: String)
  sealed class Age(val value: Int)

  type V[+E, +A] = Either[List[E], A]

  def vmap2[E, EE >: E, A, B, C](a: V[EE, A], b: V[EE, B])(f: (A, B) => C): V[EE, C] =
    (a, b) match {
      case (Left(e1), Left(e2)) => Left(e1 ++ e2)
      case (Left(e1), Right(_)) => Left(e1)
      case (Right(_), Left(e2)) => Left(e2)
      case (Right(a1), Right(a2)) => Right(f(a1, a2))
    }

  def mkName(name: String): V[String, Name] =
    if (name == "" || name == null) Left(List("Name is empty."))
    else Right(new Name(name))

  def mkAge(age: Int): V[String, Age] =
    if (age < 0) Left(List("Age is out of range."))
    else Right(new Age(age))

  def mkPerson(name: String, age: Int): V[String, Person] =
    vmap2(mkName(name), mkAge(age))(Person)
}

object ch4 extends Chapter4
