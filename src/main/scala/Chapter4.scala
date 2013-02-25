package the4e6.fpinscala

import java.util.regex.Pattern
import java.util.regex.PatternSyntaxException

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
    a.foldRight(Some(Nil): Option[List[A]]) { (o, acc) =>
      for {
        x <- o
        xs <- acc
      } yield x :: xs
    }
}

object ch4 extends Chapter4
