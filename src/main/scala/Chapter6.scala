package the4e6.fpinscala

trait Chapter6 {
  trait RNG {
    def nextInt: (Int, RNG)
  }

  object RNG {
    def simple(seed: Long): RNG = new RNG {
      def nextInt = {
        val seed2 = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
        (seed2 >>> 16).asInstanceOf[Int] -> simple(seed2)
      }
    }
  }

  lazy val rng = RNG.simple(1)

  // Exercise 1
  def positiveInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    if (i == Int.MinValue) positiveInt(r) else i.abs -> r
  }

  // Exercise 2
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = positiveInt(rng)
    if (i == Int.MaxValue)
      double(r)
    else
      i.toDouble / Int.MaxValue -> r
  }

  // Exercise 3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    (i, d) -> r2
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (id, r) = intDouble(rng)
    id.swap -> r
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    (d1, d2, d3) -> r3
  }

  // Exercise 4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count <= 0) Nil -> rng
    else {
      val (i, r) = rng.nextInt
      val (is, r2) = ints(count - 1)(r)
      (i :: is) -> r2
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      f(a) -> rng2
    }

  // Exercise 5
  def positiveMax(n: Int): Rand[Int] =
    map(positiveInt)(_ / (Int.MaxValue / n))

  // Exercise 6
  def doubleM: Rand[Double] =
    map(positiveInt)(_ / (Int.MaxValue.toDouble + 1))

  // Exercise 7
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      f(a, b) -> rng2
    }

  def intDoubleM: Rand[(Int, Double)] =
    map2(_.nextInt, double)(_ -> _)

  def doubleIntM: Rand[(Double, Int)] =
    map2(double, _.nextInt)(_ -> _)

  // Exercise 8
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List.empty[A])) {
      (r, acc) => map2(r, acc)(_ :: _)
    }

  def intsS(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  // Exercise 9
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng1) = f(rng)
      g(a)(rng1)
    }

  def positiveIntF: Rand[Int] =
    flatMap(int) { i =>
      if (i != Int.MinValue) unit(i.abs) else positiveIntF
    }

  // Exercise 10
  def mapF[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2F[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) { a => mapF(rb)(f(a, _)) }

  // Exercise 11
  case class State[S, +A](run: S => (A, S)) {

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      State { s =>
        val (a, s2) = run(s)
        f(a).run(s2)
      }

    def map[B](f: A => B): State[S, B] =
      flatMap(a => State.unit(f(a)))

    def map2[B, C](s: State[S, B])(f: (A, B) => C): State[S, C] =
      flatMap(a => s.map(f(a, _)))

  }

  object State {
    def unit[S, B](a: B): State[S, B] =
      State(s => (a, s))

    def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
      fs.foldRight(unit[S, List[A]](Nil)) { (s, acc) =>
        s.map2(acc)(_ :: _)
      }

    // Exsercise 12
    def get[S]: State[S, S] =
      State(s => s -> s)

    def set[S](s: S): State[S, Unit] =
      State(_ => () -> s)

    def modify[S](f: S => S): State[S, Unit] = for {
      s <- get
      _ <- set(f(s))
    } yield ()
  }

  // Exercise 13
  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)
}

object ch6 extends Chapter6
