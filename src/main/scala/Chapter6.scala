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
}

object ch6 extends Chapter6
