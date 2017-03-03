package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  case class Dummy(value: Int) extends RNG {
    def nextInt: (Int, RNG) = (value, Dummy(value))
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def int(rng: RNG): (Int, RNG) = rng.nextInt

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rng2) = rng.nextInt
    if (n < 0) (-(n+1), rng2) else (n, rng2)
  }

  // TODO: fix implementation, this introduces skew
  def double(rng: RNG): (Double, RNG) = {
    val (n, rng2) = nonNegativeInt(rng)
    // Yay ULP for 1:
    val d = if (n == Int.MaxValue) (n.toDouble-0.000001) / Int.MaxValue
            else n.toDouble / Int.MaxValue
    (d, rng2)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), rng2) = intDouble(rng)
    ((d, i), rng2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    {
      val (i, rng2) = rng.nextInt
      if (count == 1)
        (::(i, Nil), rng2)
      else {
        val (list, rng3) = ints(count - 1)(rng2)
        (::(i, list), rng3)
      }
    }

  // TODO: fix implementation
  def doubleViaMap: Rand[Double] = {
    map(nonNegativeInt)(n => (if (n == Int.MaxValue) (n.toDouble-0.000001) / Int.MaxValue
                              else n.toDouble / Int.MaxValue))
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def intDoubleViaMap2: Rand[(Int, Double)] = map2(int, double)((a, b) => (a, b))

  // TODO: FINISH
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => {
      fs match {
        case ::(f, Nil) =>
          val (a, rng2) = f(rng)
          (::(a, Nil), rng)
        case ::(f, fx) =>
          val (ax, rng2) = sequence(fx)(rng)
          val (a, rng3) = f(rng)
          (::(a, ax), rng3)
      }
    }

  // TODO: FINISH
  def ints2[Int](count: Int): List[Int] = sequence(List.fill(count)(int))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    sys.error("todo")
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
