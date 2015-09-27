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

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i == Integer.MIN_VALUE) 0 else Math.abs(i), r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, rng1) = nonNegativeInt(rng)
    (i / Integer.MAX_VALUE.toDouble + 1, rng1)
  }

  def mDouble: Rand[Double] = map(nonNegativeInt)(_ / Integer.MAX_VALUE.toDouble + 1)

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng1) = rng.nextInt
    val (d, rng2) = double(rng1)
    ((i, d), rng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) (List.empty[Int], rng)
    else {
      val (i, r) = rng.nextInt
      val (l, r2) = ints(count - 1)(r)
      (i :: l, r2)
    }
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }


  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  // pattern match
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => {
      fs match {
        case x :: xs => map2(x, sequence(xs))((a, as) => a :: as)(rng)
        case Nil => (Nil, rng)
      }
    }

  // or use fold and use placeholder syntax
  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight[Rand[List[A]]](unit(Nil))(map2(_, _)(_ :: _))

  // or use a fold and some parameter names
  def sequence3[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight[Rand[List[A]]](r => (Nil, r))((f, fs) => map2(f, fs)((a, as) => a :: as))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }


  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0)
        unit(mod)
      else nonNegativeLessThan(n)
    }
  }

  def mapInTermsOfFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(f andThen unit)


  def map2InTermsOfFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) { a =>
      flatMap(rb)(b => unit(f(a, b))) // could have used map
    }
}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State(s => {
    val (a, s2) = run(s)
    (f(a), s2)
  })


  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State(
    s => {
      val (a, s2) = run(s)
      val (b, s3) = sb.run(s2)
      (f(a, b), s3)
    })

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State( s => {
      val (a, s2) = run(s)
      f(a).run(s2)
    })

}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def unit[S,A](a: A): State[S,A] = State({s => (a, s)})


  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def sequence[S,A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))


  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
   sequence(inputs.map(i => modify((s: Machine) => (i,s) match {
     case (_,Machine(_,0,_)) => s
     case (Coin,Machine(true,candies,coins)) => Machine(false,candies,coins+1)
   }
   ))).flatMap{ _ => get.map{ m => (m.coins,m.candies)}}
}
