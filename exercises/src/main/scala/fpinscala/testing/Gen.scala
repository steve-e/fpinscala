package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state.RNG.{Rand, Simple}
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop {
  type  SuccessCount = Int
  def check: Boolean
  def &&(p:Prop):Prop = new Prop { override def check = Prop.this.check && p.check }
}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}


object Gen {
  case class Gen[A](sample: State[RNG,A])
  def unit[A](a: => A): Gen[A] = Gen(State(RNG.unit(a)))
  def boolean: Gen[Boolean] = Gen(State(RNG.int).map(_ %2 ==0))
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))
  def choose(start:Int,stopExclusive:Int) :Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(i => (i+start) % (stopExclusive-start)))


}

trait Gen[A] {
  def map[A,B](f: A => B): Gen[B] = ???
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
}

trait SGen[+A] {

}

