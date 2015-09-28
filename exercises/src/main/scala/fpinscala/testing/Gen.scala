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


case class Gen[A](sample: State[RNG,A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))
  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(n => Gen.listOfN(n,this))
}
object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State(RNG.unit(a)))
  def boolean: Gen[Boolean] = Gen(State(RNG.int).map(_ %2 ==0))
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))
  def choose(start:Int,stopExclusive:Int) :Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(i => (i+start) % (stopExclusive-start)))
  def double:Gen[Double] = Gen(State(RNG.double))
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = Gen.boolean.flatMap(b => if(b) g1 else g2)
  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] =
    Gen.double.flatMap(d => if(g1._2.abs/g2._2.abs + g1._2.abs > d) g1._1 else g2._1 )
}

//trait Gen[A] {
//  def map[A,B](f: A => B): Gen[B] = ???
//  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
//
//}

trait SGen[+A] {

}

