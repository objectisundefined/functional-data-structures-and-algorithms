package fpinscala.utils

import fpinscala.typeclass._

object FunctionalUtils {
  
  // Function composition utilities
  implicit class FunctionOps[A, B](f: A => B) {
    def andThen[C](g: B => C): A => C = a => g(f(a))
    def compose[Z](g: Z => A): Z => B = z => f(g(z))
    def >>>[C](g: B => C): A => C = andThen(g)
    def <<<[Z](g: Z => A): Z => B = compose(g)
  }
  
  // Currying and partial application utilities
  def curry[A, B, C](f: (A, B) => C): A => B => C = a => b => f(a, b)
  
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)
  
  def flip[A, B, C](f: (A, B) => C): (B, A) => C = (b, a) => f(a, b)
  
  // Memoization utility
  def memoize[A, B](f: A => B): A => B = {
    val cache = scala.collection.mutable.Map.empty[A, B]
    a => cache.getOrElseUpdate(a, f(a))
  }
  
  // Fixed-point combinator for recursive functions
  def fix[A, B](f: (A => B) => (A => B)): A => B = {
    lazy val result: A => B = f(result)
    result
  }
  
  // Y combinator (alternative fixed-point combinator)
  def Y[A, B](f: (A => B) => (A => B)): A => B = {
    case class Rec(h: Rec => A => B)
    val r = Rec(rec => f(rec.h(rec)))
    r.h(r)
  }
  
  // Trampolined recursion for stack-safe tail recursion
  sealed trait Trampoline[+A] {
    def runT: A = this match {
      case Done(a) => a
      case More(k) => k().runT
      case FlatMap(sub, k) => sub match {
        case Done(a) => k(a).runT
        case More(k2) => k2().flatMap(k).runT
        case FlatMap(sub2, k2) => sub2.flatMap(a => k2(a).flatMap(k)).runT
      }
    }
    
    def flatMap[B](f: A => Trampoline[B]): Trampoline[B] = FlatMap(this, f)
    def map[B](f: A => B): Trampoline[B] = flatMap(a => Done(f(a)))
  }
  
  case class Done[A](a: A) extends Trampoline[A]
  case class More[A](k: () => Trampoline[A]) extends Trampoline[A]
  case class FlatMap[A, B](sub: Trampoline[A], k: A => Trampoline[B]) extends Trampoline[B]
  
  object Trampoline {
    def apply[A](a: A): Trampoline[A] = Done(a)
  }
  
  // Safe recursion utilities using Trampoline
  def even(n: Int): Trampoline[Boolean] = 
    if (n == 0) Done(true)
    else if (n == 1) Done(false)
    else More(() => even(n - 2))
  
  def factorial(n: Int): Trampoline[BigInt] = {
    def loop(n: Int, acc: BigInt): Trampoline[BigInt] =
      if (n <= 1) Done(acc)
      else More(() => loop(n - 1, acc * n))
    loop(n, 1)
  }
  
  // Functional error handling utilities
  sealed trait Result[+E, +A] {
    def map[B](f: A => B): Result[E, B] = this match {
      case Success(a) => Success(f(a))
      case Error(e) => Error(e)
    }
    
    def flatMap[EE >: E, B](f: A => Result[EE, B]): Result[EE, B] = this match {
      case Success(a) => f(a)
      case Error(e) => Error(e)
    }
    
    def recover[AA >: A](f: E => AA): AA = this match {
      case Success(a) => a
      case Error(e) => f(e)
    }
    
    def toOption: Option[A] = this match {
      case Success(a) => Some(a)
      case Error(_) => None
    }
    
    def toEither: Either[E, A] = this match {
      case Success(a) => Right(a)
      case Error(e) => Left(e)
    }
  }
  
  case class Success[A](value: A) extends Result[Nothing, A]
  case class Error[E](error: E) extends Result[E, Nothing]
  
  object Result {
    def apply[A](a: A): Result[Nothing, A] = Success(a)
    
    def fromOption[A](opt: Option[A], error: => String): Result[String, A] = 
      opt.fold[Result[String, A]](Error(error))(Success(_))
    
    def fromEither[E, A](either: Either[E, A]): Result[E, A] = either match {
      case Right(a) => Success(a)
      case Left(e) => Error(e)
    }
    
    def attempt[A](f: => A): Result[Throwable, A] = 
      try Success(f) 
      catch { case t: Throwable => Error(t) }
    
    def sequence[E, A](results: List[Result[E, A]]): Result[E, List[A]] = {
      results.foldRight[Result[E, List[A]]](Success(Nil)) { (result, acc) =>
        for {
          a <- result
          list <- acc
        } yield a :: list
      }
    }
    
    def traverse[E, A, B](as: List[A])(f: A => Result[E, B]): Result[E, List[B]] =
      sequence(as.map(f))
  }
  
  // State monad for functional stateful computations
  case class State[S, A](run: S => (A, S)) {
    def map[B](f: A => B): State[S, B] = 
      State(s => {
        val (a, s2) = run(s)
        (f(a), s2)
      })
    
    def flatMap[B](f: A => State[S, B]): State[S, B] = 
      State(s => {
        val (a, s2) = run(s)
        f(a).run(s2)
      })
    
    def evalState(s: S): A = run(s)._1
    def execState(s: S): S = run(s)._2
  }
  
  object State {
    def apply[S, A](a: A): State[S, A] = State(s => (a, s))
    
    def get[S]: State[S, S] = State(s => (s, s))
    
    def put[S](s: S): State[S, Unit] = State(_ => ((), s))
    
    def modify[S](f: S => S): State[S, Unit] = 
      for {
        s <- get
        _ <- put(f(s))
      } yield ()
    
    def sequence[S, A](states: List[State[S, A]]): State[S, List[A]] =
      states.foldRight(State[S, List[A]](Nil))((state, acc) =>
        for {
          a <- state
          list <- acc
        } yield a :: list
      )
  }
  
  // Reader monad for dependency injection
  case class Reader[R, A](run: R => A) {
    def map[B](f: A => B): Reader[R, B] = Reader(r => f(run(r)))
    
    def flatMap[B](f: A => Reader[R, B]): Reader[R, B] = 
      Reader(r => f(run(r)).run(r))
  }
  
  object Reader {
    def apply[R, A](a: A): Reader[R, A] = Reader(_ => a)
    
    def ask[R]: Reader[R, R] = Reader(r => r)
    
    def local[R, A](f: R => R)(reader: Reader[R, A]): Reader[R, A] =
      Reader(r => reader.run(f(r)))
  }
  
  // Writer monad for computations with logging
  case class Writer[W, A](run: (A, W)) {
    def map[B](f: A => B): Writer[W, B] = Writer((f(run._1), run._2))
    
    def flatMap[B](f: A => Writer[W, B])(implicit M: Monoid[W]): Writer[W, B] = {
      val (a, w1) = run
      val (b, w2) = f(a).run
      Writer((b, M.combine(w1, w2)))
    }
    
    def value: A = run._1
    def written: W = run._2
  }
  
  object Writer {
    def apply[W, A](a: A)(implicit M: Monoid[W]): Writer[W, A] = 
      Writer((a, M.empty))
    
    def tell[W](w: W): Writer[W, Unit] = Writer(((), w))
    
    def listen[W, A](writer: Writer[W, A]): Writer[W, (A, W)] = {
      val (a, w) = writer.run
      Writer(((a, w), w))
    }
  }
  
  // Lazy evaluation utilities
  sealed trait Lazy[+A] {
    def value: A
    def map[B](f: A => B): Lazy[B] = Lazy(f(value))
    def flatMap[B](f: A => Lazy[B]): Lazy[B] = Lazy(f(value).value)
  }
  
  object Lazy {
    def apply[A](a: => A): Lazy[A] = new Lazy[A] {
      private lazy val cached = a
      def value = cached
    }
    
    def strict[A](a: A): Lazy[A] = new Lazy[A] {
      def value = a
    }
  }
  
  // Stream utilities for lazy sequences
  sealed trait Stream[+A] {
    def head: Option[A]
    def tail: Stream[A]
    def isEmpty: Boolean
    
    def map[B](f: A => B): Stream[B] = this match {
      case Empty => Empty
      case Cons(h, t) => Cons(() => f(h()), () => t().map(f))
    }
    
    def filter(p: A => Boolean): Stream[A] = this match {
      case Empty => Empty
      case Cons(h, t) =>
        if (p(h())) Cons(h, () => t().filter(p))
        else t().filter(p)
    }
    
    def take(n: Int): Stream[A] = this match {
      case Empty => Empty
      case Cons(h, t) =>
        if (n <= 0) Empty
        else Cons(h, () => t().take(n - 1))
    }
    
    def toList: List[A] = this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }
  }
  
  case object Empty extends Stream[Nothing] {
    def head: Option[Nothing] = None
    def tail: Stream[Nothing] = Empty
    def isEmpty: Boolean = true
  }
  
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
    def head: Option[A] = Some(h())
    def tail: Stream[A] = t()
    def isEmpty: Boolean = false
  }
  
  object Stream {
    def empty[A]: Stream[A] = Empty
    
    def cons[A](head: => A, tail: => Stream[A]): Stream[A] = 
      Cons(() => head, () => tail)
    
    def apply[A](as: A*): Stream[A] = 
      if (as.isEmpty) Empty
      else cons(as.head, apply(as.tail: _*))
    
    def from(n: Int): Stream[Int] = cons(n, from(n + 1))
    
    def repeat[A](a: A): Stream[A] = cons(a, repeat(a))
    
    def iterate[A](a: A)(f: A => A): Stream[A] = 
      cons(a, iterate(f(a))(f))
    
    def unfold[A, B](z: A)(f: A => Option[(B, A)]): Stream[B] = 
      f(z) match {
        case None => Empty
        case Some((b, a)) => cons(b, unfold(a)(f))
      }
  }
  
  // Parallel computation utilities
  sealed trait Par[+A] {
    def map[B](f: A => B): Par[B] = Par.map(this)(f)
    def flatMap[B](f: A => Par[B]): Par[B] = Par.flatMap(this)(f)
  }
  
  object Par {
    private case class UnitPar[A](a: A) extends Par[A]
    private case class Fork[A](a: () => Par[A]) extends Par[A]
    private case class Map[A, B](pa: Par[A], f: A => B) extends Par[B]
    
    def unit[A](a: A): Par[A] = UnitPar(a)
    
    def fork[A](a: => Par[A]): Par[A] = Fork(() => a)
    
    def map[A, B](pa: Par[A])(f: A => B): Par[B] = Map(pa, f)
    
    def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = 
      fork(map(pa)(f)).asInstanceOf[Par[B]] // Simplified implementation
    
    def run[A](pa: Par[A]): A = pa match {
      case UnitPar(a) => a
      case Fork(a) => run(a())
      case Map(pa, f) => f(run(pa))
    }
    
    def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] = {
      val pars = as.map(a => fork(unit(f(a))))
      sequence(pars)
    }
    
    def sequence[A](pas: List[Par[A]]): Par[List[A]] = 
      pas.foldRight(unit(List.empty[A]))((pa, acc) => 
        map2(pa, acc)(_ :: _))
    
    def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
      map(pa)(a => map(pb)(b => f(a, b))).asInstanceOf[Par[C]] // Simplified
  }
  
  // Utility functions for common functional patterns
  def identity[A]: A => A = a => a
  
  def const[A, B](a: A): B => A = _ => a
  
  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))
  
  def pipe[A, B](a: A)(f: A => B): B = f(a)
  
  def tap[A](a: A)(f: A => Unit): A = { f(a); a }
  
  def when[A](condition: Boolean)(action: => A): Option[A] = 
    if (condition) Some(action) else None
  
  def unless[A](condition: Boolean)(action: => A): Option[A] = 
    when(!condition)(action)
  
  def times(n: Int)(action: => Unit): Unit = 
    (1 to n).foreach(_ => action)
  
  def retry[A](maxAttempts: Int)(action: => A): Option[A] = {
    def loop(attempts: Int): Option[A] = 
      if (attempts <= 0) None
      else try Some(action)
      catch { case _: Exception => loop(attempts - 1) }
    
    loop(maxAttempts)
  }
  
  // Additional utility functions for better composition
  def tupleWith[A, B, C](f: A => B, g: A => C): A => (B, C) = 
    a => (f(a), g(a))
  
  def either[A, B, C](f: A => C, g: B => C): Either[A, B] => C = {
    case Left(a) => f(a)
    case Right(b) => g(b)
  }
  
  def option[A, B](default: B, f: A => B): Option[A] => B = {
    case None => default
    case Some(a) => f(a)
  }
}