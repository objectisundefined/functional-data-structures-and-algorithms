package fpinscala.typeclass

// Functor type class
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
  
  // Derived operations
  def lift[A, B](f: A => B): F[A] => F[B] = fa => map(fa)(f)
  
  def as[A, B](fa: F[A], b: B): F[B] = map(fa)(_ => b)
  
  def void[A](fa: F[A]): F[Unit] = as(fa, ())
}

// Monad type class
trait Monad[F[_]] extends Functor[F] {
  def pure[A](a: A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  
  // Default implementation of map using flatMap
  override def map[A, B](fa: F[A])(f: A => B): F[B] = 
    flatMap(fa)(a => pure(f(a)))
  
  // Derived operations
  def flatten[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(identity)
  
  def sequence[A](fas: List[F[A]]): F[List[A]] = 
    fas.foldRight(pure(List.empty[A]))((fa, acc) => 
      flatMap(fa)(a => map(acc)(a :: _)))
  
  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] = 
    sequence(as.map(f))
  
  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))
  
  def filterM[A](as: List[A])(f: A => F[Boolean]): F[List[A]] =
    traverse(as)(a => map(f(a))(if (_) List(a) else Nil)).map(_.flatten)
}

// Monoid type class
trait Monoid[A] {
  def empty: A
  def combine(x: A, y: A): A
  
  // Derived operations
  def combineAll(as: List[A]): A = as.foldLeft(empty)(combine)
  
  def combineN(a: A, n: Int): A = 
    if (n <= 0) empty
    else if (n == 1) a
    else {
      val half = combineN(a, n / 2)
      if (n % 2 == 0) combine(half, half)
      else combine(a, combine(half, half))
    }
}

// Foldable type class
trait Foldable[F[_]] {
  def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B
  def foldRight[A, B](fa: F[A], b: B)(f: (A, B) => B): B
  
  // Derived operations
  def foldMap[A, B](fa: F[A])(f: A => B)(implicit M: Monoid[B]): B =
    foldLeft(fa, M.empty)((b, a) => M.combine(b, f(a)))
  
  def fold[A](fa: F[A])(implicit M: Monoid[A]): A = 
    foldMap(fa)(identity)
  
  def toList[A](fa: F[A]): List[A] = 
    foldRight(fa, List.empty[A])(_ :: _)
  
  def isEmpty[A](fa: F[A]): Boolean = 
    foldRight(fa, true)((_, _) => false)
  
  def length[A](fa: F[A]): Int = 
    foldLeft(fa, 0)((n, _) => n + 1)
  
  def exists[A](fa: F[A])(p: A => Boolean): Boolean =
    foldRight(fa, false)((a, b) => p(a) || b)
  
  def forall[A](fa: F[A])(p: A => Boolean): Boolean =
    foldRight(fa, true)((a, b) => p(a) && b)
  
  def find[A](fa: F[A])(p: A => Boolean): Option[A] =
    foldRight(fa, None: Option[A])((a, b) => if (p(a)) Some(a) else b)
}

// Traversable type class
trait Traversable[F[_]] extends Functor[F] with Foldable[F] {
  def traverse[G[_]: Monad, A, B](fa: F[A])(f: A => G[B]): G[F[B]]
  
  def sequence[G[_]: Monad, A](fga: F[G[A]]): G[F[A]] = 
    traverse(fga)(identity)
}

// Type class instances for common data structures

// Option instances
object OptionInstances {
  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }
  
  implicit val optionMonad: Monad[Option] = new Monad[Option] {
    def pure[A](a: A): Option[A] = Some(a)
    def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
  }
}

// List instances
object ListInstances {
  implicit val listFunctor: Functor[List] = new Functor[List] {
    def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }
  
  implicit val listMonad: Monad[List] = new Monad[List] {
    def pure[A](a: A): List[A] = List(a)
    def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
  }
  
  implicit val listFoldable: Foldable[List] = new Foldable[List] {
    def foldLeft[A, B](fa: List[A], b: B)(f: (B, A) => B): B = fa.foldLeft(b)(f)
    def foldRight[A, B](fa: List[A], b: B)(f: (A, B) => B): B = fa.foldRight(b)(f)
  }
  
  implicit def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def empty: List[A] = Nil
    def combine(x: List[A], y: List[A]): List[A] = x ::: y
  }
}

// Common monoid instances
object MonoidInstances {
  implicit val intAddition: Monoid[Int] = new Monoid[Int] {
    def empty: Int = 0
    def combine(x: Int, y: Int): Int = x + y
  }
  
  implicit val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def empty: Int = 1
    def combine(x: Int, y: Int): Int = x * y
  }
  
  implicit val stringMonoid: Monoid[String] = new Monoid[String] {
    def empty: String = ""
    def combine(x: String, y: String): String = x + y
  }
  
  implicit def optionMonoid[A](implicit M: Monoid[A]): Monoid[Option[A]] = new Monoid[Option[A]] {
    def empty: Option[A] = None
    def combine(x: Option[A], y: Option[A]): Option[A] = (x, y) match {
      case (None, None) => None
      case (Some(a), None) => Some(a)
      case (None, Some(b)) => Some(b)
      case (Some(a), Some(b)) => Some(M.combine(a, b))
    }
  }
  
  implicit def functionMonoid[A, B](implicit M: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    def empty: A => B = _ => M.empty
    def combine(f: A => B, g: A => B): A => B = a => M.combine(f(a), g(a))
  }
}

// Syntax for type classes
object syntax {
  implicit class FunctorOps[F[_], A](fa: F[A])(implicit F: Functor[F]) {
    def map[B](f: A => B): F[B] = F.map(fa)(f)
    def as[B](b: B): F[B] = F.as(fa, b)
    def void: F[Unit] = F.void(fa)
  }
  
  implicit class MonadOps[F[_], A](fa: F[A])(implicit M: Monad[F]) {
    def flatMap[B](f: A => F[B]): F[B] = M.flatMap(fa)(f)
    def >>=[B](f: A => F[B]): F[B] = M.flatMap(fa)(f)
  }
  
  implicit class MonoidOps[A](a: A)(implicit M: Monoid[A]) {
    def |+|(b: A): A = M.combine(a, b)
  }
  
  implicit class FoldableOps[F[_], A](fa: F[A])(implicit F: Foldable[F]) {
    def foldLeft[B](b: B)(f: (B, A) => B): B = F.foldLeft(fa, b)(f)
    def foldRight[B](b: B)(f: (A, B) => B): B = F.foldRight(fa, b)(f)
    def foldMap[B](f: A => B)(implicit M: Monoid[B]): B = F.foldMap(fa)(f)
    def fold(implicit M: Monoid[A]): A = F.fold(fa)
    def toList: List[A] = F.toList(fa)
    def isEmpty: Boolean = F.isEmpty(fa)
    def length: Int = F.length(fa)
    def exists(p: A => Boolean): Boolean = F.exists(fa)(p)
    def forall(p: A => Boolean): Boolean = F.forall(fa)(p)
    def find(p: A => Boolean): Option[A] = F.find(fa)(p)
  }
}

// Helper object for working with type classes
object TypeClass {
  def apply[TC[_], F[_]](implicit tc: TC[F]): TC[F] = tc
  
  // Common combinators
  def pure[F[_]: Monad, A](a: A): F[A] = Monad[F].pure(a)
  
  def sequence[F[_]: Monad, A](fas: List[F[A]]): F[List[A]] = 
    Monad[F].sequence(fas)
  
  def traverse[F[_]: Monad, A, B](as: List[A])(f: A => F[B]): F[List[B]] = 
    Monad[F].traverse(as)(f)
  
  // Lift functions to work with functors
  def lift2[F[_]: Functor, A, B, C](f: (A, B) => C): (F[A], F[B]) => F[C] = {
    (fa, fb) => Functor[F].map(fa)(a => Functor[F].map(fb)(b => f(a, b))).asInstanceOf[F[C]] // Note: This is simplified, proper implementation would use Applicative
  }
}

// Companion objects for easy access
object Functor {
  def apply[F[_]](implicit F: Functor[F]): Functor[F] = F
}

object Monad {
  def apply[F[_]](implicit M: Monad[F]): Monad[F] = M
}

object Monoid {
  def apply[A](implicit M: Monoid[A]): Monoid[A] = M
}

object Foldable {
  def apply[F[_]](implicit F: Foldable[F]): Foldable[F] = F
}