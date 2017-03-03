package fpinscala.datastructures

// Exercise 3.1: 3

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  } 
  
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42 
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101 
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  
  def sum2(ns: List[Int]) = 
    foldRight(ns, 0)((x,y) => x + y)
  
  def product2(ns: List[Double]) = 
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  // Exercise 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => l // Book answer gives an error instead
    case Cons(_, xs) => xs
  }

  // Exercise 3.3
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("Can't set head of empty list")
    case Cons(_, xs) => Cons(h, xs)
  }

  // Exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l // Book answer checks if n <= 0. In cases where n < 0, might make more sense to give an error.
    else l match {
      case Nil => Nil
      case Cons(_, xs) => drop(xs, n-1)
    }
  }

  // Exercise 3.5
  // Book answer uses pattern guard
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) =>
      if (f(x)) dropWhile(xs, f)
      else l
  }

  // Exercise 3.6
  // This implementation can cause a stack overflow. Book suggests using list buffer.
  def init[A](l: List[A]): List[A] = l match {
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
    case _ => l
  }

  // Exercise 3.7: No, it's not possible since foldRight traverses the entire list before f is evaluated.

  // Exercise 3.8: You get the original list

  // Exercise 3.9
  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_, n) => n+1)
  }

  // Exercise 3.10
  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  // Exercise 3.11
  def sum3(l: List[Int]): Int = foldLeft(l, 0)((b, a) => b + a) // can replace (b, a) => b + a with _ + _ etc
  def product3(l: List[Double]): Double = foldLeft(l, 1.0)((b, a) => b * a)
  def length2[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

  // Exercise 3.12
  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((b, a) => Cons(a, b))

  // Exercise 3.13: TODO

  // Exercise 3.14
  def append[A](l: List[A], n: A): List[A] =
    foldLeft(reverse(l), List(n))((b, a) => Cons(a, b))

  // Exercise 3.15: TODO: revisit simplifying this
  def concat[B](ls: List[B]*): List[B] =
    foldRight(List(ls: _*), Nil: List[B])((a, b) => append(a, b))

  // Exercise 3.16: TODO: revisit using foldRight
  def addOne(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x + 1, addOne(xs))
  }

  // Exercise 3.17
  def toStrings(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((d, t) => Cons(d.toString(), t))

  // Exercise 3.18: TODO: Look at other ways of implementing this
  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))

  // Exercises 3.19 - 3.24: TODO
}
