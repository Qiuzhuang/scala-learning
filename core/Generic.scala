/**
 * All rights reserved by Qiuzhuang.Lian
 */

// upper bound(T <: U), A should be sub-type of Ordered[A],
// its opposite side(T >: S) is lower bound, means super-type.
// its combined type is (T >: S <: U)
trait Set[A <: Ordered[A]] {
  def incl(x: A): Set[A]
  def contains(x: A): Boolean
}

// view type is weaker than upper type, it means implicit convertible
trait Set[A <% Ordered[A]] ...
class EmptySet[A <% Ordered[A]] ...
class NonEmptySet[A <% Ordered[A]] ...

//Nothing is a subtype of every other type, hence EmptyStack is co-variant
object EmptyStack extends Stack[Nothing] { ... }

// complete example

abstract class Stack[+A] {
  def push[B >: A](x: B): Stack[B] = new NonEmptyStack(x, this)
  def isEmpty: Boolean
  def top: A
  def pop: Stack[A]
}
object EmptyStack extends Stack[Nothing] {
  def isEmpty = true
  def top = error("EmptyStack.top")
  def pop = error("EmptyStack.pop")
}
class NonEmptyStack[+A](elem: A, rest: Stack[A]) extends Stack[A] {
  def isEmpty = false
  def top = elem
  def pop = rest
}

// tuble
(x1, ..., xn) == Tuble(x1, ..., xn)

case class TwoInts(first: Int, second: Int)
def divmod(x: Int, y: Int): TwoInts = new TwoInts(x / y, x % y)

val v = divmod(1, 2)
val v1 = v._1
val v2 = v._2

divmod(x, y) match {
  case (n, d) => println("quotient: " + n + ", rest: " + d)
}

// function
(T1, ..., Tn) => S == Functionn[T1,... Tn, S]

val f: (AnyRef => Int) = x => x.hashCode()
val g: (String => Int) = f
g("abc")

val plus1: (Int => Int) = (x: Int) => x + 1
plus1(2)

val plus1: Function1[Int, Int] = new Function1[Int, Int] {
  def apply(x: Int): Int = x + 1
}
plus1.apply(2)



