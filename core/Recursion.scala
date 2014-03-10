/**
 * All rights reserved by Qiuzhuang.Lian
 */

def insertionSort(xs: List[Int]): List[Int] = {

  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => List(x)
    case y :: ys => if (x < y) x :: xs else y :: insert(x, ys)
  }

  if (xs.isEmpty) xs
  else insert(xs.head, insertionSort(xs.tail))
}

def mergeSort[A](less: (A, A) => Boolean)(xs: List[A]): List[A] = {
  def merge(xs1: List[A], xs2: List[A]): List[A] = {
    if (xs1.isEmpty) xs2
    else if (xs2.isEmpty) xs1
    else if (less(xs1.head, xs2.head)) xs1.head :: merge(xs1.tail, xs2)
    else xs2.head :: merge(xs1, xs2.tail)
  }

  val m = xs.length / 2
  if (m == 0) xs
  else merge(mergeSort(less)(xs.take(m)), mergeSort(less)(xs.drop(m)))
}

