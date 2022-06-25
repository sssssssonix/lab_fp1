import scala.annotation.tailrec

enum Set[+A]:
  case Empty
  case NonEmpty private[Set](a: A, cnt: Int, rest: Set[A])

object Set:
  def makeSet[A](xs: A*): Set[A] = {
    if (xs.isEmpty) Empty
    else xs.foldRight(Empty: Set[A])((el, z) => insert(z, el))
  }

  @tailrec
  def foldLeft[A, B](set: Set[A], z: B)(f: (B, A, Int) => B): B =
    set match {
      case Empty => z
      case NonEmpty(h, cnt, t) => foldLeft(t, f(z, h, cnt))(f)
    }

  def toString[A](set: Set[A]): String = {
    foldLeft(set, "")((z, x, cnt) =>
      if (z.isEmpty) z + x.toString + "x" + cnt.toString
      else z + " " + x.toString + "x" + cnt.toString)
  }
  
  def contain[A](set: Set[A], el: A): Boolean = {
    foldLeft(set, false)((z, x, cnt) => if (x == el) true else z)
  }
  
  def insert[A](set: Set[A], el: A, count: Int = 1): Set[A] = {
    if (contain(set, el))
      foldLeft(set, Empty: Set[A])((out, value, cnt) => 
        if (value != el) NonEmpty(value, cnt, out)
        else NonEmpty(value, cnt + count, out))
    else
      NonEmpty(el, count, set)
  }

  def mostCommonElement[A](set: Set[A]): Option[A] = {
    foldLeft(set, (None: Option[A], 0))((tup, value, cnt) => 
      if (cnt > tup._2)
        (Some(value), cnt)
      else
        tup)._1
  }
  
  def getCountOfElement[A](set: Set[A], el: A): Int = {
    foldLeft(set, 0)((count, value, cnt) => 
      if (value == el) cnt
      else count)
  }
  
  def union[A](firstSet: Set[A], secondSet: Set[A]): Set[A] = {
    foldLeft(secondSet, firstSet)((set, value, cnt) => insert(set, value, cnt))
  }
  
  def intersection[A](firstSet: Set[A], secondSet: Set[A]): Set[A] = {
    foldLeft(secondSet, Empty: Set[A])((out, value, cnt) => 
      if (contain(firstSet, value))
        NonEmpty(value, cnt.min(getCountOfElement(firstSet, value)), out)
      else
        out)
  }
  
  def difference[A](firstSet: Set[A], secondSet: Set[A]): Set[A] = {
    foldLeft(firstSet, Empty: Set[A])((out, value, cnt) => 
      if (getCountOfElement(secondSet, value) < cnt)
        NonEmpty(value, cnt - getCountOfElement(secondSet, value), out)
      else
        out)
  }
  
  
@main def run() = 
  println(
  Set.getCountOfElement(Set.makeSet[Int](1,2,5,5,6,5,5,6),5)
  )
