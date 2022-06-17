import scala.annotation.tailrec

enum Set[+A]:
  case Empty
  case NonEmpty private[Set](a: A, rest: Set[A])

object Set:
  def makeSet[A](xs: A*): Set[A] = {
    if (xs.isEmpty) Empty
    else xs.foldRight(Empty: Set[A])((el, z) => insert(z, el))
  }

  def foldRight[A, B](set: Set[A], z: B)(f: (A, B) => B): B =
    set match {
      case Empty => z
      case NonEmpty(x, xs) => f(x, foldRight(xs, z)(f))
    }

  @tailrec
  def foldLeft[A, B](set: Set[A], z: B)(f: (B, A) => B): B =
    set match {
      case Empty => z
      case NonEmpty(h, t) => foldLeft(t, f(z, h))(f)
    }

  def toString[A](set: Set[A]): String = {
    foldLeft(set, "")((z, x) =>
      if (z.isEmpty) z + x.toString
      else z + " " + x.toString)
  }

  def contain[A](set: Set[A], el: A): Boolean = {
    foldLeft(set, false)((z, x) => if (x == el) true else z)
  }

  def insert[A](set: Set[A], el: A): Set[A] = {
    if (!contain(set, el)) {
      NonEmpty(el, set)
    }
    else set
  }

  def join[A](xs: Set[A], prefix: String, separator: String, suffix: String): String = {
    prefix + (xs match {
      case Empty => "" 
      case NonEmpty(a, rest) => a.toString + foldLeft(rest, "")((str, b) => str + separator + b.toString)
    })+ suffix
  }

  def isSubsetOf[A](xs: Set[A], ys: Set[A]): Boolean = {
    foldLeft(xs, true)((b, x) => if (contain(ys, x)) b else false)
  }

  def disjoint[A](xs: Set[A], ys: Set[A]): Boolean = {
    foldLeft(xs, true)((b, x) => if (!contain(ys, x)) b else false)
  }

@main def run() = 
  println(
  Set.join(Set.makeSet[Int](1,2,3), "[", ", ", "]")
  )
