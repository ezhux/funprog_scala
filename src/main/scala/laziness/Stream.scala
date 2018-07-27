trait Stream[+A] {

  // 5.1
  def toList: List[A] = this match {
    case Cons(h,t) => h() :: t().toList
    case _ => List()
  }

  // 5.2
  def take(n: Int): Stream[A] = {
    val buff: Stream[A] = Empty
    def takeMore(to: Stream[A], from: Stream[A], current: Int): Stream[A] = {
      if (current < n) {
        from match {
          case Cons(h,t) => {
            takeMore(Stream.cons(h(), to), t(), current + 1)
          }
          case _ => takeMore(from, Empty, n+1)
        }
      } else {
        to
      }
    }
    takeMore(buff, this, 0)
  }

  def takeNthElement(n: Int): Option[A] = {
    def find(s: Stream[A], current: Int): Option[A] = {
      if (current < n) {
        s match {
          case Cons(h,t) => find(t(), current + 1)
          case _ => None
        }
      } else {
        s match {
          case Cons(h,t) => Some(h())
        }
      }
    }
    find(this, 0)
  }

  // 5.4
  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) if p(h()) => {
      Console.println("checking: " + h())
      t().forAll(p)
    }
    case Cons(h, t) => false
    case _ => false
  }

  def foldRight[B](z : => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def forAll2(f: A => Boolean): Boolean =
    foldRight(true)((a,b) => f(a) && b)

  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((h,t) =>
      if (p(h)) Stream.cons(h,t)
      else      Stream.empty)



}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  // 5.8
  def constant[A](a: A): Stream[A] = {
    Stream.cons(a, constant(a))
  }

  // 5.9
  //  def from(n: Int): Stream[Int] = {
  //
  //  }
}
