package monoid

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    def zero = ""
  }

  //Exercise 10.1

  val intAddition = new Monoid[Int] {
    def op (i1: Int, i2: Int) = i1 + i2
    def zero = 0
  }

  val intMultiplication = new Monoid[Int] {
    def op (i1: Int, i2: Int) = i1 * i2
    def zero = 1
  }

  val booleanOr = new Monoid[Boolean] {
    def op (b1: Boolean, b2: Boolean) = b1 || b2
    def zero = false
  }

  val booleanAnd = new Monoid[Boolean] {
    def op (b1: Boolean, b2: Boolean) = b1 && b2
    def zero = true
  }

  // Exercise 10.2

  def optionMonoid[A] = new Monoid[Option[A]] {
    def op (o1: Option[A], o2: Option[A]): Option[A] = o1 orElse o2
    def zero = None
  }

  // Exercise 10.3

  def endoMonoid[A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A) = a1 compose a2
    def zero = (a: A) => a
  }

  // Exercise 10.4

  // Exercise 10.5

  def foldMap[A,B](as: List[A], m : Monoid[B])(f: A => B): B = {
    as.map { f(_)  }.foldLeft(m.zero)(m.op)
  }

  // Exercise 10.6

  // Ex 10.7

  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    val (l, r)  = v.splitAt(v.length / 2)
    m.op(foldMapV(l, m)(f), foldMapV(r,m)(f))
  }

  // Ex 10.10

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lstub: String, words: Int, rstub: String) extends WC

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(l: WC, r: WC) = {
      (l, r) match {
        case (l: Stub, r: Stub) => Stub(l.chars + r.chars)
        case (l: Part, r: Stub) => l.copy(rstub = l.rstub + r.chars)
        case (l: Stub, r: Part) => r.copy(lstub = r.lstub + l.chars)
        case (l: Part, r: Part) => Part(l.lstub + r.lstub, l.words + r.words, l.rstub + r.rstub)
      }
    }
      def zero: WC = Stub("")
  }

//  Ex 10.11

//  def splitAndCount(a: String, wc: Monoid[WC]): Int = {
//    val (l, r) = (a.substring(0, a.length / 2), a.substring(a.length / 2, a.length))
//  }
}
