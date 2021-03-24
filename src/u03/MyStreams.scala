package u03

import u03.Streams.Stream

object MyStreams {
  import Lists._
  sealed trait Stream[A]

  object MyStream {
    private case class Empty[A]() extends Stream[A]
    private case class Cons[A](head: () => A, tail: () => Stream[A]) extends Stream[A]

    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def toList[A](stream: Stream[A]): List[A] = stream match {
      case Cons(h,t) => List.Cons(h(), toList(t()))
      case _ => List.Nil()
    }

    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match {
      case Cons(head, tail) => cons(f(head()), map(tail())(f))
      case _ => Empty()
    }

    def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match {
      case Cons(head, tail) if (pred(head())) => cons(head(), filter(tail())(pred))
      case Cons(head, tail) => filter(tail())(pred)
      case _ => Empty()
    }

    def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream,n) match {
      case (Cons(head, tail), n) if n>0 => cons(head(), take(tail())(n - 1))
      case _ => Empty()
    }

    def iterate[A](init: => A)(next: A => A): Stream[A] = cons(init, iterate(next(init))(next))

    def drop[A](stream: Stream[A])(n: Int): Stream[A] = (stream,n) match {
      case (Cons(head, tail), n) if n>0 => drop(tail())(n - 1)
      case (Cons(head, tail), n) => cons( head() , drop(tail())(n - 1) )
      case _ => Empty()
    }

    def constant[A] (const : A) : Stream[A] = cons(const, iterate(const)(x=>x))

    //        1           0                                 1       1+0=1    1
    //        1           1                                 1       1+1=2    1
    //        2           1                                 2       2+1=3    2
    //        3           2                                 3       3+2=5    3
    //        5           3                                 5       5+3=8    5
    def fibon(n: => Int)(prev: => Int): Stream[Int] = cons(n, fibon(n+prev)(n))
    //somma i due precenenti

    val fibs : Stream [Int] = fibon(0)(1) //si può costruire fibonacci con il precedente di 0 come 1,
    // così il successivo costruito è 0+1, quindi esattamente 1 con cui iniziare la sequenza normale 0,1,...
    //..., 5, 3, 2, 1, 1, 0, 1.
    // si può andare indietro, cerco il numero che sommato a 1 da 0, quindi -1
    //1,1,0, 1, -1, 2, -3, 5, -8, 13, ...
    //il successivo andando all'indietro è l'attuale meno il precedente(-1 = 0 - 1)

    def fibonReverse(n: => Int)(prev: => Int): Stream[Int] = cons(n, fibonReverse(prev-n)(n))


  }
}
