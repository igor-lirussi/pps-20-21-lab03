package u03

object MyLists  extends App{

  // A generic linkedlist
  sealed trait List[E]

  // a companion object (i.e., module) for List
  object MyList {
    case class Cons[E](head: E, tail: List[E]) extends List[E]
    case class Nil[E]() extends List[E]

    def sum(l: List[Int]): Int = l match {
      case Cons(h, t) => h + sum(t)
      case _ => 0
    }

    def append[A](l1: List[A], l2: List[A]): List[A] = (l1, l2) match {
      case (Cons(h, t), l2) => Cons(h, append(t, l2))
      case _ => l2
    }

    def drop[A](l: List[A], n: Int): List[A] = l match {
      case Cons(_,t) if n>0 => drop(t,n-1)
      case Cons(h,t) => Cons(h,t)
      case Nil() => Nil()
    }

    def flatMap[A,B] (l: List[A])(f: A => List[B]) : List[B] = l match {
      case Cons(h,t) => append( f(h),flatMap(t)(f) )
      case Nil() => Nil()
    }

    //mapper va da elemto a elemento, la flat map vuole da elemento a lista
    //in ingresso funzione da  "x" a Lista( x mappato, nil)
    def map[A,B](l: List[A])(mapper: A=>B): List[B] = flatMap(l)( x => Cons( mapper(x), Nil()))


    def filter[A](l1: List[A])(pred: A=>Boolean): List[A] = l1 match {
      case Cons(h,t) if (pred(h)) => Cons(h, filter(t)(pred))
      case Cons(_,t) => filter(t)(pred)
      case Nil() => Nil()
    }
  }

}
