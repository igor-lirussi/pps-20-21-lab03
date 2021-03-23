package u03

import u02.SumTypes._

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
    def mapNew[A,B](l: List[A])(mapper: A=>B): List[B] = flatMap(l)(x => Cons( mapper(x), Nil()))


    //passo alla flatmap una funione che va da elemento generico a lista di elemento generico, ma solo se predicato vero, altrimenti va a lista vuota
    def filterNew[A](l1: List[A])(pred: A=>Boolean): List[A] = flatMap(l1)(G=>G match {
      case G if pred(G) => Cons(G, Nil())
      case _ => Nil()
    })

    //definizione di funzione ricorsiva, scorre la lista e se la testa è maggiore del current max questo viene aggiornato
    def max(l: List[Int]): Option[Int] = {
      def maxRec(l: List[Int])(max:Int): Int = l match {
        case Cons(head, tail) if head>max => maxRec(tail)(head)
        case Cons(head, tail)  => maxRec(tail)(max)
        case Nil() => max
      }
      l match {
        case Cons(head, _) => Some(maxRec(l)(head))
        case Nil() => None
      }
    }

    //upper type bound, function to the flatmap that goes to list with course if teacher, otherwise nothing
    def coursesTaught[P<:Person](ppl: List[P]): List[String] = flatMap(ppl)(pers => pers match {
      case Teacher(name, course) => Cons(course, Nil())
      case _ => Nil()
    })

    def foldLeft[E](list: List[E])(init:E)(operator: (E,E)=>E): E = list match {
      case Cons(head, tail) => /*; println(init + "op" + head + " is "+operator(init, head));*/ foldLeft(tail)( operator(init, head))(operator)
      case Nil() => /*println( "ritorno " + init);*/ init
    }
    //*
    //provato foldLeft[E,A>:E](list: List[E])(init:A)(operator: (A,E)=>A): A , cercando di lasciare che init sia o un tipo della lista o un tipo boolenano
    //problema qui, per tornare init nel caso lista vuota senza aver fatto operazioni init è ancora di tipo E (es int) e non A (es:boolean)
    //se la lista non è vuota alla prima operazione si prendono i due E e si da in out un A, quindi init è A
    //causato dal fatto che uso init stesso come accumulatore, quindi se il primo elemento è 0:Int ma sto accumulando Booleani


    def foldRightReverse[E](list: List[E])(init:E)(operator: (E,E)=>E): E =  {
      foldLeft(reverse(list))(init)(operator)
    }

    def reverse[E] (list: List[E]): List[E] = {
        def reverseRec (list: List[E])(listRev: List[E]): List[E] = list match {
          case Cons(head, tail) => reverseRec(tail)(Cons(head, listRev))
          case Nil() => listRev
        }
      reverseRec(list)(Nil())
    }

    def foldRight[E](list: List[E])(init:E)(operator: (E,E)=>E): E = list match {
      case Cons(head, tail) => operator(head, foldRight(tail)(init)(operator))
      case Nil() => init  //*
    }
    

  }


}
