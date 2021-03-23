package u03

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import u03.Lists.List._
import u03.MyStreams._

class TestMyStreams {


  @Test def dropTest(): Unit = {
    val s = MyStream.take( MyStream. iterate (0) ( _ +1) ) (10)
    val list = MyStream.toList ( MyStream.drop( s ) (6) )
    assertEquals(Cons(6 , Cons (7 , Cons (8 , Cons (9 , Nil())))), list)
  }

  @Test def constantTest(): Unit = {
    val list = MyStream.toList ( MyStream.take ( MyStream.constant("x") ) (5) )
    assertEquals( Cons ("x", Cons ("x", Cons ("x", Cons ("x", Cons ("x", Nil ()))))), list)
  }

  @Test def fibonacciTest(): Unit ={
    val list = MyStream.toList( MyStream.take( MyStream.fibon(1)(1) ) (8))
    assertEquals(Cons(1,Cons(2,Cons(3,Cons(5,Cons(8,Cons(13,Cons(21,Cons(34,Nil())))))))), list)
    println(MyStream.toList( MyStream.take( MyStream.fibon(5)(0) ) (8)))
    val list2 = MyStream.toList ( MyStream.take ( MyStream.fibs ) (8) )
    assertEquals(Cons (0 , Cons (1 , Cons (1 , Cons (2 , Cons (3 , Cons (5 , Cons (8 , Cons (13 , Nil ())))))))) , list2)

    println(MyStream.toList( MyStream.take( MyStream.fibonReverse(1)(0) ) (8)))
  }

}
