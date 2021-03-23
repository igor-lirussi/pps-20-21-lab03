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


}
