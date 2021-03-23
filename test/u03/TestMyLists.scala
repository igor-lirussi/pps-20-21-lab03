package u03

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import u02.SumTypes._

class TestMyLists {


  import MyLists.MyList._

  val lst = Cons(10 , Cons(20 , Cons(30, Nil() )) )

  @Test def testDrop(){
    val lst = Cons(10 , Cons(20 , Cons(30, Nil() )) )
    assertEquals( Cons(20, Cons(30, Nil())) , drop(lst,1) )
    assertEquals( Cons(30, Nil()) , drop(lst,2))
    assertEquals( Nil(), drop(lst, 5))
  }

  @Test def testFlatMap(){
    assertEquals( Cons(11 , Cons(21 , Cons(31 , Nil()))),
      flatMap( lst )(v => Cons ( v+1 , Nil () )))  // Cons (11 , Cons (21 , Cons (31 , Nil ())))
    assertEquals( Cons(11 , Cons(12 , Cons(21 , Cons(22 , Cons(31 , Cons(32 , Nil())))))),
      flatMap ( lst )(v => Cons ( v+1 , Cons (v+2 , Nil () ))))
    // Cons (11 , Cons (12 , Cons (21 , Cons (22 , Cons (31 , Cons (32 , Nil ()))))))
  }

  @Test def testNewMap(): Unit ={
    assertEquals(Cons(11 , Cons(21 , Cons(31 , Nil()))),mapNew(lst)(_+1))
    assertEquals(Cons(9 , Cons(19 , Cons(29 , Nil()))),mapNew(lst)(_-1))
  }

  @Test def testNewFilter(): Unit ={
    assertEquals(Cons(10 , Cons(20 , Cons(30 , Nil()))), filterNew(lst)(_%2==0))
    assertEquals(Cons(10 ,  Nil()), filterNew(lst)(_==10))
  }

  @Test def maxTest(): Unit ={
    assertEquals(Some(25), max( Cons(10 , Cons(25 , Cons(20 , Nil() ))))) // Some (25)
    assertEquals(None, max(Nil()) )
  }


  @Test def coursesTaughtTest(): Unit = {
    val none = Nil()
    assertEquals(Nil(), coursesTaught(none))
    val ppl = Cons(Teacher("Tizio", "Geografia"), Cons(Teacher("Tizio", "Storia"), Nil()))
    assertEquals(Cons("Geografia" , Cons ( "Storia", Nil () )), coursesTaught(ppl))
    val ppl2: Cons[Person with Product]= Cons ( Student("s1",10) , Cons (Teacher("Andrea", "Geografia") , Cons ( Student("s2",20), Cons (Teacher("Bianchi", "Storia") , Cons ( Student("s3",30), Nil () )))) )
    assertEquals(Cons("Geografia" , Cons ( "Storia", Nil () )), coursesTaught(ppl2))

  }

}
